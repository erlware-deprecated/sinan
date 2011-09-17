%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Checks the dependencies in the system. Pulls down latest dependencies if
%%% required.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_depends).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0,
         do_task/2,
         format_exception/1]).

-define(TASK, depends).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================

%% @doc provide a description of the system for the caller
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Analyzes all of the dependencies in the project and provides that"
        "\n information to other tasks.",
    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          example = "depends",
          short_desc = "dependency resolution for the project",
          deps = ?DEPS,
          desc = Desc,
          opts = []}.

%% @doc gather all the dependencies for the system
-spec do_task(sin_config:config(), sin_state:state()) ->
                     sin_state:state().
do_task(Config, State0) ->
    BuildDir = sin_state:get_value(build_dir, State0),
    AppBDir = filename:join([BuildDir, "apps"]),
    AllProjectApps = gather_project_apps(State0, AppBDir),

    ProjectApps =
        try
            Release = Config:match('-r'),
            RelName = erlang:list_to_atom(Release),
            {RelName, _, RelApps} =
                lists:keyfind(RelName, 1,
                              Config:match(releases)),

            gather_project_apps(State0, AppBDir, RelApps)
        catch
            not_found ->
                AllProjectApps
        end,

    State1 =
        sin_state:store(
          project_apps, ProjectApps,
          sin_state:store(project_allapps, AllProjectApps, State0)),

    ProjectName = Config:match(project_name),
    ProjectVsn = Config:match(project_vsn),
    RootDir = sin_state:get_value(project_dir, State1),
    LibDir = code:lib_dir(),

    AllDeps =
        case process_release(State1, RootDir,
                             ProjectName, ProjectVsn, ProjectApps) of
            {ok, AD1} ->
                AD1;
            _ ->
                case do_transitive_resolution(State1,
                                              ProjectApps,
                                              AllProjectApps, LibDir) of
                    {ok, AD1} ->
                        AD1;
                    _ ->

                        ?SIN_RAISE(State1, unable_to_resolve,
                                   "Unable to resolve dependencies", []),
                        none
                end
        end,
    RepoApps = get_repo_apps(AllProjectApps, element(1, AllDeps)),

    AllDeps2 =
        case process_release(State1, RootDir,
                             ProjectName, ProjectVsn, AllProjectApps) of
            {ok, AD2} ->
                AD2;
            _ ->
                case do_transitive_resolution(State1,
                                              AllProjectApps, AllProjectApps,
                                              LibDir) of
                    {ok, AD2} ->
                        AD2;
                    _ ->
                        ?SIN_RAISE(State1, unable_to_resolve,
                                   "Unable to resolve dependencies", []),
                        none
                end
        end,

    State2 = sin_state:store(project_deps, AllDeps,
                              sin_state:store(project_alldeps, AllDeps2,
                                               sin_state:store(project_repoapps,
                                                                RepoApps,
                                                                State1))),
    State3 =
        sin_state:store(project_compile_deps,
                        gather_compile_time_dependencies(State2,
                                                         [eunit, proper] ++
                                                             Config:match(compile_deps, []),
                                                         LibDir,
                                                         []), State2),
    update_sigs(State3).


%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================
gather_compile_time_dependencies(State,
                                 [Dep | Rest], LibDir, PackageLocations) ->
    Version = case sin_resolver:package_versions(State,
                                                 LibDir,
                                                 Dep) of
        [] ->
            ?SIN_RAISE(State, unable_to_find_dependency,
                       "Couldn't find dependency ~s.",
                       [Dep]);
        [Version1 | _] ->
            Version1
    end,

    Location = sin_resolver:find_package_location(LibDir,
                                                  Dep,
                                                  Version),
    gather_compile_time_dependencies(State, Rest, LibDir,
                                     [{Dep, Version, {[], []}, Location} |
                                      PackageLocations]);
gather_compile_time_dependencies(_, _, _, PackageLocations) ->
    PackageLocations.

%% @doc Check for per project dependencies
-spec check_project_dependencies(sin_state:state(),
                                 string(), string(), [AppInfo::tuple()],
                                 [AppInfo::tuple()],
                                 term()) -> term().
check_project_dependencies(State, LibDir,
                           ErtsVersion,
                           [App = {_Name, _Vsn, {Deps, IncDeps}, _} |
                            ProjectApps],
                           AllProjectApps,
                           {Acc1, IncAcc1}) ->
    Acc2 = resolve_project_dependencies(State, LibDir,
                                        ErtsVersion,
                                        Deps,
                                        AllProjectApps,
                                        merge_deps(App, Acc1, Acc1)),

    Acc3 = case IncDeps of
               [] ->
                   [];
               undefined ->
                   [];
               _ ->
                   resolve_project_dependencies(State,
                                                LibDir, ErtsVersion, IncDeps,
                                                AllProjectApps,
                                                merge_deps(App, IncAcc1,
                                                           IncAcc1))
           end,

    check_project_dependencies(State, LibDir,
                               ErtsVersion,
                               ProjectApps,
                               AllProjectApps,
                               {Acc2, Acc3});
check_project_dependencies(_, _,
                           _,
                           [],
                           _,
                           Acc) ->
    Acc.

merge_deps(App, [App | _], All) ->
    All;
merge_deps(App,  [_ | Rest], All) ->
    merge_deps(App, Rest, All);
merge_deps(App, [], All) ->
    [App | All].

resolve_project_dependencies(State, LibDir,
                             ErtsVersion,
                             Deps0 = [Dep | Deps],
                             AllProjectApps, Acc) ->
    case already_resolved(Dep, Acc) of
        false ->
            resolve_project_dependencies2(State,
                                          LibDir, ErtsVersion, Deps0,
                                          AllProjectApps,
                                          Acc);
        true ->
            resolve_project_dependencies(State, LibDir, ErtsVersion, Deps,
                                         AllProjectApps,
                                         Acc)
    end;
resolve_project_dependencies(_, _, _, [], _, Acc) ->
    Acc.

resolve_project_dependencies2(State, LibDir,
                              ErtsVersion,
                              [Dep | Deps], AllProjectApps, Acc) ->
    case lists:keysearch(Dep, 1, AllProjectApps) of
        {value, App={Dep, _Version, NDeps, _Location}} ->
            resolve_project_dependencies(State, LibDir, ErtsVersion, Deps ++
                                         element(1,NDeps),
                                         AllProjectApps,
                                         [App | Acc]);
        false ->
            Version =
                case sin_resolver:package_versions(State, LibDir,
                                                   Dep) of
                    [] ->
                        ?SIN_RAISE(State, unable_to_find_dependency,
                                   "Couldn't find dependency ~s.",
                                   [Dep]);
                    [Version1 | _] ->
                        Version1
                end,
            NewEntry = {_, _, {NewDeps, _NewIncDeps}, _} =
                resolve_package_information(State, LibDir, Dep, Version),
            resolve_project_dependencies(State,
                                         LibDir, ErtsVersion, Deps ++ NewDeps,
                                         AllProjectApps,
                                         [NewEntry | Acc])
    end;
resolve_project_dependencies2(_, _, _, [], _, Acc) ->
    Acc.

resolve_package_information(State, LibDir, Name, Version) ->
    {Deps, IncDeps} = sin_resolver:package_dependencies(State,
                                                        LibDir,
                                                        Name,
                                                        Version),

    Location = sin_resolver:find_package_location(LibDir,
                                                  Name,
                                                  Version),
    {Name, Version, {Deps, IncDeps}, Location}.

already_resolved(Dep, [{Dep, _, _, _} | _]) ->
    true;
already_resolved(Dep, [_ | Rest]) ->
    already_resolved(Dep,  Rest);
already_resolved(_, []) ->
    false.

%% @doc Get list of dependencies excluding project apps.
-spec get_repo_apps(list(), list()) -> list().
get_repo_apps(ProjectApps, AllDeps) ->
    get_repo_apps(ProjectApps, AllDeps, []).

-spec get_repo_apps(list(), list(), list()) -> list().
get_repo_apps(ProjectApps, [Dep | Deps], Acc) ->
    case is_project_app(Dep, ProjectApps) of
        true ->
            get_repo_apps(ProjectApps, Deps, Acc);
        false ->
            get_repo_apps(ProjectApps, Deps, [Dep | Acc])
    end;
get_repo_apps(_ProjectApps, [], Acc) ->
    Acc.

%% @doc check to see if it is a project app or a repo app
-spec is_project_app(AppInfo::tuple(), list()) -> boolean().
is_project_app({Name, _Deps, _Version, _Location}, ProjectApps) ->
    lists:keymember(Name, 1, ProjectApps).

%% @doc Roll through the list of project apps and gather the app name and
%% version number.
-spec gather_project_apps(sin_state:state(), string()) -> list().
gather_project_apps(State, AppBDir) ->
    gather_project_apps(State,
                        AppBDir,
                        sin_state:get_value(project_applist, State), [],
                        sin_state:get_value(project_applist, State)).

gather_project_apps(State, AppBDir, AppList) ->
    gather_project_apps(State,
                        AppBDir,
                        AppList, [],
                        sin_state:get_value(project_applist, State)).

gather_project_apps(State, AppBDir, [AppName | T], Acc, ProjectApps) ->
    Vsn = sin_state:get_value({apps, AppName, vsn}, State),
    OpenDeps = sin_state:get_value({apps, AppName, applications}, State),
    IncludedDeps = sin_state:get_value({apps, AppName, included_applications},
                                       [], State),

    NDeps = {OpenDeps, IncludedDeps},

    BuildTarget = lists:flatten([atom_to_list(AppName), "-", Vsn]),
    AppPath = filename:join([AppBDir, BuildTarget]),

    AddToT = lists:foldl(fun(El, LAcc) ->
                               case add_to_project_app_list(El, Acc,
                                                            ProjectApps) of
                                   true ->
                                       [atom_to_list(El) | LAcc];
                                   false ->
                                       LAcc
                               end
                         end,
                         [],
                         OpenDeps ++ IncludedDeps),

    gather_project_apps(State, AppBDir, AddToT ++ T,
                        [{AppName, Vsn, NDeps, AppPath} | Acc], ProjectApps);
gather_project_apps(_, _, [], Acc, _) ->
    Acc.

%% @doc Check to see if this is a project app, but don't include it if its
%% already been processed
-spec add_to_project_app_list(string(), list(), list()) -> boolean().
add_to_project_app_list(AppName, ProccessedApps, ProjectApps) ->
    case lists:keymember(AppName, 1, ProccessedApps) of
        true ->
            false;
        false ->
            lists:member(AppName, ProjectApps)
    end.

%% @doc Update the sigs for all of the 'verifiable' apps.
-spec update_sigs(sin_state:state()) -> ok.
update_sigs(State) ->
    BuildDir = sin_state:get_value(build_dir, State),
    update_app_sigs(State, BuildDir,
                    sin_state:get_value(project_applist, State)).

%% @doc Update the signatures for each of the *.app files in the AppList.
-spec update_app_sigs(sin_state:state(), string(), [atom()]) -> ok.
update_app_sigs(State0, BuildDir, [AppName | T]) ->
    App = sin_state:get_value({apps, AppName, dotapp}, State0),
    State1 = sin_sig:update("dep", App, State0),
    update_app_sigs(State1, BuildDir, T);
update_app_sigs(State, _BuildDir, []) ->
    State.

process_release(State, RootDir, ProjectName, ProjectVsn, ProjectApps) ->
    case sin_release:get_release(State, RootDir, ProjectName, ProjectVsn) of
        no_file ->
            no_release_file;
        RelFile ->
            LibDir = sin_utils:get_application_env(State, prefix),
            Deps = sin_release:get_deps(RelFile),
            {ok, process_deps(State, LibDir, Deps, ProjectApps, [])}
    end.

process_deps(State, LibDir, [{Name, Vsn} | Rest], ProjectApps, Acc) ->
    case lists:keymember(Name, 1, ProjectApps) of
        true ->
            process_deps(State, LibDir, Rest, ProjectApps, Acc);
        false ->
            process_deps(State, LibDir, Rest,
                         ProjectApps,
                         [resolve_package_information(State, LibDir, Name, Vsn) | Acc])
    end;
process_deps(_, _, [], ProjectApps, Acc) ->
    ProjectApps ++ Acc.

do_transitive_resolution(State, ProjectApps, AllProjectApps, LibDir) ->
    ErtsVersion = erlang:system_info(version),
    AllDeps = check_project_dependencies(State,
                                         LibDir,
                                         ErtsVersion,
                                         ProjectApps,
                                         AllProjectApps,
                                         {[], []}),
    {ok, AllDeps}.
