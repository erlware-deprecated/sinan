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
	 do_task/1]).

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
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    AppBDir = filename:join([BuildDir, "apps"]),
    Release = sin_config:get_value(BuildRef, "-r"),
    AllProjectApps = gather_project_apps(BuildRef, AppBDir),

    ProjectApps = case Release of
		      undefined ->
			  AllProjectApps;
		      _ ->
			  gather_project_apps(BuildRef, AppBDir,
					      sin_config:get_value(BuildRef,
								   "releases."
								   ++ Release ++
								   ".apps"))
		  end,

    BuildRef2 =
	sin_config:store(
	  sin_config:store(BuildRef, "project.allapps", AllProjectApps),
	  "project.apps", ProjectApps),

    BuildFlavor = sin_config:get_value(BuildRef2, "build.flavor"),
    ProjectName = sin_config:get_value(BuildRef2, "project.name"),
    ProjectVsn = sin_config:get_value(BuildRef2, "project.vsn"),
    RootDir = sin_config:get_value(BuildRef2, "project.dir"),

    case process_release(RootDir, BuildFlavor,
                         ProjectName, ProjectVsn, ProjectApps) of
        {ok, AllDeps} ->
            ok;
        _ ->
            case do_transitive_resolution(ProjectApps, AllProjectApps) of
                {ok, AllDeps} ->
                    ok;
                _ ->
                    AllDeps = none,
		    sin_error_store:signal_error(),
                    ?SIN_RAISE_DA(unable_to_resolve,
                                  "Unable to resolve dependencies", [])
            end
    end,
    RepoApps = get_repo_apps(AllProjectApps, element(1, AllDeps)),

    case process_release(RootDir, BuildFlavor,
                         ProjectName, ProjectVsn, AllProjectApps) of
        {ok, AllDeps2} ->
            ok;
        _ ->
            case do_transitive_resolution(AllProjectApps, AllProjectApps) of
                {ok, AllDeps2} ->
                    ok;
                _ ->
                    AllDeps2 = none,
		    sin_error_store:signal_error(),
                    ?SIN_RAISE_DA(unable_to_resolve,
                                  "Unable to resolve dependencies", [])
            end
    end,

    BuildRef3 = sin_config:store(BuildRef2, "project.deps", AllDeps),
    BuildRef4 = sin_config:store(BuildRef3, "project.alldeps", AllDeps2),
    BuildRef5 = sin_config:store(BuildRef4, "project.repoapps", RepoApps),
    save_deps(BuildRef5, AllDeps),
    update_sigs(BuildRef5),
    BuildRef5.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Check for per project dependencies
-spec check_project_dependencies(string(), string(), [AppInfo::tuple()],
				 [AppInfo::tuple()],
				 term()) -> term().
check_project_dependencies(LibDir,
                           ErtsVersion,
                           [App = {_Name, _Vsn, {Deps, IncDeps}, _} |
			    ProjectApps],
                           AllProjectApps,
                           {Acc1, IncAcc1}) ->
    Acc2 = resolve_project_dependencies(LibDir,
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
                   resolve_project_dependencies(LibDir, ErtsVersion, IncDeps,
                                                AllProjectApps,
                                                merge_deps(App, IncAcc1,
							   IncAcc1))
           end,

    check_project_dependencies(LibDir,
                               ErtsVersion,
                               ProjectApps,
                               AllProjectApps,
                               {Acc2, Acc3});
check_project_dependencies(_,
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

resolve_project_dependencies(LibDir,
                             ErtsVersion,
                             Deps0 = [Dep | Deps],
                             AllProjectApps, Acc) ->
    case already_resolved(Dep, Acc) of
        false ->
            resolve_project_dependencies2(LibDir, ErtsVersion, Deps0,
                                          AllProjectApps,
                                          Acc);
        true ->
            resolve_project_dependencies(LibDir, ErtsVersion, Deps,
                                         AllProjectApps,
                                         Acc)
    end;
resolve_project_dependencies(_, _, [], _, Acc) ->
    Acc.

resolve_project_dependencies2(LibDir,
                              ErtsVersion,
                              [Dep | Deps], AllProjectApps, Acc) ->
    case lists:keysearch(Dep, 1, AllProjectApps) of
        {value, App={Dep, _Version, NDeps, _Location}} ->
            resolve_project_dependencies(LibDir, ErtsVersion, Deps ++
					 element(1,NDeps),
                                         AllProjectApps,
                                         [App | Acc]);
        false ->
            Version =
                case sin_resolver:package_versions(LibDir,
                                                   Dep) of
                    [] ->
			sin_error_store:signal_error(),
                        ?SIN_RAISE_DA(unable_to_find_dependency,
                                      "Couldn't find dependency ~s.",
                                      [Dep]);
                    [Version1 | _] ->
                        Version1
                end,
            NewEntry = {_, _, {NewDeps, _NewIncDeps}, _} =
                resolve_package_information(LibDir, Dep, Version),
            resolve_project_dependencies(LibDir, ErtsVersion, Deps ++ NewDeps,
                                         AllProjectApps,
                                         [NewEntry | Acc])
    end;
resolve_project_dependencies2(_, _, [], _, Acc) ->
    Acc.

resolve_package_information(LibDir, Name, Version) ->
    {Deps, IncDeps} = sin_resolver:package_dependencies(LibDir,
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

%% @doc Saves the list of dependencies for later use.
-spec save_deps(sin_config:config(), term()) -> ok.
save_deps(BuildRef, Deps) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    filelib:ensure_dir(filename:join([BuildDir, "info", "tmp"])),
    Depsf = filename:join([BuildDir, "info", "deps"]),
    case file:open(Depsf, write) of
        {error, _} ->
	    sin_error_store:signal_error(),
            ?SIN_RAISE_DA(unable_to_write_dep_info,
                          "Couldn't open ~s for writing. Unable to "
                          "write dependency information",
                          [Depsf]);

        {ok, IoDev} ->
            io:format(IoDev, "~p.", [Deps]),
            file:close(IoDev)
    end,
    save_repo_apps(BuildRef, BuildDir).

%% @doc Saves the list of repo apps to info.
-spec save_repo_apps(sin_config:config(), string()) -> ok.
save_repo_apps(BuildRef, BuildDir) ->
    Apps = sin_config:get_value(BuildRef, "project.repoapps"),
    Repsf = filename:join([BuildDir, "info", "repoapps"]),
    case file:open(Repsf, write) of
        {error, _} ->
	    sin_error_store:signal_error(),
            ?SIN_RAISE_DA(unable_to_write_dep_info,
                          "Couldn't open ~s for writing. Unable to "
                          "write dependency information",
                          [Repsf]);
        {ok, IoDev} ->
            io:format(IoDev, "~p.", [Apps]),
            file:close(IoDev)
    end.

%% @doc Roll through the list of project apps and gather the app name and
%% version number.
-spec gather_project_apps(sin_config:config(), string()) -> list().
gather_project_apps(BuildRef, AppBDir) ->
    gather_project_apps(BuildRef,
                        AppBDir,
                        sin_config:get_value(BuildRef, "project.applist"), [],
			sin_config:get_value(BuildRef, "project.applist")).

gather_project_apps(BuildRef, AppBDir, AppList) ->
    gather_project_apps(BuildRef,
                        AppBDir,
                        AppList, [],
			lists:map(fun(El) ->
					  list_to_atom(El)
				  end,
				  sin_config:get_value(BuildRef,
						       "project.applist"))).

gather_project_apps(BuildRef, AppBDir, [AppName | T], Acc, ProjectApps) ->
    Vsn = sin_config:get_value(BuildRef, "apps." ++ AppName ++ ".vsn"),
    Name = sin_config:get_value(BuildRef,
                                      "apps." ++ AppName ++ ".name"),
    OpenDeps = sin_config:get_value(BuildRef,
                                          "apps." ++ AppName ++
                                          ".applications"),
    IncludedDeps = sin_config:get_value(BuildRef,
                                              "apps." ++ AppName ++
                                              ".included_applications", []),

    Eunit = case sin_config:get_value(BuildRef, "eunit") of
                "disable" ->
                    [];
                _ ->
                    [eunit]
            end,

    NDeps = {OpenDeps ++ Eunit, IncludedDeps},

    BuildTarget = lists:flatten([atom_to_list(Name), "-", Vsn]),
    AppPath = filename:join([AppBDir, BuildTarget]),

    sin_config:store(BuildRef, "apps." ++ AppName ++ ".deps", NDeps),

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

    gather_project_apps(BuildRef, AppBDir, AddToT ++ T,
			[{Name, Vsn, NDeps, AppPath} | Acc], ProjectApps);
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
-spec update_sigs(sin_config:config()) -> ok.
update_sigs(BuildRef) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    update_app_sigs(BuildRef, BuildDir,
                    sin_config:get_value(BuildRef, "project.applist")).

%% @doc Update the signatures for each of the *.app files in the AppList.
-spec update_app_sigs(sin_config:config(), string(), list()) -> ok.
update_app_sigs(BuildRef, BuildDir, [H | T]) ->
    App = sin_config:get_value(BuildRef, "apps." ++ H ++ ".dotapp"),
    sin_sig:update("dep", BuildDir, App),
    update_app_sigs(BuildRef, BuildDir, T);
update_app_sigs(_BuildRef, _BuildDir, []) ->
    ok.

process_release(RootDir, BuildFlavor,
		ProjectName, ProjectVsn, ProjectApps) ->

    case sin_release:get_release(RootDir, BuildFlavor,
				 ProjectName, ProjectVsn) of
	no_file ->
	    no_release_file;
	RelFile ->
	    LibDir = sun_utils:get_application_env(prefix),
	    Deps = sin_release:get_deps(RelFile),
	    {ok, process_deps(LibDir, Deps, ProjectApps, [])}
    end.

process_deps(LibDir, [{Name, Vsn} | Rest], ProjectApps, Acc) ->
    case lists:keymember(Name, 1, ProjectApps) of
	true ->
	    process_deps(LibDir, Rest, ProjectApps, Acc);
	false ->
	    process_deps(LibDir, Rest,
			 ProjectApps,
			 [resolve_package_information(LibDir, Name, Vsn) | Acc])
    end;
process_deps(_, [], ProjectApps, Acc) ->
    ProjectApps ++ Acc.

do_transitive_resolution(ProjectApps, AllProjectApps) ->
    LibDir = code:lib_dir(),
    ErtsVersion = erlang:system_info(version),
    AllDeps = check_project_dependencies(LibDir,
                                         ErtsVersion,
                                         ProjectApps,
                                         AllProjectApps,
                                         {[], []}),
    {ok, AllDeps}.
