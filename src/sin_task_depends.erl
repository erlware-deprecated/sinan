%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Erlware
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Checks the dependencies in the system. Pulls down latest dependencies if
%%% required.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_depends).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0,
	 do_task/1,
	 depends/1]).

-define(TASK, depends).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
description() ->
    Desc = "Analyzes all of the dependencies in the project "
        "and pulls down those that arn't curently available "
        "locally",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = false,
	  deps = ?DEPS,
	  desc = Desc,
	  opts = []}.


%%--------------------------------------------------------------------
%% @doc
%%  Do the task defined in this module.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    depends(BuildRef).

%%--------------------------------------------------------------------
%% @doc
%%  Run the depends task.
%%
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
depends(BuildRef) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    AppBDir = filename:join([BuildDir, "apps"]),
    Release = sin_config:get_value(BuildRef, "-r"),
    AllProjectApps = gather_project_apps(BuildRef, AppBDir),

    case Release of
        undefined ->
            ProjectApps = AllProjectApps;
        _ ->
            ProjectApps =
		gather_project_apps(BuildRef, AppBDir,
				    sin_config:get_value(BuildRef,
							       "releases."++Release++".apps"))
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
%%--------------------------------------------------------------------
%% @doc
%%  Check for per project dependencies
%% @spec (LibDir, ErtsVersion, AppInfo, Acc) ->
%%                              [{Deps, Vsn, NDeps, Location}]
%% @end
%%--------------------------------------------------------------------
% check_project_dependencies(LibDir,
%                            ErtsVersion,
%                            ProjectApps,
%                            AllProjectApps,
%                            Acc) ->
%     check_project_dependencies(LibDir, ErtsVersion, ProjectApps,
%                                ProjectApps, Acc).

check_project_dependencies(LibDir,
                           ErtsVersion,
                           [App = {_Name, _Vsn, {Deps, IncDeps}, _} | ProjectApps],
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
                                                merge_deps(App, IncAcc1, IncAcc1))
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
            resolve_project_dependencies(LibDir, ErtsVersion, Deps ++ element(1,NDeps),
                                         AllProjectApps,
                                         [App | Acc]);
        false ->
            Version =
                case sin_resolver:package_versions(LibDir,
                                                   Dep) of
                    [] ->
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

%%--------------------------------------------------------------------
%% @doc
%%  Get list of dependencies excluding project apps.
%% @spec (ProjectApps, AllDeps) -> RepoApps
%% @end
%%--------------------------------------------------------------------
get_repo_apps(ProjectApps, AllDeps) ->
    get_repo_apps(ProjectApps, AllDeps, []).

get_repo_apps(ProjectApps, [Dep | Deps], Acc) ->
    case is_project_app(Dep, ProjectApps) of
        true ->  get_repo_apps(ProjectApps, Deps, Acc);
        false -> get_repo_apps(ProjectApps, Deps, [Dep | Acc])
    end;
get_repo_apps(_ProjectApps, [], Acc) ->
    Acc.

is_project_app({Name, _Deps, _Version, _Location}, ProjectApps) ->
    lists:keymember(Name, 1, ProjectApps).

%%--------------------------------------------------------------------
%% @doc
%%  Saves the list of dependencies for later use.
%% @spec (BuildRef, Deps) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
save_deps(BuildRef, Deps) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    filelib:ensure_dir(filename:join([BuildDir, "info", "tmp"])),
    Depsf = filename:join([BuildDir, "info", "deps"]),
    case file:open(Depsf, write) of
        {error, _} ->
            ?SIN_RAISE_DA(unable_to_write_dep_info,
                          "Couldn't open ~s for writing. Unable to "
                          "write dependency information",
                          [Depsf]);

        {ok, IoDev} ->
            io:format(IoDev, "~p.", [Deps]),
            file:close(IoDev)
    end,
    save_repo_apps(BuildRef, BuildDir).

%%--------------------------------------------------------------------
%% @doc
%%  Saves the list of repo apps to info.
%% @spec (BuildRef, BuildDir) -> ok
%% @end
%%--------------------------------------------------------------------
save_repo_apps(BuildRef, BuildDir) ->
    Apps = sin_config:get_value(BuildRef, "project.repoapps"),
    Repsf = filename:join([BuildDir, "info", "repoapps"]),
    case file:open(Repsf, write) of
        {error, _} ->
            ?SIN_RAISE_DA(unable_to_write_dep_info,
                          "Couldn't open ~s for writing. Unable to "
                          "write dependency information",
                          [Repsf]);
        {ok, IoDev} ->
            io:format(IoDev, "~p.", [Apps]),
            file:close(IoDev)
    end.

%%-------------------------------------------------------------------
%% @doc
%%   Roll through the list of project apps and gather the app
%%   name and version number.
%% @spec (BuildRef, AppBuildDir) -> ListOfAppVsn
%% @end
%% @private
%%-------------------------------------------------------------------
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
				  sin_config:get_value(BuildRef, "project.applist"))).

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
			       case add_to_project_app_list(El, Acc, ProjectApps) of
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


%%--------------------------------------------------------------------
%% @doc
%%  Check to see if this is a project app, but don't include it if its
%%  already been processed
%% @spec (AppName, ProcessedApps, ProjectApps) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
add_to_project_app_list(AppName, ProccessedApps, ProjectApps) ->
    case lists:keymember(AppName, 1, ProccessedApps) of
	true ->
	    false;
	false ->
	    lists:member(AppName, ProjectApps)
    end.


%%--------------------------------------------------------------------
%% @doc
%%  Update the sigs for all of the 'verifiable' apps.
%% @spec (BuildRef) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
update_sigs(BuildRef) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    update_app_sigs(BuildRef, BuildDir,
                    sin_config:get_value(BuildRef, "project.applist")).

%%--------------------------------------------------------------------
%% @doc
%%  Update the signatures for each of the *.app files in the AppList.
%% @spec (BuildRef, BuildDir, AppList) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
update_app_sigs(BuildRef, BuildDir, [H | T]) ->
    App = sin_config:get_value(BuildRef, "apps." ++ H ++ ".dotapp"),
    sin_sig:update("dep", BuildDir, App),
    update_app_sigs(BuildRef, BuildDir, T);
update_app_sigs(_BuildRef, _BuildDir, []) ->
    ok.

process_release(RootDir, BuildFlavor,
		ProjectName, ProjectVsn, ProjectApps) ->

    case sin_release:get_release(RootDir, BuildFlavor, ProjectName, ProjectVsn) of
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
