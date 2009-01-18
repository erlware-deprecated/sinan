%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Erlware
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
%%% @copyright (C) 2007, Erlware
%%%---------------------------------------------------------------------------
-module(sin_depends).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, depends/1]).

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
start() ->
    Desc = "Analyzes all of the dependencies in the project "
        "and pulls down those that arn't curently available "
        "locally",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = []},
    eta_task:register_task(TaskDesc).


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
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    AppBDir = filename:join([BuildDir, "apps"]),
    ProjectApps = gather_project_apps(BuildRef, AppBDir),
    sin_build_config:store(BuildRef, "project.apps", ProjectApps),
    Prefix = get_application_env(prefix),
    ErtsVersion = get_application_env(erts_version),
    AllDeps = check_project_dependencies(Prefix,
					 ErtsVersion,
					 ProjectApps,
					 []),
    sin_build_config:store(BuildRef, "project.deps", AllDeps),
    sin_build_config:store(BuildRef, "project.repoapps", AllDeps),
    save_deps(BuildRef, AllDeps),
    update_sigs(BuildRef).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Get an enviroment variable, throw error if unavailable.
%% @spec (Key) -> Value
%% @end
%%--------------------------------------------------------------------
get_application_env(Key) ->
    case application:get_env(sinan, Key) of
	{ok, Value} ->
	    Value;
	_ ->
	     ?ETA_RAISE_DA(variables_not_set,
			   "Key ~w not set, must be set as application"
			   " environment variable ", [Key])
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Parse the app description into a format consumable by the
%%  deps engine.
%% @spec preformat_version_data(AppInfo, Acc) -> {Deps, Pkg, Vsns}
%% @end
%%--------------------------------------------------------------------
check_project_dependencies(Prefix,
			   ErtsVersion,
			   [{_, _, Deps, _} | ProjectApps], Acc) ->
    Acc2 = resolve_project_dependencies(Prefix, ErtsVersion, Deps,
				       Acc),
    check_project_dependencies(Prefix,
			       ErtsVersion,
			       ProjectApps,
			       Acc2);
check_project_dependencies(_,
			   _,
			   [],
			   Acc) ->
    Acc.

resolve_project_dependencies(Prefix,
			      ErtsVersion,
			      [Dep | Deps], Acc) ->
     case already_resolved(Dep, Acc) of
	 false ->
	     Version =
		 case sin_resolver:package_versions(Prefix,
						    ErtsVersion,
						    Dep) of
		     [] ->
			 ?ETA_RAISE_DA(unable_to_find_dependency,
				       "Couldn't find dependency ~s.",
				       [Dep]);
		     [Version1 | _] ->
			 Version1
		 end,		 
	     NDeps = sin_resolver:package_dependencies(Prefix,
						       ErtsVersion,
						       Dep,
						       Version),
	     Location = sin_resolver:find_package_location(Prefix,
							   ErtsVersion,
							   Dep,
							   Version),
	     resolve_project_dependencies(Prefix, ErtsVersion, Deps ++ NDeps,
					  [{Dep, Version, NDeps, Location} |
					   Acc]);
	 true ->
	     resolve_project_dependencies(Prefix, ErtsVersion, Deps,
					 Acc)
    end;
resolve_project_dependencies(_, _, [], Acc) ->
    Acc.


already_resolved(Dep, [{Dep, _, _, _} | _]) ->
    true;
already_resolved(Dep, [_ | Rest]) ->
    already_resolved(Dep,  Rest);
already_resolved(_, []) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%%  Saves the list of dependencies for later use.
%% @spec (BuildRef, Deps) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
save_deps(BuildRef, Deps) ->
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    filelib:ensure_dir(filename:join([BuildDir, "info", "tmp"])),
    Depsf = filename:join([BuildDir, "info", "deps"]),
    case file:open(Depsf, write) of
        {error, _} ->
            ?ETA_RAISE_DA(unable_to_write_dep_info,
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
    Apps = sin_build_config:get_value(BuildRef, "project.repoapps"),
    Repsf = filename:join([BuildDir, "info", "repoapps"]),
    case file:open(Repsf, write) of
        {error, _} ->
            ?ETA_RAISE_DA(unable_to_write_dep_info,
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
%% @spec (BuildRef) -> ListOfAppVsn
%% @end
%% @private
%%-------------------------------------------------------------------
gather_project_apps(BuildRef, AppBDir) ->
    gather_project_apps(BuildRef,
			AppBDir,
                        sin_build_config:get_value(BuildRef, "project.applist"), []).

gather_project_apps(BuildRef, AppBDir, [AppName | T], Acc) ->
    Vsn = sin_build_config:get_value(BuildRef, "apps." ++ AppName ++ ".vsn"),
    Name = sin_build_config:get_value(BuildRef,
                                      "apps." ++ AppName ++ ".name"),
    OpenDeps = sin_build_config:get_value(BuildRef,
                                       "apps." ++ AppName ++
                                        ".applications"),
    IncludedDeps = sin_build_config:get_value(BuildRef,
                                           "apps." ++ AppName ++
                                            ".included_applications"),

    Eunit = case sin_build_config:get_value(BuildRef, "eunit") of
		"disable" ->
		    [];
		_ ->
		    [eunit]

	    end,
    NDeps = merge(OpenDeps ++ Eunit, IncludedDeps),

    BuildTarget = lists:flatten([atom_to_list(Name), "-", Vsn]),
    AppPath = filename:join([AppBDir, BuildTarget]),

    sin_build_config:store(BuildRef, "apps." ++ AppName ++ ".deps", NDeps),

    gather_project_apps(BuildRef, AppBDir, T,
			[{Name, Vsn, NDeps, AppPath} | Acc]);
gather_project_apps(_, _, [], Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @doc
%%   Merge the two types of deps removing duplicates.
%% @spec (OpenDeps, IncludedDeps) -> MergedList
%% @end
%%--------------------------------------------------------------------
merge(OpenDeps, undefined) ->
    OpenDeps;
merge(OpenDeps, IncludedDeps) ->
    lists:umerge(lists:sort(OpenDeps), lists:sort(IncludedDeps)).

%%--------------------------------------------------------------------
%% @doc
%%  Update the sigs for all of the 'verifiable' apps.
%% @spec (BuildRef) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
update_sigs(BuildRef) ->
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    update_app_sigs(BuildRef, BuildDir,
                    sin_build_config:get_value(BuildRef, "project.applist")).

%%--------------------------------------------------------------------
%% @doc
%%  Update the signatures for each of the *.app files in the AppList.
%% @spec (BuildRef, BuildDir, AppList) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
update_app_sigs(BuildRef, BuildDir, [H | T]) ->
    App = sin_build_config:get_value(BuildRef, "apps." ++ H ++ ".dotapp"),
    sin_sig:update("dep", BuildDir, App),
    update_app_sigs(BuildRef, BuildDir, T);
update_app_sigs(_BuildRef, _BuildDir, []) ->
    ok.
