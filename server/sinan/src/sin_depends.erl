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
    ProjectApps = gather_project_apps(BuildRef),
    sin_build_config:store(BuildRef, "project.apps", ProjectApps),
    Repos = sin_build_config:get_value(BuildRef, "repositories"),
    case catch ewr_depends:
               check_project_dependencies(Repos,
                                          ProjectApps,
                                          get_supplimental(BuildRef)) of
               {dependency_resolution_error, Reason} ->
                 ?ETA_RAISE_D(dependency_issue, Reason);
               dependency_resolution_error ->
                 ?ETA_RAISE(dependency_issue);
               {error, Reason} ->
                 ?ETA_RAISE_D(dependency_issue, Reason);
               {'EXIT', Reason} ->
                 ?ETA_RAISE_DA(dependency_issue,
                               "'EXIT': ~p~n", [Reason]);
               AllDeps ->
                 sin_build_config:store(BuildRef, "project.deps", AllDeps),
                 sin_repo_fetcher:fetch(?TASK, BuildRef, ProjectApps, AllDeps),
                 save_deps(BuildRef, AllDeps),
                 update_sigs(BuildRef)

         end.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Add the latest version of eunit to the dependency list.
%% @spec (BuildRef) -> SupplimentalInformation
%% @end
%% @private
%%--------------------------------------------------------------------
get_supplimental(BuildRef) ->
    case sin_build_config:get_value(BuildRef, "eunit") of
        "disable" ->
            [];
        _ ->
            [{eunit, []}]
    end.



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
gather_project_apps(BuildRef) ->
    gather_project_apps(BuildRef,
                        sin_build_config:get_value(BuildRef, "project.applist"), []).

gather_project_apps(BuildRef, [AppName | T], Acc) ->
    Vsn = sin_build_config:get_value(BuildRef, "apps." ++ AppName ++ ".vsn"),
    Name = sin_build_config:get_value(BuildRef,
                                      "apps." ++ AppName ++ ".name"),
    OpenDeps = sin_build_config:get_value(BuildRef,
                                       "apps." ++ AppName ++
                                        ".applications"),
    IncludedDeps = sin_build_config:get_value(BuildRef,
                                           "apps." ++ AppName ++
                                            ".included_applications"),

    VersionedDeps = sin_build_config:get_value(BuildRef,
                                          "apps." ++ AppName ++
                                           ".versioned_dependencies"),
    NDeps = {merge(OpenDeps, IncludedDeps), VersionedDeps},

    sin_build_config:store(BuildRef, "apps." ++ AppName ++ ".deps", NDeps),

    gather_project_apps(BuildRef, T, [{Name, Vsn, NDeps} | Acc]);
gather_project_apps(_BuildRef, [], Acc) ->
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
