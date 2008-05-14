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
%%%  Checks that dependencies are acceptable. If dependencies have been
%%%  generated. If they have not it asks the user if he wants to run them.
%%%  If the answer is affermative then he runs depends. If not he errors out.
%%% @end
%%% @copyright (C) 2007, Erlware
%%%---------------------------------------------------------------------------
-module(sin_depends_check).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, check_depends/1]).

-define(TASK, check_depends).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Checks that dependencies have been run.  "
        "If dependencies have not been run attempts to "
        "run depends task",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = []},
    eta_task:register_task(TaskDesc).


%%--------------------------------------------------------------------
%% @doc
%%  dO the task defined in this module.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    check_depends(BuildRef).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Run the check_depends task.
%%
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
check_depends(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK),
    ProjectApps = gather_project_apps(BuildRef),
    sin_build_config:store(BuildRef, "project.apps", ProjectApps),
    case needs_verify(BuildRef) of
        true ->
            interactive_check(BuildRef);
        false ->
            load_deps(BuildRef),
            ok
    end,
    eta_event:task_stop(BuildRef, ?TASK).


%%--------------------------------------------------------------------
%% @doc
%%  Do an interactive check to ask if dependency information should be checked.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
interactive_check(BuildRef) ->
    case sin_build_config:get_value(BuildRef, "task.depends.no_remote") of
        "True" ->
            load_deps(BuildRef);
        "true" ->
            load_deps(BuildRef);
        "t" ->
            load_deps(BuildRef);
        "T" ->
            load_deps(BuildRef);
        _ ->
            sin_depends:depends(BuildRef)
    end.


%%--------------------------------------------------------------------
%% @doc
%%  Load dependency information from the file system and store it
%%  where needed.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
load_deps(BuildRef) ->
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    Depsf = filename:join([BuildDir, "info", "deps"]),
    case file:consult(Depsf) of
        {ok, [AllDeps]} ->
            sin_build_config:store(BuildRef, "project.deps", AllDeps);
        {ok, _Else} ->
            eta_event:task_fault(BuildRef, {"Dependency information exists but seems to "
                                            "be in a strange format (~s)", [Depsf]}),
            throw(unable_to_read_deps_info);
        {error, _} ->
            eta_event:task_fault(BuildRef,
                                 {"Unable to read stored dependency information "
                                  "from file ~s", [Depsf]}),
            throw(unable_to_read_deps_info)
    end,
    load_repo_apps(BuildRef, BuildDir).

%%--------------------------------------------------------------------
%% @doc
%%  Load the repo apps info from the file system.
%% @spec (BuildRef, BuildDir) -> ok
%% @end
%%--------------------------------------------------------------------
load_repo_apps(BuildRef, BuildDir) ->
    Repsf = filename:join([BuildDir, "info", "repoapps"]),
    case file:consult(Repsf) of
        {ok, [Reps]} ->
            sin_build_config:store(BuildRef, "project.repoapps", Reps);
        {ok, _Else} ->
            eta_event:task_fault(BuildRef, ?TASK, {"The repo application list exists but seems to "
                                                  "be in a strange format (~s)", [Repsf]}),
            throw(unable_to_read_deps_info);
        {error, _} ->
            ewl_event:task_fault(BuildRef, ?TASK, {"Unable to read stored repo application information "
                                                   "from file ~s", [Repsf]}),
            throw(unable_to_read_deps_info)
    end.



%%--------------------------------------------------------------------
%% @doc
%%  Check to see if we need to do a dependency verification.
%% @spec (BuildRef) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
needs_verify(BuildRef) ->
    BuildConfig = sin_build_config:get_value(BuildRef, "build.config"),
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    case sin_sig:changed("dep", BuildDir, BuildConfig) of
        true ->
            sin_sig:update("dep", BuildDir, BuildConfig),
            true;
        false ->
            verify_app_list(BuildRef, BuildDir,
                            sin_build_config:get_value(BuildRef, "project.applist"))
    end.

%%--------------------------------------------------------------------
%% @doc
%%   Verify that the *.app files are unchanged.
%% @spec (BuildRef, BuildDir, AppList) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
verify_app_list(BuildRef, BuildDir, [H | T]) ->
    App = sin_build_config:get_value(BuildRef, "apps." ++ H ++ ".dotapp"),
    case sin_sig:changed("dep", BuildDir, App) of
        true ->
            true;
        false ->
            verify_app_list(BuildRef, BuildDir, T)
    end;
verify_app_list(_BuildRef, _BuildDir, []) ->
    false.

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

