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
-export([start/0, do_task/2, check_depends/2]).

-define(TASK, check_depends).
-define(DEPS, [discover]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start() -> ok.
%%
%% @doc
%% Starts the server
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
%% @spec do_task(BuildRef, Args) -> ok
%%
%% @doc
%%  dO the task defined in this module.
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef, Args) ->
    check_depends(BuildRef, Args).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Run the check_depends task.
%%
%% @spec check_depends() -> ok.
%% @end
%%--------------------------------------------------------------------
check_depends(BuildRef, Args) ->
    eta_event:task_start(BuildRef, ?TASK),
    ProjectApps = gather_project_apps(BuildRef),
    fconf:store(BuildRef, "project.apps", ProjectApps),
    case needs_verify(BuildRef) of
        true ->
            interactive_check(BuildRef, Args);
        false ->
            load_deps(BuildRef),
            ok
    end,
    eta_event:task_stop(BuildRef, ?TASK).


%%--------------------------------------------------------------------
%% @doc
%%  Do an interactive check to ask if dependency information should be checked.
%% @spec interactive_check(BuildRef, Args) -> ok
%% @end
%%--------------------------------------------------------------------
interactive_check(BuildRef, Args) ->
%    case ewl_talk:ask("Dependencies are out of date. "
%                      "Should I run dependecies now", boolean) of
%        true ->
            sin_depends:depends(BuildRef, Args).
%        false ->
%            load_deps(BuildRef)
%    end.


%%--------------------------------------------------------------------
%% @spec load_deps() -> ok
%%
%% @doc
%%  Load dependency information from the file system and store it
%%  where needed.
%% @end
%%--------------------------------------------------------------------
load_deps(BuildRef) ->
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    Depsf = filename:join([BuildDir, "info", "deps"]),
    case file:consult(Depsf) of
        {ok, [AllDeps]} ->
            fconf:store(BuildRef, "project.deps", AllDeps);
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
%% @spec load_repo_apps(BuildDir) -> ok.
%%
%% @doc
%%  Load the repo apps info from the file system.
%% @end
%%--------------------------------------------------------------------
load_repo_apps(BuildRef, BuildDir) ->
    Repsf = filename:join([BuildDir, "info", "repoapps"]),
    case file:consult(Repsf) of
        {ok, [Reps]} ->
            fconf:store(BuildRef, "project.repoapps", Reps);
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
%% @spec needs_verify() -> true | false
%%
%% @doc
%%  Check to see if we need to do a dependency verification.
%% @end
%% @private
%%--------------------------------------------------------------------
needs_verify(BuildRef) ->
    BuildConfig = fconf:get_value(BuildRef, "build.config"),
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    case sin_sig:changed("dep", BuildDir, BuildConfig) of
        true ->
            sin_sig:update("dep", BuildDir, BuildConfig),
            true;
        false ->
            verify_app_list(BuildRef, BuildDir,
                            fconf:get_value(BuildRef, "project.applist"))
    end.

%%--------------------------------------------------------------------
%% @spec verify_app_list(BuildDir, AppList) -> true | false
%%
%% @doc
%%   Verify that the *.app files are unchanged.
%% @end
%% @private
%%--------------------------------------------------------------------
verify_app_list(BuildRef, BuildDir, [H | T]) ->
    App = fconf:get_value(BuildRef, {path, ["apps", H, "dotapp"]}),
    case sin_sig:changed("dep", BuildDir, App) of
        true ->
            true;
        false ->
            verify_app_list(BuildRef, BuildDir, T)
    end;
verify_app_list(_BuildRef, _BuildDir, []) ->
    false.

%%-------------------------------------------------------------------
%% @spec gather_project_apps() -> ListOfAppVsn
%% @doc
%%   Roll through the list of project apps and gather the app
%%   name and version number.
%% @end
%% @private
%%-------------------------------------------------------------------
gather_project_apps(BuildRef) ->
    gather_project_apps(BuildRef,
                        fconf:get_value(BuildRef, "project.applist"), []).

gather_project_apps(BuildRef, [AppName | T], Acc) ->
    Vsn = fconf:get_value(BuildRef, {path, ["apps", AppName, "vsn"]}),
    Name = fconf:get_value(BuildRef, {path, ["apps", AppName, "name"]}),
    OpenDeps = fconf:get_value(BuildRef, {path,
                                       ["apps", AppName,
                                        "applications"]}),
    IncludedDeps = fconf:get_value(BuildRef, {path,
                                           ["apps", AppName,
                                            "included_applications"]}),

    VersionedDeps = fconf:get_value(BuildRef, {path,
                                          ["apps", AppName,
                                           "versioned_dependencies"]}),
    NDeps = {merge(OpenDeps, IncludedDeps), VersionedDeps},

    fconf:store(BuildRef, {path, ["apps", AppName, "deps"]}, NDeps),

    gather_project_apps(BuildRef, T, [{Name, Vsn, NDeps} | Acc]);
gather_project_apps(_BuildRef, [], Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @doc
%%   Merge the two types of deps removing duplicates.
%% @spec merge(OpenDeps, IncludedDeps) -> MergedList.
%% @end
%%--------------------------------------------------------------------
merge(OpenDeps, undefined) ->
    OpenDeps;
merge(OpenDeps, IncludedDeps) ->
    lists:umerge(lists:sort(OpenDeps), lists:sort(IncludedDeps)).

