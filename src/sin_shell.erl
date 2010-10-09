%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Eric Merritt
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
%%%   Starts a shell with the correct code paths.
%%% @end
%%% @copyright (C) 2006-2010 Erlware
%%%---------------------------------------------------------------------------
-module(sin_shell).

-behaviour(eta_gen_task).

-include("etask.hrl").
-include("eunit.hrl").

%% API
-export([start/0, do_task/1, shell/1]).

-define(TASK, shell).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================
%%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Starts an erlang shell with all of the correct "
        "paths preset so the developer can noodle with the "
        "code to his hearts content",
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
    shell(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the shell command.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
shell(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK, "Returning paths for shell ..."),
    ProjectApps = sin_build_config:get_value(BuildRef, "project.apps"),
    ProjectRepoApps = sin_build_config:get_value(BuildRef, "project.repoapps"),
    Repo = sin_build_config:get_value(BuildRef, "project.repository"),
    make_shell(BuildRef, ProjectApps, ProjectRepoApps, Repo),
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Go through and actually start the shell.
%% @spec (BuildRef, ProjectApps, ProjectRepoApps, Repo) -> ok
%% @end
%%--------------------------------------------------------------------
make_shell(BuildRef, ProjectApps, ProjectRepoApps, Repo) ->
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    AppDir = filename:join([BuildDir, "apps"]),
    send_paths(BuildRef, AppDir, ProjectApps),
    send_paths(BuildRef, Repo, ProjectRepoApps).


%%--------------------------------------------------------------------
%% @doc
%%  Send events for all the paths in the list.
%% @spec (BuildRef, AppDir, DirList) -> Paths
%% @end
%%--------------------------------------------------------------------
send_paths(BuildRef, RepoDir, [{AppName, Vsn, _, _} | T]) ->
    send_path(BuildRef, RepoDir, AppName, Vsn),
    send_paths(BuildRef, RepoDir, T);
send_paths(BuildRef, RepoDir, [{AppName, Vsn, _} | T]) ->
    send_path(BuildRef, RepoDir, AppName, Vsn),
    send_paths(BuildRef, RepoDir, T);
send_paths(BuildRef, RepoDir, [{AppName, Vsn} | T]) ->
    send_path(BuildRef, RepoDir, AppName, Vsn),
    send_paths(BuildRef, RepoDir, T);
send_paths(_, _, []) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Send out the requisite events for a specific piece of the
%%  application.
%% @spec (BuildRef, RepoDir, AppName, Vsn) -> ok
%% @end
%%--------------------------------------------------------------------
send_path(BuildRef, RepoDir, AppName, Vsn) ->
    NAppName = case is_atom(AppName) of
                   true ->
                       atom_to_list(AppName);
                   false ->
                       AppName
               end,
    eta_event:task_event(BuildRef, ?TASK, app_name, NAppName),
    DirName = lists:flatten([NAppName, "-", Vsn]),
    Path = filename:join([RepoDir, DirName, "ebin"]),
    eta_event:task_event(BuildRef, ?TASK, app_path, Path).

%%====================================================================
%% Tests
%%====================================================================

