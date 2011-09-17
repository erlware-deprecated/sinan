%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Starts a shell with the correct code paths.
%%% @end
%%% @copyright (C) 2006-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_shell).

-behaviour(sin_task).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%% API
-export([description/0, do_task/2]).

-define(TASK, shell).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================
%% @doc return a description of this task
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Starts an erlang shell with all of the correct \n"
        "paths preset so the developer can noodle with the \n"
        "code to his hearts content",
    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "shell",
          desc = Desc,
          short_desc = "Provides an erlang repl on the project",
          opts = []}.

%% @doc Run the shell command.
-spec do_task(sin_config:config(), sin_state:state()) -> sin_state:state().
do_task(_Config, State) ->
    ProjectApps = sin_state:get_value(project_apps, State),
    ProjectRepoApps = sin_state:get_value(project_repoapps, State),
    make_shell(ProjectApps, ProjectRepoApps),
    State.

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Go through and actually start the shell.
-spec make_shell(string(), [string()]) -> ok.
make_shell(ProjectApps, ProjectRepoApps) ->
    setup_paths(ProjectApps),
    setup_paths(ProjectRepoApps).

%% @doc gather a list of paths from the app and add them to the code path
-spec setup_paths([AppInfo::term()]) -> ok.
setup_paths([{_AppName, _Vsn, _, AppPath} | T]) ->
    code:add_patha(AppPath),
    setup_paths(T);
setup_paths([]) ->
    ok.


%%====================================================================
%% Tests
%%====================================================================

