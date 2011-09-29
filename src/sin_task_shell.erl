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
-include_lib("sinan/include/sinan.hrl").
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

    Desc = "The repl is an extraordinarily important part of any Erlang
    development project. This command helps the developer by starting up an
    erlang shell with all the paths to code utilized within the project already
    set. This does not, however, attempt to start any of that code. That is left
    up to the developer.",

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
    ReleaseDeps = sin_state:get_value(release_deps, State),
    lists:foreach(fun(#app{path=Path}) ->
                          code:add_patha(filename:join(Path, "ebin"))
                  end, ReleaseDeps),

    State.

%%====================================================================
%%% Internal functions
%%====================================================================


