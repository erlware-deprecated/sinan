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

    Desc = "
shell Task
==========

The repl is an extraordinarily important part of any Erlang
development project. This command helps the developer by starting up an
erlang shell with all the paths to code utilized within the project already
set. This does not, however, attempt to start any of that code. That is left
up to the developer.

Configuration Options
---------------------

    {kernel_start_args, [foo, shortnames]}.
    {cookie, my_secret}.

kernel_start_args are the args that will be passed to net_kernel:start/1
when the shell is started. cookie is the secret cookie to set.

if you specify these values erlang will ensure epmd is started and start the
networking kernel for erlang. Do not use these options if you do not want
networking started.",

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
do_task(Config, State) ->
    case Config:match(kernel_start_args, undefined) of
        undefined ->
            ok;
        NodeName ->
            os:cmd("epmd -daemon"),
            net_kernel:start(NodeName),
            case Config:match(cookie, undefined) of
                undefined ->
                    ok;
                Cookie ->
                    erlang:set_cookie(node(), Cookie)
            end
    end,
    lists:foreach(fun(#app{path=Path}) ->
                          Ebin = filename:join(Path, "ebin"),
                          true = code:add_patha(Ebin)
                  end, sin_state:get_value(release_deps, State)),

    shell:server(false, false),
    State.

%%====================================================================
%%% Internal functions
%%====================================================================
