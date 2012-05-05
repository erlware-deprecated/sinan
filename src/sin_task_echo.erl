%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   This provides echo functionality to sinan. The ability to echo paths,
%%%   include directories, etc to The caller
%%% @end
%%% @copyright (C) 2012 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_echo).

-behaviour(sin_task).

-include_lib("sinan/include/sinan.hrl").

%% API
-export([description/0, do_task/2]).

-define(TASK, echo).
-define(DEPS, [depends]).

%%====================================================================
%% API
%%====================================================================

%% @doc provides a description of this task
-spec description() ->  sin_task:task_description().
description() ->

    Desc = "This echos information to the command line",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "echo paths",
          desc = Desc,
          short_desc = "Displays the requested information on the command line
          in a format that erlang can understand.",
          opts = []}.

%% @doc run all tests for all modules in the system
do_task(Config, State0) ->
    lists:foldl(fun print_correct/2, State0, Config:match(additional_args, [])).


%%====================================================================
%%% Internal functions
%%====================================================================
print_correct("paths", State) ->
    lists:foreach(fun(#app{path=Path}) ->
                          io:format("-pa ~s ", [filename:join(Path, "ebin")])
                  end, sin_state:get_value(release_apps, State)),
    State;
print_correct("includes", State) ->
    lists:foreach(fun(#app{path=Path}) ->
                          io:format("-I ~s ", [filename:join(Path, "include")])
                  end, sin_state:get_value(release_apps, State)),
    State;
print_correct(Else, State) ->
    AtomRep = erlang:list_to_atom(Else),
    io:format("~s ", [sin_state:get_value(AtomRep, State)]),
    State.



