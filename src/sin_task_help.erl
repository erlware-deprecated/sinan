%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Describes the extant tasks.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_help).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1]).

-define(TASK, help).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================
%% @doc return a description of the task for callers
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Provides help information for the available tasks",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = true,
	  deps = ?DEPS,
	  desc = Desc,
	  opts = []}.

%% @doc print out help text for everything in the system
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    case sin_task:get_tasks() of
        [] ->
	    ewl_talk:say("No tasks to describe.");
        Tasks ->
            Fun = fun(Val) ->
                          process_task_entry(Val)
                  end,
            lists:map(Fun, Tasks)
    end,
    BuildRef.

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Prints out the task description.
-spec process_task_entry(sin_task:task_description()) -> ok.
process_task_entry(#task{name=Key, desc=Desc}) ->
    ewl_talk:say("~s~n   ~s~n~n",
		  [Key, Desc]).


