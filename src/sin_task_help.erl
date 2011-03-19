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

    Desc = "Provides helpful information about the tasks available and how to"
	"invoke them",

    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = true,
	  deps = ?DEPS,
	  desc = Desc,
	  example = "help [command]",
	  short_desc = "Provides help information for the available tasks",
	  opts = []}.

%% @doc print out help text for everything in the system
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    Tasks = sin_task:get_tasks() ,
    case sin_config:get_value(BuildRef, "command_line.arg", undefined) of
	undefined ->
	    ewl_talk:say("~nsinan [options] <command>"),
	    ewl_talk:say(" available commands are as follows~n~n"),
	    lists:map(fun(Task) ->
			      ewl_talk:say("  ~p: ~s", [Task#task.name,
						       Task#task.short_desc])
		      end,
		      Tasks),
	    ewl_talk:say("~nfor more information run 'sinan help <command>'");

	Task ->
	    process_task_entry(Task, Tasks)
    end,
    BuildRef.

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Prints out the task description.
-spec process_task_entry(string(), sin_task:task_description()) -> ok.
process_task_entry(TaskName, Tasks) ->
    AtomName = list_to_atom(TaskName),
    ActualTask = lists:foldl(fun(Task, Acc) ->
				     case Task#task.name == AtomName of
					 true ->
					     Task;
					 false ->
					     Acc
				     end
			     end,
			     undefined,
			     Tasks),
    case ActualTask of
	undefined ->
	    ewl_talk:say("~p: not found", [AtomName]);
	_ ->
	    ewl_talk:say("~nexample: ~n"),
	    ewl_talk:say("  sinan ~s", [ActualTask#task.example]),
	    ewl_talk:say(""),
	    ewl_talk:say("  ~s~n", [ActualTask#task.desc])
    end.







