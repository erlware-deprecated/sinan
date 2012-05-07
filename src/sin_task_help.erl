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

-include_lib("sinan/include/sinan.hrl").

%% API
-export([description/0, do_task/2]).

-define(TASK, help).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================
%% @doc return a description of the task for callers
-spec description() -> sin_task:task_description().
description() ->

    Desc = "
help Task
=========

Provides helpful information about the tasks available in Sinan and how to
invoke them. ",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = true,
          deps = ?DEPS,
          desc = Desc,
          example = "help [command] | [<command> len <integer>",
          short_desc = "Provides help information for the available tasks",
          opts = []}.

%% @doc print out help text for everything in the system
-spec do_task(sin_config:config(), sin_state:state()) -> sin_state:state().
do_task(Config, State) ->
    Tasks = sin_task:get_tasks() ,
    Result =
        case Config:match(additional_args) of
            [] ->
                sinan:usage(),
                sin_log:normal(Config, " available commands are as follows~n"),
                sin_log:normal(Config, "~nfor more information run 'sinan help <command>'~n~n"),

                TaskNames =
                    lists:map(fun(Task) ->
                                      sin_log:normal(Config, "  ~-20s: ~s", [Task#task.name,
                                                                   Task#task.short_desc]),
                                      Task#task.name
                              end,
                              Tasks),

                {command_list, TaskNames};
        [Task] ->
                process_task_entry(Config, Task, Tasks),
                {help_detail, Task}
    end,
    sin_state:store(help_displayed, Result, State).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Prints out the task description.
-spec process_task_entry(sin_config:config(), string(),
                         sin_task:task_description()) -> ok.
process_task_entry(Config, TaskName, Tasks) ->
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
            sin_log:normal(Config, "~p: not found", [AtomName]);
        _ ->
            sin_log:normal(Config, "~nexample: sinan ~s", [ActualTask#task.example]),
            sin_log:normal(Config, ""),
            sin_log:normal(Config, ActualTask#task.desc)
    end.






