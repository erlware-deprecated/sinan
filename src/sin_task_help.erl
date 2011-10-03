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

    Desc = "Provides helpful information about the tasks available and how to "
        "invoke them. <break> <break> he additional args for <command> len
        <integer> allows the user to control the line length of the help
        printouts",

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
                ec_talk:say(" available commands are as follows~n~n"),
                TaskNames =
                    lists:map(fun(Task) ->
                                      ec_talk:say("  ~-20s: ~s", [Task#task.name,
                                                                   Task#task.short_desc]),
                                      Task#task.name
                              end,
                              Tasks),
                ec_talk:say("~nfor more information run 'sinan help <command>'"),
                {command_list, TaskNames};

        [Task, "len", LineLen] ->
                process_task_entry(Task, Tasks, erlang:list_to_integer(LineLen)),
                {help_detail, Task};
        [Task] ->
                process_task_entry(Task, Tasks, 80),
                {help_detail, Task}
    end,
    sin_state:store(help_displayed, Result, State).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Prints out the task description.
-spec process_task_entry(string(), sin_task:task_description(), integer()) -> ok.
process_task_entry(TaskName, Tasks, LineLen) ->
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
            ec_talk:say("~p: not found", [AtomName]);
        _ ->
            ec_talk:say("~nexample: sinan ~s", [ActualTask#task.example]),
            ec_talk:say(""),
            break(ActualTask#task.desc, LineLen)
    end.


break(Line, LineLen) ->
    Tokens = string:tokens(lists:flatten(Line), [$\s, $\n, $\t, $\f, $\r]),
    lists:foldl(fun("<break>", _Count) ->
                        io:format("~n"),
                        0;
                   (Word, Count) when Count < LineLen ->
                        io:format("~s ", [Word]),
                        Count + erlang:length(Word);
                   (Word, _) ->
                        io:format("~s~n", [Word]),
                        erlang:length(Word)
                end, 0, Tokens),
    io:format("~n").







