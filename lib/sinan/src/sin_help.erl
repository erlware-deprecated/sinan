%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
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
%%%   Describes the extant tasks.
%%% @end
%%% @copyright (C) 2007, Erlware
%%%---------------------------------------------------------------------------
-module(sin_help).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/2, help/2]).

-define(TASK, help).
-define(DEPS, []).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start() -> ok
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Provides help information for the available tasks",
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
    help(BuildRef, Args).


%%--------------------------------------------------------------------
%% @doc
%%  Run the help command.
%% @spec help(BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
help(BuildRef, _) ->
    eta_event:task_start(BuildRef, ?TASK, "Describing tasks ..."),
    case eta_task:get_task_defs() of
        [] ->
            ewl_talk:say("No tasks to describe.");
        Tasks ->
            lists:map(fun process_task_entry/1, Tasks)
    end,
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Prints out the task description.
%%
%% @spec process_task_entry({Key, Value}) -> ok.
%% @end
%%--------------------------------------------------------------------
process_task_entry({Key, #task{desc=Desc, deps=Deps}}) ->
    ewl_talk:say("~s", [Key]),
    ewl_talk:say("   ~s", [Desc]),
    ewl_talk:say(" depends on: ~s", [print_dependencies(Deps, "")]),
    ewl_talk:say("~n").


%%--------------------------------------------------------------------
%% @doc
%%  Print the dependency list.
%% @spec print_dependencies(DepList, Acc) -> ok
%% @end
%%--------------------------------------------------------------------
print_dependencies([H | T], "") ->
    print_dependencies(T, io_lib:format("~s", [H]));
print_dependencies([H | T], Acc) ->
    print_dependencies(T, io_lib:format("~s, ~s", [H, Acc]));
print_dependencies([], Acc) ->
    Acc.
