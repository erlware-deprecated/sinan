%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Erlware
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
%%% @copyright (C) 2007-2010 Erlware
%%%---------------------------------------------------------------------------
-module(sin_help).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, help/1]).

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
%% @doc
%%  do the task defined in this module.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    help(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the help command.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
help(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK, "Describing tasks ..."),
    case eta_task:get_task_defs() of
        [] ->
            eta_event:task_event(BuildRef, ?TASK, info,
                                 "No tasks to describe.");
        Tasks ->
            Fun = fun(Val) ->
                          process_task_entry(BuildRef, Val)
                  end,
            lists:map(Fun, Tasks)
    end,
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Prints out the task description.
%%
%% @spec (BuildRef, {Key, Value}) -> ok
%% @end
%%--------------------------------------------------------------------
process_task_entry(BuildRef, #task{name=Key, desc=Desc, deps=Deps}) ->
    eta_event:task_event(BuildRef, ?TASK, info,
                        {"~s~n   ~s~n depends on: ~s~n~n",
                         [Key, Desc,
                          print_dependencies(Deps, "")]}).


%%--------------------------------------------------------------------
%% @doc
%%  Print the dependency list.
%% @spec (DepList, Acc) -> ok
%% @end
%%--------------------------------------------------------------------
print_dependencies([H | T], "") ->
    print_dependencies(T, io_lib:format("~s", [H]));
print_dependencies([H | T], Acc) ->
    print_dependencies(T, io_lib:format("~s, ~s", [H, Acc]));
print_dependencies([], Acc) ->
    Acc.
