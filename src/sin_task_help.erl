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
-module(sin_task_help).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1, help/1]).

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
description() ->
    Desc = "Provides help information for the available tasks",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  deps = ?DEPS,
	  desc = Desc,
	  opts = []}.


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
%%--------------------------------------------------------------------
%% @doc
%%  Prints out the task description.
%%
%% @spec (BuildRef, {Key, Value}) -> ok
%% @end
%%--------------------------------------------------------------------
process_task_entry(#task{name=Key, desc=Desc}) ->
    ewl_talk:say("~s~n   ~s~n~n",
		  [Key, Desc]).


