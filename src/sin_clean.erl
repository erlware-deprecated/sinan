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
%%%   Deletes everything in the Build directory
%%% @end
%%% @copyright (C) 2006-2007 Erlware
%%% Created : 11 Oct 2006 by Eric Merritt <ericbmerritt@gmail.com>
%%%---------------------------------------------------------------------------
-module(sin_clean).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, clean/1]).

-define(TASK, clean).
-define(DEPS, []).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Removes the build area and everything underneath",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = []},
    eta_task:register_task(TaskDesc).

%%--------------------------------------------------------------------
%% @doc
%%  dO the task defined in this module.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    clean(BuildRef).

%%--------------------------------------------------------------------
%% @doc
%%   Run the clean task.
%%
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
clean(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK, "cleaning build artifacts"),
    BuildDir = sin_build_config:get_value(BuildRef, "build.root"),
    eta_event:task_event(BuildRef, ?TASK, info, {"Removing directories and contents in ~s", [BuildDir]}),
    sin_utils:delete_dir(BuildDir),
    eta_event:task_stop(BuildRef, ?TASK).

%%====================================================================
%%% Internal functions
%%====================================================================
