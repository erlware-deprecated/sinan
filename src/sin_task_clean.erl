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
-module(sin_task_clean).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1, clean/1]).

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
description() ->
    Desc = "Removes the build area and everything underneath",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = false,
	  deps = ?DEPS,
	  desc = Desc,
	  opts = []}.

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
    ewl_talk:say("cleaning build artifacts"),
    BuildDir = sin_config:get_value(BuildRef, "build.root"),
    ewl_talk:say("Removing directories and contents in ~s", [BuildDir]),
    sin_utils:delete_dir(BuildDir).

%%====================================================================
%%% Internal functions
%%====================================================================
