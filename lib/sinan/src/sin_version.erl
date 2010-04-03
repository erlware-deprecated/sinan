%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2010 Eric Merritt
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
%%%   Return the sinan server version.
%%% @end
%%% @copyright (C) 2008-2010 Erlware
%%%---------------------------------------------------------------------------
-module(sin_version).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, version/1]).

-define(TASK, version).
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
    Desc = "Provides sinan server version information",
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
    version(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the version command.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
version(BuildRef) ->
    case get_version() of
	unknown_version ->
	    eta_event:task_event(BuildRef, ?TASK, info, "unknown");
	Version ->
	    eta_event:task_event(BuildRef, ?TASK, info, Version)
    end,
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Gets the current version of the sinan release.
%% @spec () -> Vsn | unkown_version
%% @end
%%--------------------------------------------------------------------
get_version() ->
    Info = release_handler:which_releases(),
    get_version(Info).

get_version([{"sinan", Vsn, _, _} | _]) ->
    Vsn;
get_version([_ | T]) ->
    get_version(T);
get_version([]) ->
    unknown_version.
