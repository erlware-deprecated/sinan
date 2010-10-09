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
-module(sin_task_version).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1, version/1]).

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
description() ->
    Desc = "Provides sinan server version information",
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
    version(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the version command.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
version(BuildRef) ->
    Version = case get_version() of
		  unknown_version ->
		      "unknown";
		  SinVersion ->
		      SinVersion
	      end,
    sin_talk:say("sinan version: ~s", [Version]),
    sin_build_config:store(BuildRef, "sinan.vsn", Version).



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
    SinDir = filename:join([filename:dirname(code:priv_dir(sinan)), "ebin", "sinan.app"]),
    get_version(file:consult(SinDir)).

get_version({ok, [{application, sinan, Opts}]}) ->
    case lists:keysearch(vsn, 1, Opts) of
	{value, {vsn, Vsn}} ->
	    Vsn;
	_ ->
	    unknown_version
    end;
get_version(_) ->
    unknown_version.

