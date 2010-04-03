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
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   Central supervisor for the sinan build system. Starts up all tasks
%%%   and allows them to run.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created : 14 Mar 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(eta_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () -> {ok,Pid} | ignore | {error,Error}
%%
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%%
%% @doc
%%  Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
init(_) ->
    MetaTid = ets:new(eta_meta_task, [duplicate_bag, public]),
    TaskTid = ets:new(eta_task, [set, public]),
    Event = {eta_event, {eta_event, start_link, []},
             permanent, 2000, worker, [eta_event]},
    Task = {eta_task, {eta_task, start_link, [TaskTid]},
            permanent, 2000, worker, [eta_task, eta_topo]},
    MetaTask = {eta_meta_task, {eta_meta_task, start_link, [MetaTid]},
            permanent, 2000, worker, [eta_meta_task]},
    Engine = {eta_engine, {eta_engine, start_link, []},
              permanent, 2000, worker, [eta_engine]},
    {ok,{{one_for_one,0,1}, [Event, Task, MetaTask, Engine]}}.

%%====================================================================
%% Internal functions
%%====================================================================
