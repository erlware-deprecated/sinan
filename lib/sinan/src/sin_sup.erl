%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Erlware
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
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Supervisor for all of the sinan build tasks.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created : 18 Nov 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_sup).

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
%% @doc
%% Starts the supervisor
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
init([]) ->
    PrintGuard = {print_guard,{eta_event_guard,start_link,
                               [sin_print_handler, []]},
                  permanent,2000,worker,[eta_event_guard]},
    ConfigSup = {sin_config_sup,{sin_config_sup,start_link, []},
                  permanent,2000,supervisor,[sin_config_sup]},
    ConfigReg = {sin_config_registry,{sin_config_registry,start_link, []},
                 permanent,2000,supervisor,[sin_config_registry]},
    GroupLeaderSup = {sin_group_leader_sup, {sin_group_leader_sup,
                                             start_link,
                                             []},
                      permanent,2000,supervisor,[sin_group_leader_sup]},
    {ok,{{one_for_one,0,1}, [PrintGuard, GroupLeaderSup,
                             ConfigSup, ConfigReg]}}.

%%====================================================================
%% Internal functions
%%====================================================================
