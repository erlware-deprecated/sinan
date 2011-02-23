%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Supervisor for all of the sinan build tasks.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
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

%% @private
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @private
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
