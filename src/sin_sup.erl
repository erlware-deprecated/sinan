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
    SinErrorStore = {sin_error_store, {sin_error_store, start_link, []},
		    permanent, 2000, worker, [sin_error_store]},

    {ok,{{one_for_one,0,1}, [SinErrorStore]}}.
%%====================================================================
%% Internal functions
%%====================================================================
