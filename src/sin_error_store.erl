%%%-------------------------------------------------------------------
%%% @author Eric Merritt <>
%%% @copyright (C) 2011, Eric Merritt
%%% @doc
%%%
%%% @end
%%% Created : 25 Feb 2011 by Eric Merritt <>
%%%-------------------------------------------------------------------
-module(sin_error_store).

-behaviour(gen_server).

%% API
-export([start_link/0,
	 signal_error/0,
	 has_errors/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export_type([]).

-define(SERVER, ?MODULE).

-record(state, {counter}).

%%%===================================================================
%%% Public Types
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc indicate if an error has occured in the system
-spec signal_error() -> ok.
signal_error() ->
    gen_server:cast(?SERVER, signal).

%% @doc indicate if errors have occured by returning the count
-spec has_errors() -> number().
has_errors() ->
    gen_server:call(?SERVER, has_errors).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, #state{counter = 0}}.

%% @private
handle_call(has_errors, _From,
	    State = #state{counter = Counter}) ->
    Reply = Counter,
    {reply, Reply, State}.

%% @private
handle_cast(signal, #state{counter = Counter}) ->
    {noreply, #state{counter = Counter + 1}}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
