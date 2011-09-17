%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2006, 2011 Erlware
%%% @doc
%%%  An abstract storage and management piece for build related state
%%%  pairs.
%%% @end
%%%----------------------------------------------------------------------------
-module(sin_state).

%% API
-export([new/0,
         store/2,
         store/3,
         get_value/2,
         get_value/3,
         get_pairs/1,
         delete/2,
         add_run_error/3,
         get_run_errors/1,
         add_run_warning/3,
         get_run_warnings/1,
         format_exception/1]).

-export_type([key/0,
              value/0,
              state/0]).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%%====================================================================
%% Types
%%====================================================================

-type key() :: term().
-type value() :: term().
-opaque state() :: any().

%%====================================================================
%% API
%%====================================================================

%% @doc Create a new empty config
-spec new() -> state().
new() ->
    dict:new().

%% @doc Add a key to the state.
-spec store(key(), value(), state()) -> state().
store(Key, Value, State) ->
    dict:store(Key, Value, State).

%% @doc Store a list of key value pairs into the state
-spec store(KeyValuePairs::[{string(), term()}], state()) ->
    state().
store(KeyValuePairs, State) when is_list(KeyValuePairs) ->
    lists:foldl(fun ({Key, Value}, Dict) ->
                        dict:store(Key, Value, Dict)
                end, State, KeyValuePairs).

%% @doc Get a value from the state.
-spec get_value(key(), state()) -> value() | undefined.
get_value(Key, State) ->
    case dict:find(Key, State) of
        error ->
            undefined;
        {ok, Value} when is_binary(Value) ->
            binary_to_list(Value);
        {ok, Value} ->
            Value
    end.

%% @doc Attempts to get the specified key. If the key doesn't exist it
%% returns the requested default instead of just undefined.
-spec get_value(key(), value(), state()) -> value().
get_value(Key, DefaultValue, State) ->
    case get_value(Key, State) of
        undefined ->
            DefaultValue;
        Value ->
            Value
    end.

%% @doc Delete a value from the state.
-spec delete(key(), state()) -> state().
delete(Key, State) ->
    dict:erase(Key, State).

%% @doc Get the complete state as key,value pairs
-spec get_pairs(state()) -> [{key(), value()}].
get_pairs(State) ->
              dict:to_list(State).

%% @doc Add a run time error occurance to the state
-spec add_run_error(atom(), term(), state()) -> state().
add_run_error(Task, Error, State) ->
    CurrentErrors = get_value(run_errors, [], State),
    store(run_errors, [{Task, Error} | CurrentErrors], State).

%% @doc return the list of run errors in the state
-spec get_run_errors(state()) -> [term()].
get_run_errors(State) ->
    get_value(run_errors, [], State).

%% @doc Add a run time warning occurance to the state
-spec add_run_warning(atom(), term(), state()) -> state().
add_run_warning(Task, Warning, State) ->
    CurrentWarnings = get_value(run_warnings, [], State),
    store(run_warnings, [{Task, Warning} | CurrentWarnings], State).

%% @doc return the list of run warnings in the state
-spec get_run_warnings(state()) -> [term()].
get_run_warnings(State) ->
    get_value(run_warnings, [], State).

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal Functions
%%====================================================================
