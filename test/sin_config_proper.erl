%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Eric Merritt
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sin_config_proper).

-export([]).

-include_lib("proper/include/proper.hrl").

%%==============================================================================
%% Properties
%%==============================================================================
prop_size_increases_with_new_entry() ->
    ?FORALL({Config, K, V}, {config(), full_key(), value()},
            begin
                Size = sin_config:size(Config),
                case sin_config:has_key(K, Config) of
                    true ->
                        Size == sin_config:size(sin_config:'__add__'(K, V, Config));
                    false ->
                        (Size + 1) == sin_config:size(
                                        sin_config:'__add__'(K, V, Config))
                end
            end).

prop_size_decrease_when_removing() ->
    ?FORALL({Config, K}, {config(), full_key()},
            begin
                Size = sin_config:size(Config),
                case sin_config:has_key(K, Config) of
                    false ->
                        Size == sin_config:size(sin_config:remove(K, Config));
                    true ->
                        (Size - 1) == sin_config:size(
                                        sin_config:remove(K, Config))
                end
            end).


prop_get_after_add_returns_correct_value() ->
    ?FORALL({Config, K, V}, {config(), full_key(), value()},
            begin
                try sin_config:get(K, sin_config:'__add__'(K, V, Config)) of
                    V ->
                        true;
                    _ ->
                        false
                catch
                    _:_ ->
                        false
                end
            end).

prop_key_is_present_after_add() ->
    ?FORALL({Config, K, V}, {config(), full_key(), value()},
            begin
                sin_config:has_key(K, sin_config:'__add__'(K, V, Config))
            end).

prop_inexact_match_positive() ->
    ?FORALL({Config, Key = {K, S1, S2, S3, S4, S5}, V},
            {config(),
             {key(), specifier(), specifier(),
              specifier(), specifier(), specifier()},
             value()},
            begin
                MatchKey = {K, resolve_s(S1),
                            resolve_s(S2), resolve_s(S3),
                            resolve_s(S4), resolve_s(S5)},
                Config2 = sin_config:'__add__'(Key, V, Config),
                V == sin_config:'__match__'(MatchKey, Config2)
            end).

prop_inexact_match_negative() ->
    ?FORALL({Config, Key = {K, _, _, _, _, _}, V},
            {config(),
             {key(), direct(), specifier(), specifier(),
              specifier(), specifier()},
             value()},
            begin
                MatchKey = {K, direct(),
                            direct(), direct(),
                            direct(), direct()},
                Config2 = sin_config:'__add__'(Key, V, Config),
                try
                    sin_config:'__match__'(MatchKey, Config2),
                    false
                catch
                    throw:not_found ->
                        true
                end
            end).

prop_new_from_terms() ->
   ?FORALL({Entries, InitialOpts,
            Key = {K, S1, S2, S3, S4, S5}, Value},
           {partial_entries(), spec_opts(), full_key(),
            value()},
           begin
               Config = sin_config:new_from_terms(Entries, InitialOpts),
               Config1 = sin_config:'__add__'(Key, Value, Config),

               MatchKey = {K, resolve_s(S1),
                           resolve_s(S2), resolve_s(S3),
                           resolve_s(S4), resolve_s(S5)},
               Value == sin_config:'__match__'(MatchKey, Config1)
           end).

prop_test_sort() ->
   ?FORALL({Key1 = {K, S1, S2, _S3, S4, S5}, Value1, Value2, Value3},
           {{key(), direct(), direct(), wildcard(), direct(), wildcard()},
            value(), value(), value()},
           begin
               Key2 = {K, S1, S2, direct(), S4, S5},
               Key3 = {K, S1, S2, direct(), S4, direct()},
               Config = sin_config:new(),
               Config1 = sin_config:'__add__'(Key3, Value3, Config),
               Config2 = sin_config:'__add__'(Key2, Value2, Config1),
               Config3 = sin_config:'__add__'(Key1, Value1, Config2),

               [{TKey3, _}, {TKey2, _}, {TKey1, _}] =
                   sin_config:to_list(Config3),

               (TKey1 == Key1 andalso TKey2 == Key2
                andalso TKey3 == Key3)

           end).

%%==============================================================================
%% Support functions
%%=============================================================================

resolve_s('*') ->
    direct();
resolve_s(V) ->
    V.


%%==============================================================================
%% Generators
%%=============================================================================

wildcard() ->
    exactly('*').

direct() ->
    atom().

specifier() ->
    union([wildcard(), direct()]).

key() ->
    union([atom(), binary(), string()]).

value() ->
    term().

full_key() ->
    tuple([key(), specifier(),
           specifier(), specifier(),
           specifier(), specifier()]).

spec_name() ->
    union([task, release, app, dir, module]).

spec_opts() ->
    ?SIZED(N, spec_opts(N)).

spec_opts(0) ->
    [{spec_name(), atom()}];
spec_opts(N) ->
    ?LET(SO, spec_opts(N - 1),
         [{spec_name(), atom()} | SO]).

partial_entry() ->
    union([{key(), value()},
           {{key(), spec_opts()}, value()},
           {{key(), specifier()}, value()},
           {{key(), specifier(), specifier()}, value()},
           {{key(), specifier(), specifier(), specifier()}, value()},
           {{key(), specifier(), specifier(), specifier(),
             specifier()}, value()},
           {{key(), specifier(), specifier(), specifier(),
             specifier(), specifier()}, value()}]).


partial_entries() ->
    ?SIZED(N, partial_entries(N)).

partial_entries(0) ->
    [partial_entry()];
partial_entries(N) ->
    ?LET(PE, partial_entries(N - 1),
         [partial_entry() | PE]).

config() ->
    ?SIZED(N, config(N)).

config(0) ->
    {'$call', sin_config, new, []};
config(N) ->
    ?LAZY(frequency([
                     {1, config(0)},
                     {3, {'$call', sin_config, remove,
                          [full_key(), config(N - 1)]}},
                     {6, {'$call', sin_config, '__add__',
                          [full_key(), value(), config(N - 1)]}}
                  ])).

