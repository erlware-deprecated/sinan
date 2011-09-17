%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2006, 2011 Erlware
%%% @doc
%%% Key = any atom
%%% Specifiers [TaskName, ReleaseName, AppName] = any atom
%%% Entry is a Key, Specifier, Value tuple
%%% Values may be term
%%%
%%% Keys may exist multple times as long as the have different specifiers
%%% Specifiers may be wildcarded with the wildcard atom (‘*’)
%%%
%%% The tightest specifier binding wins so if we have a two entries with the
%%% same key, but different specifier values, the closest specfier match
%%% wins. for example
%%%
%%%     {foo, bar, baz, bof, “My Value”}
%%%     {foo, bar, baz, ‘*’, “Other Value”}
%%%     {foo, bar, ‘*’, ‘*’, “Still Another”}
%%%
%%%  and we ask for
%%%
%%%    foo, bar, baz, bof, we should get “My Value”
%%%
%%% if we ask for
%%%    foo, bar, baz, buzzard, we should get “Other Value”
%%%
%%% and if we ask for
%%%
%%%   foo, bar, bid, buckaroo, we should git “Still Another”
%%%
%%% and if we ask for
%%%   foo, something, else, again we should throw a not found exception
%%%
%%%  An abstract storage and management piece for config related key value
%%%  pairs.
%%% @end
%%%----------------------------------------------------------------------------
-module(sin_config).

%% API
-export([new/0,
         new_from_file/2,
         new_from_terms/2,
         create_matcher/2,
         get/2,
         match/2,
         match/3,
         add/3,
         add_all/2,
         merge/2,
         remove/2,
         size/1,
         has_key/2,
         will_match/2,
         to_list/1,
         format_exception/1]).

%% Private API
-export(['__add__'/3,
         '__match__'/2,
         '__match__'/3,
         '__will_match__'/2]).

-export_type([config/0,
              matcher/0,
              read_exception/0,
              file_name/0,
              full_key/0,
              partial_entry/0,
              spec_opts/0,
              spec_name/0,
              key/0,
              value/0]).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

-define(WRAP(X), {?MODULE, X}).

%%====================================================================
%% Types
%%====================================================================


-opaque config() :: {config, list()}.
-type matcher() :: module().
-type read_exception() :: {error_accessing_file, string(), term()}.
-type file_name() :: string().
-type key() :: atom() | binary() | string().
-type specifier() :: '*' | atom().
-type full_key() :: {key(), specifier(),
                     specifier(), specifier(),
                     specifier, specifier()}.
-type spec_name() :: task | release | app | dir | module.
-type spec_opts() :: [{spec_name(), atom()}].
-type partial_key() :: key()
                     | {key(), specifier()}
                     | {key(), specifier(), specifier()}
                     | {key(), specifier(), specifier(), specifier()}
                     | {key(), specifier(), specifier(),
                        specifier(), specifier()}
                     | {key(), specifier(), specifier(), specifier(),
                        specifier(), specifier()}
                     | {key(), spec_opts()}.

-type partial_entry() :: {partial_key(), value()}.

-type value() :: term().
-type internal_config_entry() :: {full_key(), value()}.

%%====================================================================
%% API
%%====================================================================

%% @doc Create a new empty config
-spec new() -> config().
new() ->
    ?WRAP([]).

-spec new_from_file(file_name(), spec_opts()) ->
    config().
new_from_file(ConfigFile, Opts) ->
    Terms = read_config(ConfigFile),
    new_from_terms(Terms, Opts).

%% @doc Create a new config from a config file
-spec new_from_terms([partial_entry()], spec_opts()) -> config().
new_from_terms(Terms, Opts) when is_list(Terms)->
    Interim = lists:foldl(fun(Term, Config) ->
                                  [process_term(Term, Opts) | Config]
                          end, [], Terms),
    ?WRAP(sort(Interim)).

%% @doc Create a matcher that has all the specified defaults, specified by spec
%% opts
-spec create_matcher(spec_opts(), config()) ->
    matcher().
create_matcher(Opts0, Config)
  when is_list(Opts0)->
    Task = get_opt(task, Opts0),
    Release = get_opt(release, Opts0),
    App = get_opt(app, Opts0),
    Dir = get_opt(dir, Opts0),
    Module = get_opt(module, Opts0),

    sin_matcher:new(Config, Opts0, Task, Release,
                    App, Dir, Module).

%% @doc add a new key value pair to the config, if the key is an exact match
%% with an existing key the existing key is replaced.
-spec add(partial_key(), value(), config()) -> config().
add(Key0, Value, Config) ->
    Key1 = process_key(Key0, []),
    '__add__'(Key1, Value, Config).

%% @doc internal version of add
'__add__'(Key0, Value, ?WRAP(Config)) ->
    {NewConfig, Replaced} =
        lists:foldl(fun(Entry = {Key, _}, {NewConfig, Replaced}) ->
                            case exact_match(Key0, Key) of
                                true ->
                                    {[{Key, Value} | NewConfig], true};
                                false ->
                                    {[Entry | NewConfig], Replaced}
                            end
                    end,
                    {[], false},
                    Config),
    case Replaced of
        false ->
             ?WRAP(sort([{Key0, Value} | Config]));
        true ->
            ?WRAP(sort(NewConfig))
    end.

%% @doc simply loop over key value pairs adding as it goes
-spec add_all([{full_key(), value()}], config()) -> config().
add_all(KeyValuePairs, Config) ->
    lists:foldl(fun({InKey, Value}, NewConfig) ->
                        add(InKey, Value, NewConfig)
                end, Config, KeyValuePairs).

%% @doc two configs together with the first argument taking precidence over the
%% second
-spec merge(config(), config()) -> config().
merge(?WRAP(Config1), Config2) ->
    lists:foldl(fun({InKey, Value}, NewConfig) ->
                        '__add__'(InKey, Value, NewConfig)
                end, Config2, Config1).

%% @doc get a value from the key where.
-spec get(full_key(), config()) -> value().
get(Key, ?WRAP(Values))
  when erlang:is_tuple(Key), erlang:size(Key) == 6 ->
    {_, Value} = ec_lists:fetch(fun({TKey, _}) ->
                                        exact_match(TKey, Key)
                                end, Values),
    Value.

%% @doc get a value from the key where.
-spec match(partial_key(), config()) -> value().
match(Key0, Values) ->
    Key1 = process_key(Key0, []),
    '__match__'(Key1, Values).

%% @doc non key manipulated version of match
-spec '__match__'(full_key(), config()) -> value().
'__match__'(Key0, ?WRAP(Values)) ->
    {_, Value} = ec_lists:fetch(fun({TKey, _}) ->
                                        inexact_match(TKey, Key0)
                                end, Values),
    Value.

%% @doc get a value from the key where.
-spec match(partial_key(), value(), config()) -> value().
match(Key0, Default, Values) ->
    Key1 = process_key(Key0, []),
    '__match__'(Key1, Default, Values).

%% @doc internal non-key manipulated version of match/3
-spec '__match__'(full_key(), value(), config()) -> value().
'__match__'(Key0, Default, ?WRAP(Values))
  when is_tuple(Key0), erlang:size(Key0) == 6 ->
    try
        {_, Value} = ec_lists:fetch(fun({TKey, _}) ->
                                            inexact_match(TKey, Key0)
                                    end, Values),
        Value
    catch
        not_found ->
            Default
    end.

%% @doc remove the config entry by an exact match of keys
-spec remove(full_key(), config()) -> config().
remove(Key, ?WRAP(Config)) ->
    ?WRAP(sort(lists:filter(fun({TKey, _Value}) ->
                                    not exact_match(TKey, Key)
                            end,
                            Config))).

%% @doc get the current number of entries in the config
-spec size(config()) -> non_neg_integer().
size(?WRAP(Config)) ->
    erlang:length(Config).

%% @doc test to see if the key exists in the data base via an exact match
-spec has_key(full_key(), config()) -> boolean().
has_key(Key, ?WRAP(Config))
  when erlang:is_tuple(Key), erlang:size(Key) == 6 ->
    case ec_lists:find(fun({TKey, _}) ->
                               exact_match(TKey, Key)
                       end, Config) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

%% @doc test to see if the key exists in the data base via an exact match
-spec will_match(partial_key(), config()) -> boolean().
will_match(Key0, Config) ->
    Key1 = process_key(Key0, []),
    '__will_match__'(Key1, Config).

%% @doc internal non-key manipulated version of will match
-spec '__will_match__'(full_key(), config()) -> boolean().
'__will_match__'(Key, ?WRAP(Config))
  when erlang:is_tuple(Key), erlang:size(Key) == 6 ->
    case ec_lists:find(fun({TKey, _}) ->
                               inexact_match(TKey, Key)
                       end, Config) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

to_list(?WRAP(Config)) ->
    Config.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc read in the config file using file:consult
-spec  read_config(file_name()) -> [term()].
read_config(ConfigFile) ->
     case file:consult(ConfigFile) of
        {ok, Terms} ->
            Terms;
        {error, Reason} ->
            throw({error_accessing_file, ConfigFile,  Reason})
    end.

%% @doc convert the user entered value into an expanded full value tuple
-spec process_term(partial_entry(), spec_opts()) ->
    internal_config_entry().
process_term({Key, Value}, Opts) ->
    {process_key(Key, Opts), Value}.

%% @doc convert the user entered key into an expanded full key tuple
-spec process_key(partial_entry(),  spec_opts()) ->
    full_key().
process_key({Key, UserOpts = [{A, V} | _]}, Opts)
  when is_atom(A), is_atom(V) ->
    NewOpts = UserOpts ++ Opts,
    {Key,
     get_opt(task, NewOpts),
     get_opt(release, NewOpts),
     get_opt(app, NewOpts),
     get_opt(dir, NewOpts),
     get_opt(module, NewOpts)};
process_key(Key, Opts) ->
    {Key,
     get_opt(task, Opts),
     get_opt(release, Opts),
     get_opt(app, Opts),
     get_opt(dir, Opts),
     get_opt(module, Opts)}.


%% @doc given a spec list and a spec name return the value from the
-spec get_opt(spec_name(), spec_opts()) -> '*' | atom().
get_opt(OptName, Opts) ->
    case lists:keyfind(OptName, 1, Opts) of
        {OptName, Value} ->
            Value;
        false ->
            '*'
    end.

equality([{'*', '*'} | Rest]) ->
    equality(Rest);
equality([{'*', _B} | _Rest]) ->
    false;
equality([{_A, '*'} | Rest]) ->
    equality(Rest);
equality([{A, B} | Rest])
  when A == B ->
    equality(Rest);
equality([{A, B} | _Rest])
  when A < B ->
    false;
equality([{A, B} | _Rest])
  when A > B ->
    true;
equality([]) ->
    true.

sort(Config) ->
    lists:sort(fun({{AK, AS1, AS2, AS3, AS4, AS5}, _},
                   {{BK, BS1, BS2, BS3, BS4, BS5}, _}) ->
                      equality([{AK, BK}, {AS1, BS1}, {AS2, BS2},
                                {AS3, BS3}, {AS4, BS4}, {AS5, BS5}])
               end, Config).

%% @doc allow a loose matching where wildcards in K1 will match against any
%% value in K2
-spec inexact_match(K1::full_key(), K2::full_key()) -> boolean().
inexact_match({Key, TaskName, ReleaseName, AppName, DirName, ModName},
                {Key, UTaskName, UReleaseName, UAppName, UDirName, UModName})
  when (TaskName == '*' orelse TaskName == UTaskName) andalso
       (ReleaseName == '*' orelse ReleaseName == UReleaseName) andalso
       (AppName == '*' orelse AppName == UAppName) andalso
       (DirName == '*' orelse DirName == UDirName) andalso
       (ModName == '*' orelse ModName == UModName) ->
    true;
inexact_match(_IncomingKey, _Key) ->
     false.

%% @doc make sure the two keys match exactly
-spec exact_match(full_key(), full_key()) -> boolean().
exact_match(K1, K1) ->
    true;
exact_match(_IncomingKey, _Key) ->
    false.
