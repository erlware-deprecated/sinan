%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Eric Merritt
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
%%%  Represents a build config
%%% @end
%%% Created : 30 Oct 2006 by Eric Merritt <ericbmerritt@gmail.com>
%%%----------------------------------------------------------------------------
-module(sin_build_config).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%% API
-export([new/0,
	 new/1,
	 apply_flavor/1,
	 merge_configs/2,
	 parse_args/1,
	 parse_args/2,
         get_seed/1,
         store/3,
         get_value/2,
         get_value/3,
	 get_pairs/1,
         delete/2]).


-export_type([key/0,
	      value/0,
	      config/0]).

%%====================================================================
%% Types
%%====================================================================
-type key() :: term().
-type value() :: term().
-opaque config() :: any().


%%====================================================================
%% API
%%====================================================================
%% @doc
%% Create a new empty config
%% @end
-spec new() -> config().
new() ->
    dict:new().

%% @doc
%% Create a new config from a config file
%% @end
-spec new(ConfigFile::string()) -> config().
new(ConfigFile) when is_list(ConfigFile)->
    case sin_utils:file_exists(ConfigFile) of
	true ->
	    parse_config_file(ConfigFile);
	false ->
	    throw(invalid_config_file)
    end.

%% @doc
%%  Parse the command line args into a spec that can be overridden.
%% @end
-spec parse_args(list()) -> config().
parse_args(ArgList) when is_list(ArgList) ->
    parse_args(ArgList, dict:new()).

%% @doc
%%  Get a preexisting seed from the the seed config.
%% @end
-spec get_seed(string()) -> config().
get_seed(ProjectDir) when is_list(ProjectDir) ->
    new(ProjectDir).

%% @doc
%%  Add a key to the config.
%% @end
-spec store(config(), key(), value()) -> config().
store(BuildConfig, Key, Value) ->
    dict:store(Key, Value, BuildConfig).


%% @doc
%%  Get a value from the config.
%% @end
-spec get_value(config(), key()) -> value() | undefined.
get_value(BuildConfig, Key) ->
    case dict:find(Key, BuildConfig) of
	error ->
	    undefined;
	{ok, Value} when is_binary(Value) ->
	    binary_to_list(Value);
	{ok, Value} ->
	    Value
    end.


%% @doc
%%  Attempts to get the specified key. If the key doesn't exist it
%%  returns the requested default instead of just undefined.
%% @end
-spec get_value(config(), key(), value()) -> value().
get_value(BuildConfig, Key, DefaultValue) ->
    case get_value(BuildConfig, Key) of
	undefined ->
            DefaultValue;
	Value ->
	    Value
    end.

%% @doc
%%  Delete a value from the config.
%% @end
-spec delete(config(), key()) -> config().
delete(BuildConfig, Key) ->
    dict:erase(Key, BuildConfig).

%% @doc
%%  Get the complete config as key,value pairs
%% @end
-spec get_pairs(config()) -> [{key(), value()}].
get_pairs(BuildConfig) ->
	      dict:to_list(BuildConfig).


-spec apply_flavor(config()) -> config().
apply_flavor(Config0) ->
    Flavor = get_build_flavor(Config0),
    Config1 = store(Config0, "build.flavor", Flavor),
    apply_flavors(Config1, Flavor).


%% @doc
%%  Merges the build config. The second config always overrides the first
%% @end
-spec merge_configs(config(), config()) -> config().
merge_configs(Config1, Config2) ->
    dict:merge(fun(_Key, _Value1, Value2) ->
		       Value2
	       end, Config1, Config2).


%%====================================================================
%% Internal Functions
%%====================================================================
%% @doc
%% Convert a text file to a fully parsed config object
%% @end
-spec parse_config_file(ConfigFile::string()) ->
    config().
parse_config_file(ConfigFile) ->
    convert_parsed_data(new(),
			sin_config_parser:parse_config_file(ConfigFile),
			"").

%%--------------------------------------------------------------------
%% @doc
%%  Return the flavor for the current build attempt
%% @end
%%--------------------------------------------------------------------
-spec get_build_flavor(config()) -> string().
get_build_flavor(Config) ->
    get_value(Config, "build.flavor", "development").

%% @doc
%%  Apply flavor changes to the config file.
%% @end
-spec apply_flavors(config(), config()) -> config().
apply_flavors(Config, Flavor) ->
    FilterFun = fun([$f, $l, $a, $v, $o, $r, $s, $. | Rest], Value, NConfig) ->
                        case lists:prefix(Flavor, Rest) of
                            true ->
                                NewKey = "tasks." ++
                                    lists:nthtail(length(Flavor) + 1, Rest),
                                dict:store(NewKey, Value, NConfig);
                            _ ->
                                NConfig
                        end;
                   (_, _, NConfig) ->
                        NConfig
                end,
    dict:fold(FilterFun, Config, Config).



%% @doc
%%  Take the parsed config data and push it into the actual
%%  config.
%% @end
-spec convert_parsed_data(config(), JSONData::any(), term()) -> config().
convert_parsed_data(Config, {obj, Data}, CurrentName) ->
    convert_parsed_data(Config, Data, CurrentName);
convert_parsed_data(Config, [{Key, {obj, Data}} | Rest], "") ->
    NewConfig = convert_parsed_data(Config, Data, convert_value(Key)),
    convert_parsed_data(NewConfig, Rest, "");
convert_parsed_data(Config, [{Key, {obj, Data}} | Rest], CurrentName) ->
    NewConfig = convert_parsed_data(Config, Data, CurrentName ++ "." ++
			     convert_value(Key)),
    convert_parsed_data(NewConfig, Rest, CurrentName);
convert_parsed_data(Config, [{Key, Value} | Rest], "") ->
    NewConfig = dict:store(convert_value(Key), convert_value(Value), Config),
    convert_parsed_data(NewConfig, Rest, "");
convert_parsed_data(Config, [{Key, Value} | Rest], CurrentName) ->
    NewConfig = dict:store(CurrentName ++ "." ++ convert_value(Key),
			   convert_value(Value), Config),
    convert_parsed_data(NewConfig, Rest, CurrentName);
convert_parsed_data(Config, [], _) ->
    Config.


%% @doc
%%  take a list of binaries and convert it to a list of lists.
%% @end
-spec convert_list(Target::list(), Acc::list()) -> list().
convert_list([H | T], Acc) when is_binary(H) ->
    convert_list(T, [binary_to_list(H) | Acc]);
convert_list([H | T], Acc) ->
    convert_list(T, [H | Acc]);
convert_list([], Acc) ->
    lists:reverse(Acc).

%% @doc
%%  Convert any values taken from the config from a binary to a list.
%%  ktuo treats lists a binaries but we would rather use lists in the
%%  system.
%% @end
-spec convert_value(Value::any()) -> any().
convert_value(Value) when is_list(Value) ->
    convert_list(Value, []);
convert_value(Value) when is_binary(Value) ->
    binary_to_list(Value);
convert_value(Value) ->
    Value.

%% @doc
%%  Parse pairs of keys and values into a build config form
%% @end
-spec parse_args(list(), config()) -> config().
parse_args([Key, Value | Rest], Dict) ->
    parse_args(Rest, dict:store(strip_key(Key, []), Value, Dict));
parse_args([], Dict) ->
    Dict;
parse_args(_, _Dict) ->
    ?SIN_RAISE_D(sinan_arg_exception, "Problem parsing config args").

%% @doc
%%  strip out the : and replace it with .
%% @end
-spec strip_key(string(), list()) -> string().
strip_key([$: | Rest], Acc) ->
    strip_key(Rest, [$. | Acc]);
strip_key([H | Rest], Acc) ->
    strip_key(Rest, [H | Acc]);
strip_key([], Acc) ->
    lists:reverse(Acc).
%%====================================================================
%%% Tests
%%====================================================================
new_0_test() ->
    Config = new(),
    ?assertMatch(0, dict:size(Config)).

new_1_test() ->
    TestBuildConfigFile = filename:join(["test_data",
					 "sin_build_config",
					 "test_config.cfg"]),
    NonExistantBuildConfigFile = filename:join(["test_data",
						"sin_build_config",
						"doesnt_exist.cfg"]),
    Config = new(TestBuildConfigFile),
    ?assertMatch("test", get_value(Config, "project.name")),
    ?assertMatch("0.0.0.1", get_value(Config, "project.vsn")),
    ?assertException(throw, invalid_config_file,
		     new(NonExistantBuildConfigFile)).





