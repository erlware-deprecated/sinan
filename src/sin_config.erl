%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  An abstract storage and management piece for config related key value
%%%  pairs.
%%% @end
%%%----------------------------------------------------------------------------
-module(sin_config).

%% API
-export([new/0,
	 new/1,
	 apply_flavor/1,
	 merge_configs/2,
	 parse_args/1,
	 parse_args/2,
         get_seed/1,
	 store/2,
         store/3,
         get_value/2,
         get_value/3,
	 get_pairs/1,
         delete/2]).

-export_type([key/0,
	      value/0,
	      config/0]).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

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
-spec new() -> config().
new() ->
    dict:new().

%% @doc
%% Create a new config from a config file
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
-spec parse_args(list()) -> config().
parse_args(ArgList) when is_list(ArgList) ->
    parse_args(ArgList, dict:new()).

%% @doc
%%  Get a preexisting seed from the the seed config.
-spec get_seed(string()) -> config().
get_seed(ProjectDir) when is_list(ProjectDir) ->
    new(ProjectDir).

%% @doc
%%  Add a key to the config.
-spec store(config(), key(), value()) -> config().
store(Config, Key, Value) ->
    dict:store(Key, Value, Config).

%% @doc
%% Store a list of key value pairs into the config
-spec store(config(), KeyValuePairs::[{string(), term()}]) ->
    config().
store(Config, KeyValuePairs) when is_list(KeyValuePairs) ->
    lists:foldl(fun ({Key, Value}, Dict) ->
			dict:store(Key, Value, Dict)
		end, Config, KeyValuePairs).

%% @doc
%%  Get a value from the config.
-spec get_value(config(), key()) -> value() | undefined.
get_value(Config, Key) ->
    case dict:find(Key, Config) of
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
-spec get_value(config(), key(), value()) -> value().
get_value(Config, Key, DefaultValue) ->
    case get_value(Config, Key) of
	undefined ->
            DefaultValue;
	Value ->
	    Value
    end.

%% @doc
%%  Delete a value from the config.
-spec delete(config(), key()) -> config().
delete(Config, Key) ->
    dict:erase(Key, Config).

%% @doc
%%  Get the complete config as key,value pairs
-spec get_pairs(config()) -> [{key(), value()}].
get_pairs(Config) ->
	      dict:to_list(Config).

%% @doc
%% Apply the build flavor overrides to the system
-spec apply_flavor(config()) -> config().
apply_flavor(Config0) ->
    Flavor = get_build_flavor(Config0),
    Config1 = store(Config0, "build.flavor", Flavor),
    apply_flavors(Config1, Flavor).


%% @doc
%%  Merges the build config. The second config always overrides the first
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
-spec parse_config_file(ConfigFile::string()) ->
    config().
parse_config_file(ConfigFile) ->
    convert_parsed_data(new(),
			sin_config_parser:parse_config_file(ConfigFile),
			"").

%% @doc
%%  Return the flavor for the current build attempt
-spec get_build_flavor(config()) -> string().
get_build_flavor(Config) ->
    get_value(Config, "build.flavor", "development").

%% @doc
%%  Apply flavor changes to the config file.
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
-spec convert_value(Value::any()) -> any().
convert_value(Value) when is_list(Value) ->
    convert_list(Value, []);
convert_value(Value) when is_binary(Value) ->
    binary_to_list(Value);
convert_value(Value) ->
    Value.

%% @doc
%%  Parse pairs of keys and values into a build config form
-spec parse_args(list(), config()) -> config().
parse_args([Value], Dict) ->
    % A single value on the system becomes commandline.arg and
    % accessable to other parts of the build.
    dict:store("command_line.arg", Value, Dict);
parse_args([Key, Value | Rest], Dict) ->
    parse_args(Rest, dict:store(strip_key(Key, []), Value, Dict));
parse_args([], Dict) ->
    Dict;
parse_args(_, _Dict) ->
    ?SIN_RAISE_D(sinan_arg_exception, "Problem parsing config args").

%% @doc
%%  strip out the : and replace it with .
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
    TestConfigFile = filename:join(["test_data",
					 "sin_config",
					 "test_config.cfg"]),
    NonExistantConfigFile = filename:join(["test_data",
						"sin_config",
						"doesnt_exist.cfg"]),
    Config = new(TestConfigFile),
    ?assertMatch("test", get_value(Config, "project.name")),
    ?assertMatch("0.0.0.1", get_value(Config, "project.vsn")),
    ?assertException(throw, invalid_config_file,
		     new(NonExistantConfigFile)).

parse_args_test() ->
    Config = parse_args(["Key", "Value"], new()),
    Config2 = parse_args(["Key", "Value", "CmdValue"], new()),
    Config3 = parse_args(["CmdValue"], new()),
    ?assertMatch("Value", get_value(Config, "Key")),
    ?assertMatch("Value", get_value(Config2, "Key")),
    ?assertMatch("CmdValue", get_value(Config2, "command_line.arg")),
    ?assertMatch("CmdValue", get_value(Config3, "command_line.arg")),
    ?assertMatch(undefined, get_value(Config3, "Key")).




