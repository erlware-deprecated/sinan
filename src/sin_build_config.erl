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
	 new/3,
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
	      project_dir/0,
	      build_config/0]).

%%====================================================================
%% Types
%%====================================================================
-type key() :: term().
-type value() :: term().
-type project_dir() :: string().
-opaque build_config() :: any().


%%====================================================================
%% API
%%====================================================================

%% @doc
%%  Parse the command line args into a spec that can be overridden.
%% @end
-spec parse_args(list()) -> build_config().
parse_args(ArgList) when is_list(ArgList) ->
    parse_args(ArgList, dict:new()).

%%--------------------------------------------------------------------
%% @doc
%%  Get a preexisting seed from the the seed config.
%% @end
%%--------------------------------------------------------------------
-spec get_seed(project_dir()) -> build_config().
get_seed(ProjectDir) when is_list(ProjectDir) ->
    new(ProjectDir).

%%-------------------------------------------------------------------
%% @doc
%%  Add a key to the config.
%%
%% @end
%%-------------------------------------------------------------------
-spec store(build_config(), key(), value()) -> build_config().
store(BuildConfig, Key, Value) ->
    dict:store(Key, Value, BuildConfig).

%%-------------------------------------------------------------------
%% @doc
%%  Get a value from the config.
%% @end
%%-------------------------------------------------------------------
-spec get_value(build_config(), key()) -> value() | undefined.
get_value(BuildConfig, Key) ->
    case dict:find(Key, BuildConfig) of
	error ->
	    undefined;
	{ok, Value} when is_binary(Value) ->
	    binary_to_list(Value);
	{ok, Value} ->
	    Value
    end.


%%--------------------------------------------------------------------
%% @doc
%%  Attempts to get the specified key. If the key doesn't exist it
%%  returns the requested default instead of just undefined.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_value(build_config(), key(), value()) -> value().
get_value(BuildConfig, Key, DefaultValue) ->
    case get_value(BuildConfig, Key) of
	undefined ->
            DefaultValue;
	Value ->
	    Value
    end.

%%-------------------------------------------------------------------
%% @doc
%%  Delete a value from the config.
%%
%% @end
%%-------------------------------------------------------------------
-spec delete(build_config(), key()) -> build_config().
delete(BuildConfig, Key) ->
    dict:erase(Key, BuildConfig).

%%-------------------------------------------------------------------
%% @doc
%%  Get the complete config as key,value pairs
%%
%% @end
%%-------------------------------------------------------------------
-spec get_pairs(build_config()) -> [{key(), value()}].
get_pairs(BuildConfig) ->
	      dict:to_list(BuildConfig).

-spec new() -> build_config().
new() ->
    dict:new().

-spec new(project_dir() | build_config()) -> build_config().
new(ProjectDir) when is_list(ProjectDir)->
    try get_build_config(ProjectDir) of
        Config ->
	    Config
    catch
        Error ->
	    throw(Error)
    end;
new(Override) ->
    Config = dict:new(),
    NewConfig = merge_build_configs(Config, Override),
    NewConfig.

-spec new(project_dir(), build_config(), build_config()) -> build_config().
new(ProjectDir, Config, Override) ->
    Flavor = get_build_flavor(Config, Override),
    NewConfig0 = merge_build_configs(apply_flavors(Config, Flavor), Override),
    BuildConfigDict = get_build_config(ProjectDir),
    NewConfig = merge_config(NewConfig0, dict:to_list(BuildConfigDict), ""),
    BuildRoot = filename:join([ProjectDir, get_value(NewConfig, "build_dir",
                                                        "_build")]),
    BuildDir = filename:join([BuildRoot, Flavor]),
    NewConfig1 = store(NewConfig, "build.dir", BuildDir),
    NewConfig2 = store(NewConfig1, "build.root", BuildRoot),
    store(NewConfig2, "build.flavor",  Flavor).

%%====================================================================
%% Internal Functions
%%====================================================================
%% @doc
%%  Merges the build config. The second config always overrides the first
%% @end
-spec merge_build_configs(build_config(), build_config()) -> build_config().
merge_build_configs(Config1, Config2) ->
    dict:merge(fun(_Key, _Value1, Value2) ->
		       Value2
	       end, Config1, Config2).
%%--------------------------------------------------------------------
%% @doc
%%  Return the flavor for the current build attempt
%% @end
%%--------------------------------------------------------------------
-spec get_build_flavor(build_config(), build_config()) -> string().
get_build_flavor(Config, Override) ->
   case get_value(Override, "build.flavor", undefined) of
       undefined ->
           get_value(Config, "build.flavor", "development");
       Value ->
           Value
   end.

%%--------------------------------------------------------------------
%% @doc
%%  Apply flavor changes to the config file.
%% @end
%%--------------------------------------------------------------------
-spec apply_flavors(build_config(), build_config()) -> build_config().
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


%%-------------------------------------------------------------------
%% @doc
%%   Find the build config under _build.cfg or sinan.cfg
%%
%% @end
%% @private
%%------------------------------------------------------------------
-spec get_build_config(project_dir()) -> build_config().
get_build_config(ProjectDir) ->
    Config1 = filename:join([ProjectDir, "_build.cfg"]),
    Config2 = filename:join([ProjectDir, "sinan.cfg"]),
    case sin_utils:file_exists(Config1) of
	true ->
	    process_build_config(ProjectDir, Config1);
	false ->
	    case sin_utils:file_exists(Config2) of
		true ->
		    process_build_config(ProjectDir, Config2);
		false ->
		    throw(no_config_file)
	    end
    end.

%%-------------------------------------------------------------------
%% @doc
%%   Read in the build config/parse and send to the config process.
%%
%% @end
%% @private
%%-------------------------------------------------------------------
-spec process_build_config(project_dir(), build_config()) -> build_config().
process_build_config(ProjectDir, BuildConfig) ->
    DefaultData =
        sin_config_parser:parse_config_file(filename:join([code:priv_dir(sinan),
                                                           "default_build"])),
    Config = merge_config(dict:new(), DefaultData, ""),
    Data = sin_config_parser:parse_config_file(BuildConfig),

    NewConfig = merge_config(Config, Data, ""),

    NewConfig1 = dict:store("build.config", BuildConfig, NewConfig),
    NewConfig2 = dict:store("project.dir", ProjectDir, NewConfig1),

    sin_discover:discover(ProjectDir, NewConfig2).


%%--------------------------------------------------------------------
%% @doc
%%  Take the parsed config data and push it into the actual
%%  config.
%% @end
%%--------------------------------------------------------------------
-spec merge_config(build_config(), JSONData::any(), term()) -> build_config().
merge_config(Config, {obj, Data}, CurrentName) ->
    merge_config(Config, Data, CurrentName);
merge_config(Config, [{Key, {obj, Data}} | Rest], "") ->
    NewConfig = merge_config(Config, Data, convert_value(Key)),
    merge_config(NewConfig, Rest, "");
merge_config(Config, [{Key, {obj, Data}} | Rest], CurrentName) ->
    NewConfig = merge_config(Config, Data, CurrentName ++ "." ++ convert_value(Key)),
    merge_config(NewConfig, Rest, CurrentName);
merge_config(Config, [{Key, Value} | Rest], "") ->
    NewConfig = dict:store(convert_value(Key), convert_value(Value), Config),
    merge_config(NewConfig, Rest, "");
merge_config(Config, [{Key, Value} | Rest], CurrentName) ->
    NewConfig = dict:store(CurrentName ++ "." ++ convert_value(Key), convert_value(Value), Config),
    merge_config(NewConfig, Rest, CurrentName);
merge_config(Config, [], _) ->
    Config.


%%--------------------------------------------------------------------
%% @doc
%%  take a list of binaries and convert it to a list of lists.
%% @end
%%--------------------------------------------------------------------
-spec convert_list(Target::list(), Acc::list()) -> list().
convert_list([H | T], Acc) when is_binary(H) ->
    convert_list(T, [binary_to_list(H) | Acc]);
convert_list([H | T], Acc) ->
    convert_list(T, [H | Acc]);
convert_list([], Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @doc
%%  Convert any values taken from the config from a binary to a list.
%%  ktuo treats lists a binaries but we would rather use lists in the
%%  system.
%% @end
%%--------------------------------------------------------------------
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
-spec parse_args(list(), build_config()) -> build_config().
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
