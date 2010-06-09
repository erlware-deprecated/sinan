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

-behaviour(gen_server).

-include("eunit.hrl").

%% API
-export([start_link/1, start_link/3, start_link/4,
         start_config/4,
         start_config/3,
         stop_config/1,
         get_seed/1,
         store/3,
         get_value/2,
         get_value/3,
	 get_pairs/1,
         delete/2,
         shutdown/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(RECHECK, 1000 * 60 * 5).
-define(SERVER, ?MODULE).

-record(state, {config, project_dir, build_id = no_id, canonical}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(ProjectDir) -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link(ProjectDir) ->
    gen_server:start_link(?MODULE, [ProjectDir], []).

start_link(BuildId, ProjectDir, Config, Override) ->
    gen_server:start_link(?MODULE, [BuildId, ProjectDir, Config, Override], []).

start_link(BuildId, ProjectDir,  Override) ->
    gen_server:start_link(?MODULE, [BuildId, ProjectDir, Override], []).


%%--------------------------------------------------------------------
%% @doc
%%   Start a new config with a handler and and override
%%
%% @spec (ProjectDir, BuildId, Seed, Override) -> ok
%% @end
%%--------------------------------------------------------------------
start_config(ProjectDir, BuildId, Seed, Override) ->
    sin_config_sup:start_config(ProjectDir, BuildId, Seed, Override).

%%--------------------------------------------------------------------
%% @doc
%%   Start a new config with a handler and and override
%%
%% @spec (ProjectDir, BuildId, Override) -> ok
%% @end
%%--------------------------------------------------------------------
start_config(ProjectDir, BuildId, Override) ->
    sin_config_sup:start_config(ProjectDir, BuildId, Override).


%%--------------------------------------------------------------------
%% @doc
%%  Stop the config identified by the name.
%% @spec stop_config(Name) -> ok
%% @end
%%--------------------------------------------------------------------
stop_config(BuildId) ->
    shutdown(BuildId).

%%--------------------------------------------------------------------
%% @doc
%%  Get a preexisting seed from the the seed config.
%% @spec (ProjectDir) -> Seed
%% @end
%%--------------------------------------------------------------------
get_seed(ProjectDir) when is_list(ProjectDir) ->
    case sin_config_registry:get_canonical(ProjectDir) of
        undefined ->
            case sin_config_sup:start_canonical(ProjectDir) of
                {error, Error} ->
                    throw({unable_to_create_canonical, Error});
                {error, _, Desc} ->
                    throw({unable_to_create_canonical, Desc});
                _ ->
                    get_seed(ProjectDir)
            end;
        Pid ->
            gen_server:call(Pid, get_raw_config)
    end;
get_seed(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_raw_config).

%%-------------------------------------------------------------------
%% @doc
%%  Add a key to the config.
%%
%% @spec store(BuildId, Path, Value::any()) -> ok
%% @end
%%-------------------------------------------------------------------
store(BuildId, Path, Value) ->
    Pid = sin_config_registry:get_by_build_id(BuildId),
    gen_server:cast(Pid, {add, Path, Value}),
    ok.

%%-------------------------------------------------------------------
%% @doc
%%  Get a value from the config.
%%
%% @spec get_value(BuildId, Path) -> Value | undefined
%% @end
%%-------------------------------------------------------------------
get_value(BuildId, Path) ->
    Pid = sin_config_registry:get_by_build_id(BuildId),
    gen_server:call(Pid, {get, Path}).

%%--------------------------------------------------------------------
%% @doc
%%  Attempts to get the specified key. If the key doesn't exist it
%%  returns the requested default instead of just undefined.
%%
%% @spec get_value(BuildId, Key, Default) -> Value | Default
%% @end
%%--------------------------------------------------------------------
get_value(BuildId, Key, Default) ->
    case get_value(BuildId, Key) of
        undefined ->
            Default;
        Else ->
            Else
    end.

%%-------------------------------------------------------------------
%% @doc
%%  Delete a value from the config.
%%
%% @spec delete(BuildId, Key) -> ok
%% @end
%%-------------------------------------------------------------------
delete(BuildId, Key) ->
    Pid = sin_config_registry:get_by_build_id(BuildId),
    gen_server:cast(Pid, {delete,  Key}).

%%-------------------------------------------------------------------
%% @doc
%%  Get the complete config as key,value pairs
%%
%% @spec (BuildId) -> ListOfKeyValuePairs::list()
%% @end
%%-------------------------------------------------------------------
get_pairs(BuildId) ->
    Pid = sin_config_registry:get_by_build_id(BuildId),
    gen_server:call(Pid, {get_pairs}).

%%--------------------------------------------------------------------
%% @doc
%%  Tell sin_build_config to shutdown.
%%
%% @spec (BuildId) -> ok
%% @end
%%--------------------------------------------------------------------
shutdown(BuildId) ->
    Pid = sin_config_registry:get_by_build_id(BuildId),
    gen_server:cast(Pid, exit).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ProjectDir]) ->
    try get_build_config(ProjectDir) of
        Config ->
            sin_config_registry:register_canonical(ProjectDir, self()),
            {ok, #state{config = Config, project_dir = ProjectDir,
                        canonical = true}, ?RECHECK}
    catch
        Error ->
            {stop, Error}
    end;
init([BuildId, ProjectDir, Override]) ->
    Config = dict:new(),
    NewConfig = merge_config(Config, Override, ""),
    sin_config_registry:register_config(BuildId, self()),
    {ok, #state{config = NewConfig, build_id = BuildId,
                project_dir = ProjectDir, canonical = false},
     ?RECHECK};
init([BuildId, ProjectDir, Config, Override]) ->
    OverrideDict = merge_config(dict:new(), Override, ""),
    Flavor = get_build_flavor(Config, OverrideDict),
    NewConfig0 = merge_config(apply_flavors(Config, Flavor), Override, ""),
    BuildConfigDict = get_build_config(ProjectDir),
    NewConfig = merge_config(NewConfig0, dict:to_list(BuildConfigDict), ""),
    BuildRoot = filename:join([ProjectDir, in_get_value(NewConfig, "build_dir",
                                                        "_build")]),
    BuildDir = filename:join([BuildRoot, Flavor]),
    NewConfig1 = in_store("build.dir", NewConfig, BuildDir),
    NewConfig2 = in_store("build.root", NewConfig1, BuildRoot),
    NewConfig3 = in_store("build.flavor", NewConfig2, Flavor),
    sin_config_registry:register_config(BuildId, self()),
    {ok, #state{config = NewConfig3, build_id = BuildId,
                project_dir = ProjectDir, canonical = false},
     ?RECHECK}.

%%--------------------------------------------------------------------
%% @doc
%%  Return the flavor for the current build attempt
%% @spec get_build_flavor(Config, Override) -> Flavor
%% @end
%%--------------------------------------------------------------------
get_build_flavor(Config, Override) ->
   case in_get_value(Override, "build.flavor", undefined) of
       undefined ->
           in_get_value(Config, "build.flavor", "development");
       Value ->
           Value
   end.

%%--------------------------------------------------------------------
%% @doc
%%  Apply flavor changes to the config file.
%% @spec (Config, Flavor) -> NewConfig
%% @end
%%--------------------------------------------------------------------
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


%%-------------------------------------------------------<-------------
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_raw_config, _From, State = #state{config = Config,
                                                  project_dir = ProjectDir}) ->
    BuildRoot = in_get_value(Config, "build_dir", "_build"),
    BuildDir = filename:join([ProjectDir, BuildRoot]),
    {ok, BuildConfig} = dict:find("build.config", Config),
    case sin_sig:changed("config", BuildDir, BuildConfig) of
        false ->
            {reply, Config, State, ?RECHECK};
        _ ->
            NewConfig = get_build_config(ProjectDir),
            {reply, NewConfig, State#state{config = NewConfig}, ?RECHECK}
    end;
handle_call({get, Key}, _From, State = #state{config = Config}) ->
    case get_item(Key, Config) of
        {ok, Reply} ->
            {reply, Reply, State, ?RECHECK};
        error ->
            {reply, undefined, State, ?RECHECK}
    end;
handle_call({get_pairs}, _From, State = #state{config = Config}) ->
    case in_get_pairs(Config) of
        {ok, Reply} ->
            {reply, Reply, State, ?RECHECK};
        error ->
            {reply, error, State, ?RECHECK}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({add, Key, Value}, State = #state{config = Config}) ->
    case in_store(Key, Config, Value) of
        error ->
            error_logger:warning_msg("Unable to add ~w", [Key]),
            {noreply, State#state{config = Config}, ?RECHECK};
        NConfig ->
            {noreply, State#state{config = NConfig}, ?RECHECK}
    end;
handle_cast({delete, Key}, State = #state{config = Config}) ->
    case in_delete(Key, Config) of
        error ->
            error_logger:warning_msg("Unable to delete ~w", [Key]),
            {noreply, State#state{config = Config}, ?RECHECK};
        Config ->
            {noreply, State#state{config = Config}, ?RECHECK}
    end;
handle_cast(exit, _) ->
    exit(normal).


%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, State = #state{canonical = true,
                                    project_dir = ProjectDir}) ->
    Config = get_build_config(ProjectDir),
    {noreply, State#state{config = Config}};
handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{project_dir = ProjectDir,
                          build_id = no_id, canonical = true}) ->
    sin_config_registry:unregister_canonical(ProjectDir),
    ok;
terminate(_Reason, #state{build_id = BuildId}) ->
    sin_config_registry:unregister_config(BuildId),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal Functions
%%====================================================================
%%-------------------------------------------------------------------
%% @doc
%%   Find the build config under _build.cfg or sinan.cfg
%%
%% @spec (ProjectDir::string()) -> NewConfig
%% @end
%% @private
%%------------------------------------------------------------------
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
%% @spec (ProjectDir::string(), BuildConfig::dict()) -> NewConfig
%% @end
%% @private
%%-------------------------------------------------------------------
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
%% @spec (Config, JSONData, CurrentName) -> NewConfig
%% @end
%%--------------------------------------------------------------------
merge_config(Config, {obj, Data}, CurrentName) ->
    merge_config(Config, Data, CurrentName);
merge_config(Config, [{Key, {obj, Data}} | Rest], "") ->
    NewConfig = merge_config(Config, Data, Key),
    merge_config(NewConfig, Rest, "");
merge_config(Config, [{Key, {obj, Data}} | Rest], CurrentName) ->
    NewConfig = merge_config(Config, Data, CurrentName ++ "." ++ Key),
    merge_config(NewConfig, Rest, CurrentName);
merge_config(Config, [{Key, Value} | Rest], "") ->
    NewConfig = dict:store(Key, Value, Config),
    merge_config(NewConfig, Rest, "");
merge_config(Config, [{Key, Value} | Rest], CurrentName) ->
    NewConfig = dict:store(CurrentName ++ "." ++ Key, Value, Config),
    merge_config(NewConfig, Rest, CurrentName);
merge_config(Config, [], _) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%%  Given a key return the value for that key. If the value
%% is not present return the default value.
%% @spec (Config, Key, DefaultValue) -> Value | DefaultValue
%% @end
%%--------------------------------------------------------------------
in_get_value(Config, Key, DefaultValue) ->
    case catch dict:fetch(Key, Config) of
        {'EXIT', _} ->
            DefaultValue;
        Value ->
            Value
    end.


%%--------------------------------------------------------------------
%% @doc
%%  Get the item from the dict recursivly pulling each value.
%% @spec (Name, Object) -> {ok, Item} | error
%% @end
%% @private
%%--------------------------------------------------------------------
get_item(Key, Config) ->
    dict:find(Key, Config).

%%--------------------------------------------------------------------
%% @doc
%%  Get the complete list of key value pairs from the system
%% @spec (Config) -> {ok, KeyValuePairs} | error
%% @end
%% @private
%%--------------------------------------------------------------------
in_get_pairs(Config) ->
    {ok, dict:to_list(Config)}.

%%--------------------------------------------------------------------
%% @doc
%%  Delete the value from the dict recursively.
%% @spec (Key, Config) -> Config | Error
%% @end
%% @private
%%--------------------------------------------------------------------
in_delete(Key, Config) ->
    dict:erase(Key, Config).


%%--------------------------------------------------------------------
%% @doc
%%  Store the value recursively into the dict and sub dicts.
%% @spec (Key, Dict, Value) -> NDict
%% @end
%% @private
%%--------------------------------------------------------------------
in_store(Key, Config, Value) ->
    dict:store(Key, Value, Config).


%%====================================================================
%%% Tests
%%====================================================================
