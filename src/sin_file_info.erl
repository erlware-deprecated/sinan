%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @copyright Erlware, LLC
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Parse an erlang file (yrl, hrl, erl) and information about the file
%%% that is relevant to the build system
%%% @end
%%%-------------------------------------------------------------------
-module(sin_file_info).

%% API
-export([process_file/3]).
-export_type([date/0, time/0, date_time/0, mod/0]).

-include_lib("sinan/include/sinan.hrl").
-include_lib("kernel/include/file.hrl").


%%====================================================================
%% Types
%%====================================================================
-type date() :: {Year :: non_neg_integer(),
                 Month :: non_neg_integer(),
                 Day :: non_neg_integer()}.

-type time() :: {Hour :: non_neg_integer(),
                 Minute :: non_neg_integer(),
                 Second :: non_neg_integer()}.

-type date_time() :: {date() , time()}.
-type mod() :: record(module).

%%====================================================================
%% API
%%====================================================================
-spec process_file(sin_state:state(), string(), [string()]) ->
                          sinan:mod().
process_file(State0, Path0, Includes) ->
    sin_sig:do_if_changed(?MODULE,
                          Path0,
                          fun deps_changed/2,
                          fun(Path1, State1) ->
                                  do_extract(State1, Path1,  Includes)
                          end, State0).

%%====================================================================
%% Internal Functions
%%====================================================================
-spec deps_changed(mod(), sin_state:state()) -> boolean().
deps_changed(#module{include_timestamps=Includes}, State) ->
    lists:any(fun({Path, Stamp}) ->
                      not Stamp == get_timestamp_info(State, Path)
              end, Includes).

-spec do_extract(sin_state:state(), string(), [string()]) ->
                        {sin_state:state(), mod()}.
do_extract(State0, Path, Includes) ->
    case filename:extension(Path) of
        Ext when Ext == ".erl";
                 Ext == ".hrl";
                 Ext == ".yrl" ->
            {State1, Mod0, ChangeSig} = sin_erl_info:process_file(State0,
                                                                  Path,
                                                                  Includes),
            Mod1 = add_stamp_info(State1, Path, Mod0, ChangeSig),
            {State1, Mod1};
        Error ->
            ?SIN_RAISE(State0, {unable_to_parse_file, Path, Error})
    end.

-spec add_stamp_info(sin_state:state(), string(), mod(), term()) -> mod().
add_stamp_info(State, Path, Mod, ChangeSig) ->
    Mod#module{changed=get_timestamp_info(State, Path),
               change_sig=ChangeSig}.

-spec get_timestamp_info(sin_state:state(), string()) -> non_neg_integer().
get_timestamp_info(State, Path) ->
    case file:read_file_info(Path) of
        {ok, FileInfo} ->
            FileInfo#file_info.mtime;
        {error, enoent} ->
            0;
        {error, Reason} ->
            ?SIN_RAISE(State, {unable_to_get_file_info, Path, Reason})
    end.

