%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%% @end
%%%----------------------------------------------------------------------------
-module(sin_matcher, [Config,
                      Opts,
                      Task,
                      Release,
                      App,
                      Dir,
                      Module]).

%% API
-export([get/1,
         match/1,
         match/2,
         size/0,
         has_key/1,
         will_match/1,
         to_list/0,
         specialize/1]).

-include_lib("sinan/include/sinan.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc specialize an existing matcher on new opts
-spec specialize(sin_config:spec_opts()) -> module().
specialize(NewOpts) ->
    sin_config:create_matcher(NewOpts ++ Opts, Config).

%% @doc get a value from the key where.
-spec get(sin_config:full_key()) -> sin_config:value().
get(Key) ->
    sin_config:get({Key, Task, Release, App, Dir, Module}, Config).

%% @doc get a value from the key where.
-spec match(sin_config:partial_key()) -> sin_config:value().
match(Key) ->
    sin_config:'__match__'({Key, Task, Release, App, Dir, Module}, Config).

%% @doc get a value from the key where, if the value does not exist use the
%% specified default
-spec match(sin_config:partial_key(), sin_config:value()) -> sin_config:value().
match(Key, Default) ->
    sin_config:'__match__'({Key, Task, Release, App, Dir, Module},
                           Default, Config).

%% @doc get the current number of entries in the config
-spec size() -> non_neg_integer().
size() ->
    sin_config:size(Config).

%% @doc test to see if the key exists in the data base via an exact match
-spec has_key(sin_config:full_key()) -> boolean().
has_key(Key) ->
    sin_config:has_key({Key, Task, Release, App, Dir, Module}, Config).

%% @doc test to see if the key exists in the data base via an exact match
-spec will_match(sin_config:full_key()) -> boolean().
will_match(Key) ->
    sin_config:'__will_match__'({Key, Task, Release, App, Dir, Module},
                                Config).

to_list() ->
    sin_config:to_list(Config).

%%====================================================================
%% Internal Functions
%%====================================================================
