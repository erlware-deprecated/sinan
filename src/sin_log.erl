%% -*- mode: Erlang; fill-column: 79; comment-column: 70; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Provides log functionality to the system so that things that need to be
%%%  loged can be logged get a response at the correct verbosity level
%%% @end
%%% @copyright Erlware 2006-2011
%%%---------------------------------------------------------------------------
-module(sin_log).

%% API
-export([verbose/2,
         verbose/3,
         normal/2,
         normal/3]).


%%============================================================================
%% API
%%============================================================================

%% @doc Outputs the line to the screen
-spec verbose(sin_config:config(), string()) -> ok.
verbose(Config, Say) ->
    case Config:match(verbose, false) of
        true ->
            ec_talk:say(Say);
        false ->
            ok
    end.

-spec verbose(sin_config:config(), string(), [term()] | term()) -> ok.
verbose(Config, Say, Args) ->
    case Config:match(verbose, false) of
        true ->
            ec_talk:say(Say, Args);
        false ->
            ok
    end.

%% @doc Outputs the line to the screen
-spec normal(sin_config:config(), string()) -> ok.
normal(_Config, Say) ->
    ec_talk:say(Say).

-spec normal(sin_config:config(), string(), [term()] | term()) -> ok.
normal(_Config, Say, Args) ->
    ec_talk:say(Say, Args).



