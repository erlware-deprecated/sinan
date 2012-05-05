%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Erlware, LLC
%%% @doc
%%%  A resolver used only for testing
%%% @end
%%% Created : 17 Sep 2011 by Eric Merritt <>
%%%-------------------------------------------------------------------
-module(sin_test_resolver).

-behaviour(sin_dep_resolver).

-export([new/2, app_dependencies/3, app_versions/2, resolve/3]).

new(Config, _State) ->
    Config.

app_dependencies(Config, App, Ver) ->
    {Config,Config:match({dependencies, App, Ver})}.

app_versions(Config, App) ->
    {Config, Config:match({versions, App})}.

%% Not used for testing
resolve(Config, _App, _Version) ->
    {Config, ""}.
