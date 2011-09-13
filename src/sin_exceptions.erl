%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  A utility to help format sinan exceptions
%%% @end
%%% @copyright 2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_exceptions).

%% API
-export([format_exception/1]).
-export_type([reason/0, exception/0]).


%%====================================================================
%% Public Types
%%====================================================================

-type reason() :: {atom(), string()} | atom().
-type exception() :: {module(), non_neg_integer(), reason()}.

%%====================================================================
%% API
%%====================================================================

%% @doc a helper function to format sinan formated exceptions
-spec format_exception(exception()) -> string().
format_exception({pe, _, {Module, Line, {Reason, Description}}})
  when is_list(Reason) ->
    io_lib:format("~s:~p [~p] ~s", [Module, Line, Reason, Description]);
format_exception({pe, _, {Module, Line, Reason}}) ->
    io_lib:format("~s:~p [~p]", [Module, Line, Reason]).


