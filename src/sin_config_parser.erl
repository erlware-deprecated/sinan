%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Parses a config file returning data formatted in config compatible form.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%----------------------------------------------------------------------------
-module(sin_config_parser).

-include("internal.hrl").

-export([parse_config_file/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Read in the correct config file. Root specifies server root and env
%% specifies the runtime environment.
-spec parse_config_file(BuildFile::string()) -> ParsedConfig::term().
parse_config_file(BuildFile) ->
    case file:read_file(BuildFile) of
        {ok, FileBin} ->
            parse_config(binary_to_list(FileBin), 0, 0);
        Else ->
            Else
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Parse the config file into a usable format.
-spec parse_config(Stream::string(),
		   Newlines::integer(),
		   Chars::integer()) -> ParsedConfig::term().
parse_config([$\s | T], NewLines, Chars) ->
    parse_config(T, NewLines, Chars + 1);
parse_config([$\t | T], NewLines, Chars) ->
    parse_config(T, NewLines, Chars + 1);
parse_config([$\n | T], NewLines, _Chars) ->
    parse_config(T, NewLines + 1, 0);
parse_config([$\r | T], NewLines, _Chars) ->
    parse_config(T, NewLines + 1, 0);
parse_config(All = [${ | _], NewLines, Chars) ->
    case  ktj_decode:decode(All, NewLines, Chars) of
        Error = {error, _} ->
            Error;
        {Value, _, _} ->
            Value
    end;
parse_config(All, NewLines, Chars) ->
   case ktj_parse:parse([${ | All] ++ [$}], NewLines, Chars) of
        Error = {error, _} ->
            Error;
        {Value, _, _} ->
            Value
   end.



