%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
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
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%  Supports configuration aspects of the build system. Allows
%%%  applications to query aspects of the data.
%%% @end
%%% @copyright (C) 2007, Erlware
%%%----------------------------------------------------------------------------
-module(sin_parse_handle).

-include("eunit.hrl").

-export([parse_config_file/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec parse_config_file(BuildFile) -> ParsedConfig.
%%
%% @doc
%%  Read in the correct config file. Root specifies server root and
%%  env specifies the runtime environment.
%% @end
%% @private
%%--------------------------------------------------------------------
parse_config_file(BuildFile) ->
    case file:read_file(BuildFile) of
        {ok, FileBin} ->
            parse_config(binary_to_list(FileBin), 0, 0);
        Else ->
            Else
    end.


%%--------------------------------------------------------------------
%% @spec parse_config(Stream) -> ParsedConfig.
%%
%% @doc
%%  Parse the config  file into a usable format.
%% @end
%% @private
%%--------------------------------------------------------------------
parse_config([$\s | T], NewLines, Chars) ->
    parse_config(T, NewLines, Chars + 1);
parse_config([$\t | T], NewLines, Chars) ->
    parse_config(T, NewLines, Chars + 1);
parse_config([$\n | T], NewLines, _Chars) ->
    parse_config(T, NewLines + 1, 0);
parse_config([$\r | T], NewLines, _Chars) ->
    parse_config(T, NewLines + 1, 0);
parse_config(All = [${ | _], NewLines, Chars) ->
    case  ktuo_json:decode(All, NewLines, Chars) of
        Error = {error, _} ->
            Error;
        {Value, _, _} ->
            Value
    end;
parse_config(All, NewLines, Chars) ->
   case ktuo_json:decode([${ | All] ++ [$}], NewLines, Chars) of
        Error = {error, _} ->
            Error;
        {Value, _, _} ->
            Value
   end.



