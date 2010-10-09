%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Erlware
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
%%%  Supports generating 'compile' args from a string using standard
%%%  erlc arguments.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created :  5 Apr 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_build_arg_parser).

-include("eunit.hrl").

%% API
-export([compile_build_args/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Compile build args into terms the compiler understands.
%% @spec (String) -> CompileOpts
%% @end
%%--------------------------------------------------------------------
compile_build_args([]) ->
    [];
compile_build_args(ArgString) ->
    compile_build_args(ArgString, []).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% @spec (String, Acc) -> ok
%% @end
%%--------------------------------------------------------------------
compile_build_args([$+ | T], Acc) ->
    parse_term(T, [], Acc);
compile_build_args([$\ | T], Acc) ->
    compile_build_args(T, Acc);
compile_build_args([$\r | T], Acc) ->
    compile_build_args(T, Acc);

compile_build_args([$\f | T], Acc) ->
    compile_build_args(T,  Acc);
compile_build_args([$\n | T], Acc) ->
    compile_build_args(T,  Acc);
compile_build_args([$\t | T], Acc) ->
    compile_build_args(T, Acc);
compile_build_args([$-, $I | T], Acc) ->
    eat_space(T, Acc, fun parse_include/3);
compile_build_args([$-, $D | T], Acc) ->
    parse_define(T, [], Acc);
compile_build_args([$-, $W, $0 | T], Acc) ->
    compile_build_args(T, [{warn_format, 0} | Acc]);
compile_build_args([$-, $W, $1 | T], Acc) ->
    compile_build_args(T, [{warn_format, 1} | Acc]);
compile_build_args([$-, $W, $2 | T], Acc) ->
    compile_build_args(T, [{warn_format, 2} | Acc]);
compile_build_args([$-, $v | T], Acc) ->
    compile_build_args(T, [verbose | Acc]);
compile_build_args([$-, $s, $m, $p | T], Acc) ->
    compile_build_args(T,  Acc);
compile_build_args([_ | _], _Acc) ->
        throw({build_arg_error, "Invalid define"});
compile_build_args([], Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc
%%  eat space until you get a non space character.
%% @spec (Stream, Acc, Handler) -> Opts
%% @end
%%--------------------------------------------------------------------
eat_space([$\ | T], Acc, Handler) ->
    eat_space(T, Acc, Handler);
eat_space([$\r | T], Acc, Handler) ->
    eat_space(T, Acc, Handler);
eat_space([$\f | T], Acc, Handler) ->
    eat_space(T, Acc, Handler);
eat_space([$\n | T], Acc, Handler) ->
    eat_space(T, Acc, Handler);
eat_space([$\t | T], Acc, Handler) ->
    eat_space(T, Acc, Handler);
eat_space(Stream, Acc, Handler) ->
    Handler(Stream, [], Acc).


%%--------------------------------------------------------------------
%% @doc
%%  Parse out the define.
%% @spec (String, LAcc, Acc) -> ParseOpts
%% @end
%%--------------------------------------------------------------------
parse_define([$\ | T], LAcc, Acc) ->
    compile_build_args(T, [{d, list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define([$\r | T], LAcc, Acc) ->
    compile_build_args(T, [{d, list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define([$\f | T], LAcc, Acc) ->
    compile_build_args(T, [{d,list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define([$\n | T], LAcc, Acc) ->
    compile_build_args(T, [{d, list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define([$\t | T], LAcc, Acc) ->
    compile_build_args(T, [{d, list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define([$= | _], [], _Acc) ->
    throw({build_arg_error, "Invalid define"});
parse_define([$= | T], LAcc, Acc) ->
    Key = list_to_atom(lists:reverse(LAcc)),
    {Value, NewT} = parse_define_value(T, []),
    compile_build_args(NewT, [{d, Key, Value} | Acc]);
parse_define([H | T], LAcc, Acc) ->
    parse_define(T, [H | LAcc], Acc);
parse_define([], [], _Acc) ->
    throw({build_arg_error, "Invalid define"});
parse_define([], LAcc, Acc) ->
    [{d, list_to_atom(lists:reverse(LAcc))} | Acc].


%%--------------------------------------------------------------------
%% @doc
%%  Parse an include directive out.
%% @spec (String, Acc) -> ParsedOpts
%% @end
%%--------------------------------------------------------------------
parse_define_value([$\" | T], _LAcc) ->
   {Dir, NewT, _} =  ktuo_parse_utils:stringish_body($\", T, [], 0, 0),
   {Dir, NewT};
parse_define_value([$\ | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value([$\r | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value([$\f | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value([$\n | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value([$\t | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value([H | T], LAcc) ->
    parse_define_value(T, [H | LAcc]);
parse_define_value([], []) ->
    throw({build_arg_error, "Unable to parse include"});
parse_define_value([], LAcc) ->
    {lists:reverse(LAcc), []}.


%%--------------------------------------------------------------------
%% @doc
%%  Parse an include directive out.
%% @spec (String, LAcc, Acc) -> ParsedOpts
%% @end
%%--------------------------------------------------------------------
parse_include([$\" | T], _LAcc, Acc) ->
   {Dir, NewT, _} =  ktuo_parse_utils:stringish_body($\", T, [], 0, 0),
   compile_build_args(NewT, [{i, lists:reverse(Dir)} | Acc]);
parse_include([$\ | T], LAcc, Acc) ->
    compile_build_args(T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include([$\r | T], LAcc, Acc) ->
    compile_build_args(T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include([$\f | T], LAcc, Acc) ->
    compile_build_args(T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include([$\n | T], LAcc, Acc) ->
    compile_build_args(T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include([$\t | T], LAcc, Acc) ->
    compile_build_args(T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include([H | T], LAcc, Acc) ->
    parse_include(T, [H | LAcc], Acc);
parse_include([], [], _Acc) ->
    throw({build_arg_error, "Unable to parse include"});
parse_include([], LAcc, Acc) ->
    [{i, lists:reverse(LAcc)} | Acc].


%%--------------------------------------------------------------------
%% @doc
%%  Parse a strait term.
%% @spec (ParseString, LAcc, Acc) -> ParsedOpts
%% @end
%%--------------------------------------------------------------------
parse_term([$\ | T], LAcc, Acc) ->
    compile_build_args(T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term([$\r | T], LAcc, Acc) ->
    compile_build_args(T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term([$\f | T], LAcc, Acc) ->
    compile_build_args(T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term([$\n | T], LAcc, Acc) ->
    compile_build_args(T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term([$\t | T], LAcc, Acc) ->
    compile_build_args(T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term([H | T], LAcc, Acc) ->
    parse_term(T, [H | LAcc], Acc);
parse_term([], [], _Acc) ->
    throw({build_arg_error, "Unable to parse build args"});
parse_term([], LAcc, Acc) ->
    [list_to_atom(lists:reverse(LAcc)) | Acc].

%%====================================================================
%% Tests
%%====================================================================
term_parse_test() ->
    ?assertMatch([debug_info], compile_build_args("+debug_info")).


include_parse_test() ->
    ?assertMatch([{i, "/test/ax"}], compile_build_args("-I /test/ax")).


define_parse_test() ->
    ?assertMatch([{d, this_is_a_test}], compile_build_args("-Dthis_is_a_test")).

define_value_parse_test() ->
    ?assertMatch([{d, this_is_a_test, "this_a"}],
                 compile_build_args("-Dthis_is_a_test=this_a")).


multi_parse_test() ->
    ?assertMatch([{i,"xa/tset/"},{d,this_test},verbose,debug_info],
                 compile_build_args("+debug_info +verbose -Dthis_test -I \"/test/ax\"")).
