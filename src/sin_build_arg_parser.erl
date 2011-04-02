%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Supports generating 'compile' args from a string using standard
%%%  erlc arguments.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%-------------------------------------------------------------------
-module(sin_build_arg_parser).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%% API
-export([compile_build_args/1,
	 format_exception/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Compile build args into terms the compiler understands.
-spec compile_build_args(string()) -> CompileOpts::term().
compile_build_args([]) ->
    [];
compile_build_args(ArgString) ->
    compile_build_args(ArgString, []).

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================

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
    ?SIN_RAISE(build_arg_error, "Invalid define");
compile_build_args([], Acc) ->
    Acc.

%% @doc eat space until you get a non space character.
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

%% @doc Parse out the define.
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
    ?SIN_RAISE(build_arg_error, "Invalid define");
parse_define([$= | T], LAcc, Acc) ->
    Key = list_to_atom(lists:reverse(LAcc)),
    {Value, NewT} = parse_define_value(T, []),
    compile_build_args(NewT, [{d, Key, Value} | Acc]);
parse_define([H | T], LAcc, Acc) ->
    parse_define(T, [H | LAcc], Acc);
parse_define([], [], _Acc) ->
    ?SIN_RAISE(build_arg_error, "Invalid define");
parse_define([], LAcc, Acc) ->
    [{d, list_to_atom(lists:reverse(LAcc))} | Acc].

%% @doc Parse an include directive out.
parse_define_value([$\" | T], _LAcc) ->
   {Dir, NewT, _} =  ktuo_parse_utils:stringish_body($\", T, [], 0, 0),
   {binary_to_list(Dir), NewT};
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
    ?SIN_RAISE(build_arg_error, "Unable to parse include");
parse_define_value([], LAcc) ->
    {lists:reverse(LAcc), []}.

%% @doc Parse an include directive out.
parse_include([$\" | T], _LAcc, Acc) ->
   {Dir, NewT, _} =  ktuo_parse_utils:stringish_body($\", T, [], 0, 0),
   compile_build_args(NewT, [{i, lists:reverse(binary_to_list(Dir))} | Acc]);
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
    ?SIN_RAISE(build_arg_error, "Unable to parse include");
parse_include([], LAcc, Acc) ->
    [{i, lists:reverse(LAcc)} | Acc].

%% @doc Parse a strait term.
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
    ?SIN_RAISE(build_arg_error, "Unable to parse build args");
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
