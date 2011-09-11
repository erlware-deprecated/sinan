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
-export([compile_build_args/2,
         format_exception/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Compile build args into terms the compiler understands.
-spec compile_build_args(sin_config:config(), string()) -> CompileOpts::term().
compile_build_args(_, []) ->
    [];
compile_build_args(Config, ArgString) ->
    compile_build_args(Config, ArgString, []).

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================

compile_build_args(Config, [$+ | T], Acc) ->
    parse_term(Config, T, [], Acc);
compile_build_args(Config, [$\ | T], Acc) ->
    compile_build_args(Config, T, Acc);
compile_build_args(Config, [$\r | T], Acc) ->
    compile_build_args(Config, T, Acc);

compile_build_args(Config, [$\f | T], Acc) ->
    compile_build_args(Config, T,  Acc);
compile_build_args(Config, [$\n | T], Acc) ->
    compile_build_args(Config, T,  Acc);
compile_build_args(Config, [$\t | T], Acc) ->
    compile_build_args(Config, T, Acc);
compile_build_args(Config, [$-, $I | T], Acc) ->
    eat_space(Config, T, Acc, fun parse_include/4);
compile_build_args(Config, [$-, $D | T], Acc) ->
    parse_define(Config, T, [], Acc);
compile_build_args(Config, [$-, $W, $0 | T], Acc) ->
    compile_build_args(Config, T, [{warn_format, 0} | Acc]);
compile_build_args(Config, [$-, $W, $1 | T], Acc) ->
    compile_build_args(Config, T, [{warn_format, 1} | Acc]);
compile_build_args(Config, [$-, $W, $2 | T], Acc) ->
    compile_build_args(Config, T, [{warn_format, 2} | Acc]);
compile_build_args(Config, [$-, $v | T], Acc) ->
    compile_build_args(Config, T, [verbose | Acc]);
compile_build_args(Config, [$-, $s, $m, $p | T], Acc) ->
    compile_build_args(Config, T,  Acc);
compile_build_args(Config, [_ | _], _Acc) ->
    ?SIN_RAISE(Config, build_arg_error, "Invalid define");
compile_build_args(_, [], Acc) ->
    Acc.

%% @doc eat space until you get a non space character.
eat_space(Config, [$\ | T], Acc, Handler) ->
    eat_space(Config, T, Acc, Handler);
eat_space(Config, [$\r | T], Acc, Handler) ->
    eat_space(Config, T, Acc, Handler);
eat_space(Config, [$\f | T], Acc, Handler) ->
    eat_space(Config, T, Acc, Handler);
eat_space(Config, [$\n | T], Acc, Handler) ->
    eat_space(Config, T, Acc, Handler);
eat_space(Config, [$\t | T], Acc, Handler) ->
    eat_space(Config, T, Acc, Handler);
eat_space(Config, Stream, Acc, Handler) ->
    Handler(Config, Stream, [], Acc).

%% @doc Parse out the define.
parse_define(Config, [$\ | T], LAcc, Acc) ->
    compile_build_args(Config, T, [{d, list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define(Config, [$\r | T], LAcc, Acc) ->
    compile_build_args(Config,
                       T, [{d, list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define(Config, [$\f | T], LAcc, Acc) ->
    compile_build_args(Config,
                       T, [{d,list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define(Config, [$\n | T], LAcc, Acc) ->
    compile_build_args(Config,
                       T, [{d, list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define(Config, [$\t | T], LAcc, Acc) ->
    compile_build_args(Config,
                       T, [{d, list_to_atom(lists:reverse(LAcc))} | Acc]);
parse_define(Config, [$= | _], [], _Acc) ->
    ?SIN_RAISE(Config, build_arg_error, "Invalid define");
parse_define(Config, [$= | T], LAcc, Acc) ->
    Key = list_to_atom(lists:reverse(LAcc)),
    {Value, NewT} = parse_define_value(Config, T, []),
    compile_build_args(Config, NewT, [{d, Key, Value} | Acc]);
parse_define(Config, [H | T], LAcc, Acc) ->
    parse_define(Config, T, [H | LAcc], Acc);
parse_define(Config, [], [], _Acc) ->
    ?SIN_RAISE(Config, build_arg_error, "Invalid define");
parse_define(_Config, [], LAcc, Acc) ->
    [{d, list_to_atom(lists:reverse(LAcc))} | Acc].

%% @doc Parse an include directive out.
parse_define_value(_Config, [$\" | T], _LAcc) ->
   {Dir, NewT, _} =  ktuo_parse_utils:stringish_body($\", T, [], 0, 0),
   {binary_to_list(Dir), NewT};
parse_define_value(_Confif, [$\ | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value(_Config, [$\r | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value(_Config, [$\f | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value(_Config, [$\n | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value(_Config, [$\t | T], LAcc) ->
    {lists:reverse(LAcc), T};
parse_define_value(Config, [H | T], LAcc) ->
    parse_define_value(Config, T, [H | LAcc]);
parse_define_value(Config, [], []) ->
    ?SIN_RAISE(Config, build_arg_error, "Unable to parse include");
parse_define_value(_, [], LAcc) ->
    {lists:reverse(LAcc), []}.

%% @doc Parse an include directive out.
parse_include(Config, [$\" | T], _LAcc, Acc) ->
   {Dir, NewT, _} =  ktuo_parse_utils:stringish_body($\", T, [], 0, 0),
   compile_build_args(Config,
                      NewT, [{i, lists:reverse(binary_to_list(Dir))} | Acc]);
parse_include(Config, [$\ | T], LAcc, Acc) ->
    compile_build_args(Config,
                       T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include(Config, [$\r | T], LAcc, Acc) ->
    compile_build_args(Config,
                       T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include(Config, [$\f | T], LAcc, Acc) ->
    compile_build_args(Config, T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include(Config, [$\n | T], LAcc, Acc) ->
    compile_build_args(Config, T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include(Config, [$\t | T], LAcc, Acc) ->
    compile_build_args(Config, T, [{i, lists:reverse(LAcc)} | Acc]);
parse_include(Config, [H | T], LAcc, Acc) ->
    parse_include(Config, T, [H | LAcc], Acc);
parse_include(Config, [], [], _Acc) ->
    ?SIN_RAISE(Config, build_arg_error, "Unable to parse include");
parse_include(_Config, [], LAcc, Acc) ->
    [{i, lists:reverse(LAcc)} | Acc].

%% @doc Parse a strait term.
parse_term(Config, [$\ | T], LAcc, Acc) ->
    compile_build_args(Config, T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term(Config, [$\r | T], LAcc, Acc) ->
    compile_build_args(Config, T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term(Config, [$\f | T], LAcc, Acc) ->
    compile_build_args(Config, T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term(Config, [$\n | T], LAcc, Acc) ->
    compile_build_args(Config, T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term(Config, [$\t | T], LAcc, Acc) ->
    compile_build_args(Config, T, [list_to_atom(lists:reverse(LAcc)) | Acc]);
parse_term(Config, [H | T], LAcc, Acc) ->
    parse_term(Config, T, [H | LAcc], Acc);
parse_term(Config, [], [], _Acc) ->
    ?SIN_RAISE(Config, build_arg_error, "Unable to parse build args");
parse_term(_Config, [], LAcc, Acc) ->
    [list_to_atom(lists:reverse(LAcc)) | Acc].

%%====================================================================
%% Tests
%%====================================================================
term_parse_test() ->
    ?assertMatch([debug_info], compile_build_args(sin_config:new(),
                                                  "+debug_info")).


include_parse_test() ->
    ?assertMatch([{i, "/test/ax"}],
                 compile_build_args(sin_config:new(), "-I /test/ax")).


define_parse_test() ->
    ?assertMatch([{d, this_is_a_test}],
                 compile_build_args(sin_config:new(), "-Dthis_is_a_test")).

define_value_parse_test() ->
    ?assertMatch([{d, this_is_a_test, "this_a"}],
                 compile_build_args(sin_config:new(),
                                    "-Dthis_is_a_test=this_a")).


multi_parse_test() ->
    ?assertMatch([{i,"xa/tset/"},{d,this_test},verbose,debug_info],
                 compile_build_args(sin_config:new(),
                                    "+debug_info +verbose -Dthis_test -I \"/test/ax\"")).
