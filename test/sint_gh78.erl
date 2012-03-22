%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Test the ability to correctly correctly handle parse_transform
%%%  dependencies
%%% @end
%%%-------------------------------------------------------------------
-module(sint_gh78).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([a,generated,project], _State, _) ->
    sint_test_project_gen:a_generated_project();
given([that,project,an,includes,a,header,in,the,include,directory],
      State={ProjectDir, _}, _) ->
    io:format("~p~n", [ProjectDir]),
    File = filename:join([ProjectDir, "include",
                          "test.hrl"]),
    ok = file:write_file(File, header_contents()),
    {ok, State};
given([that,project,contains,an,erl,file,that,includes,that,header],
      State={ProjectDir, _}, _) ->
    AFile = filename:join([ProjectDir, "src", "sint_gh78_includer.erl"]),
    ok = file:write_file(AFile, dependent_contents()),
    {ok, State}.

'when'([a,build,step,is,run,on,this,project],
       {ProjectDir, ProjectName}, _) ->
    Ret = sinan:run_sinan(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    {_, TrueRet} = Ret,
    {ok, {ProjectDir, ProjectName, TrueRet}}.

then([build,the,app,normally],
     State = {_, _, BuildState}, _) ->
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    {ok, State}.

header_contents() ->
    "-define(FOO, ok).
".

dependent_contents() ->
    "-module(sint_gh78_includer).
-include(\"test.hrl\").
-export([new/2]).
new(_, _) -> ?FOO.
".
