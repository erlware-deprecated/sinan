%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Test the ability to correctly correctly handle parse_transform
%%%  dependencies
%%% @end
%%%-------------------------------------------------------------------
-module(sint_gh50).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([a,generated,project], _State, _) ->
    sin_test_project_gen:a_generated_project();
given([that,project,contains,a,parse,transform], State={ProjectDir, _}, _) ->
    File = filename:join([ProjectDir, "src", "sint_gh50_z_some_module.erl"]),
    ok = file:write_file(File, transform_contents()),
    {ok, State};
given([that,project,contains,a,module,that,
       depends,on,that,parse,transform], State={ProjectDir, _}, _) ->
    AFile = filename:join([ProjectDir, "src", "sint_gh50_a_some_module.erl"]),
    IncludeFile = filename:join([ProjectDir, "include", "sint_gh50_some_include.hrl"]),
    ok = file:write_file(AFile, dependent_contents()),
    ok = file:write_file(IncludeFile, include_contents()),
    {ok, State}.

'when'([a,build,step,is,run,on,this,project],
       {ProjectDir, ProjectName}, _) ->
    Ret = sinan:main(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    {_, TrueRet} = Ret,
    {ok, {ProjectDir, ProjectName, TrueRet}}.

then([build,the,app,normally],
     State = {_, _, BuildState}, _) ->
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    {ok, State}.

transform_contents() ->
    "-module(sint_gh50_z_some_module).
-export([parse_transform/2]).
parse_transform(Forms, _Options) ->
    Forms.
".

include_contents() ->
"-compile({parse_transform, sint_gh50_z_some_module}).".

dependent_contents() ->
    "-module(sint_gh50_a_some_module).
-include_lib(\"super_foo/include/sint_gh50_some_include.hrl\").
-export([new/2]).
new(_, _) -> ok.
".
