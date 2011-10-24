-module(sin_sinan_as_escript).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([sinan, has, nothing, in, its, priv, dir], State, _) ->
    %% The current directory should be the sinan project root
    ?assertMatch(false, sin_utils:file_exists(sin_state:new(), "./priv")),
    {ok, State};
given([a, project, generated, by, gen], _State, _) ->
    sin_test_project_gen:a_generated_project().

'when'([a,build,step,is,run,on,this,project],
    {ProjectDir, ProjectName}, _) ->
    Ret = sinan:main(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    {ok, {ProjectDir, ProjectName, Ret}}.

then([sinan,should,build,the,app,normally],
     {ProjectDir, ProjectName, Ret}, _) ->
    ?assertMatch({ok, _}, Ret),
    {ok, BuildState} = Ret,
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    sin_test_project_gen:validate_single_app_project(ProjectDir, ProjectName),
    {ok, {ProjectDir, ProjectName, BuildState}}.
