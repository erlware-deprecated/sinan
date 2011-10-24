-module(sint_module_dependencies).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").

given([a,module,the,depends,on,that,behaviour], State={ProjectDir,_}, _) ->
    File = filename:join([ProjectDir, "src",
                          "sin_module_dependencies_a_some_module.erl"]),
    ok = file:write_file(File, dependent_contents()),
    {ok, State};
given([an,erlang,project,that,contains,a,behaviour], _State, _) ->
  Result = {ok, {ProjectDir, _}} = sint_test_project_gen:a_generated_project(),
    File = filename:join([ProjectDir, "src",
                          "sin_module_dependencies_z_some_module.erl"]),
    ok = file:write_file(File, behaviour_contents()),
    Result.

'when'([a,build,step,is,run,on,the,project], {ProjectDir, ProjectName}, _) ->
    Ret = sinan:main(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    {ok, {ProjectDir, ProjectName, Ret}}.

then([sinan,should,build,the,project,correctly],
    {ProjectDir, ProjectName, Ret}, _) ->
    ?assertMatch({ok, _}, Ret),
    {ok, BuildState} = Ret,
    ?assertMatch([], sin_state:get_run_warnings(BuildState)),
    sint_test_project_gen:validate_single_app_project(ProjectDir, ProjectName),
    {ok, {ProjectDir, ProjectName, BuildState}}.


behaviour_contents() ->
    "-module(sin_module_dependencies_z_some_module).
-export([behaviour_info/1]).
%% @doc define the behaviour for tasks.
behaviour_info(callbacks) ->
    [{new, 2}];
behaviour_info(_) ->
    undefined.
".

dependent_contents() ->
    "-module(sin_module_dependencies_a_some_module).
-behaviour(sin_module_dependencies_z_some_module).
-export([new/2]).
new(_, _) -> ok.
".

