-module(sin_sig_save).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([a,generated,project], _State, _) ->
    sin_test_project_gen:a_generated_project().

'when'([a,build,step,is,run,on,this,project],
       {ProjectDir, ProjectName}, _) ->
    Ret = sinan:main(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    {ok, {ProjectDir, ProjectName, Ret}}.

then([sinan, should, build, the, app, normally],
     {ProjectDir, ProjectName, Ret}, _) ->
    ?assertMatch({ok, _}, Ret),
    {ok, BuildState} = Ret,
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    {ok, {ProjectDir, ProjectName, BuildState}};
then(['not',save, intermediate, state], State, _) ->
    %% There isnt really any way to test this
    {ok, State};
then([save,state,to,a,single,file,on,close],
     State = {_, _, BuildState}, _) ->
    %% We can check for a .sig file in the build dir
    BuildDir = sin_state:get_value(build_dir, BuildState),
    SigFile = filename:join(BuildDir, ".sig"),
    ?assertMatch(true, sin_utils:file_exists(sin_state:new(), SigFile)),
    {ok, State}.

