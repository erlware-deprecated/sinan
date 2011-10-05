-module(sin_app_src_mod_list).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([a,generated,project,that,contains,an,'ebin/app'], _State, _) ->
    BaseDir = ec_file:mkdtemp(),
    ProjectName = "super_foo",
    {ProjectDir, _} =
        sin_test_project_gen:single_app_project(BaseDir, ProjectName),
    AppEbin = filename:join([ProjectDir, "ebin", ProjectName ++ ".app"]),
    AppSrc = filename:join([ProjectDir, "src", ProjectName ++ ".app.src"]),
    ?assertMatch(ok, file:write_file(AppEbin,
                                     sin_cuke_support_funs:app_src(ProjectName))),
    sin_cuke_support_funs:delete_if_exists(AppSrc),
    {ok, {ProjectDir, ProjectName}};
given([a,generated,project,that,contains,an,'app.src'], _State, _) ->
    BaseDir = ec_file:mkdtemp(),
    ProjectName = "super_foo",
    {ProjectDir, _} =
        sin_test_project_gen:single_app_project(BaseDir, ProjectName),
    {ok, {ProjectDir, ProjectName}}.

'when'([a,build,step,is,run,on,this,project],
       {ProjectDir, ProjectName}, _) ->
    Ret = sinan:main(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    {ok, {ProjectDir, ProjectName, Ret}}.

then([sinan, should, put, the, populate, the, module, list],
     {ProjectDir, ProjectName, Ret}, _) ->
    BuildEbin = filename:join([ProjectDir,
                               "_build", ProjectName,
                               "lib", ProjectName ++ "-0.1.0",
                               "ebin", ProjectName ++ ".app"]),
    Result = file:consult(BuildEbin),
    ?assertMatch({ok, [{application, _, _}]}, Result),
    {ok, [{application, _, Details}]} = Result,
    ModuleEntry = lists:keyfind(modules, 1, Details),
    ?assertMatch({modules, _}, ModuleEntry),
    {modules, ModuleList} = ModuleEntry,
    ?assertMatch(2, erlang:length(ModuleList)),
    ?assertMatch(true,
                 lists:member(erlang:list_to_atom(ProjectName ++ "_app"),
                             ModuleList)),
    ?assertMatch(true,
                 lists:member(erlang:list_to_atom(ProjectName ++ "_sup"),
                              ModuleList)),

    {ok, {ProjectDir, ProjectName, Ret}};

then([build, the, app, normally],
     State = {_, _, Ret}, _) ->
    ?assertMatch({ok, _}, Ret),
    {ok, BuildState} = Ret,
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    {ok, State}.
