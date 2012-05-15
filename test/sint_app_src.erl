-module(sint_app_src).

-include_lib("eunit/include/eunit.hrl").
-include_lib("sinan/include/sinan.hrl").

-export([given/3, 'when'/3, then/3]).

given([a, generated, project, that, contains, an, 'ebin/app'],
      _State, _) ->
    Result = {ok, {ProjectDir, ProjectName}} = sint_test_project_gen:a_generated_project(),
    AppEbin = filename:join([ProjectDir, "ebin", ProjectName ++ ".app"]),
    ?assertMatch(ok, file:write_file(AppEbin,
                                     sint_cuke_support_funs:app_src(ProjectName))),
    Result;
given([a,generated,project,that,contains,an,'app.src'], _State, _) ->
    BaseDir = ec_file:mkdtemp(),
    ProjectName = "super_foo",
    {ProjectDir, _} =
        sint_test_project_gen:single_app_project(BaseDir, ProjectName),
    AppSrcPath = filename:join([ProjectDir, "src",
                                ProjectName ++ ".app.src"]),
    ?assertMatch(ok, file:write_file(AppSrcPath,
                                     sint_cuke_support_funs:app_src(ProjectName))),
    {ok, {ProjectDir, ProjectName}};
given([does,'not',contain,an,'ebin/app'],
      State = {ProjectDir, ProjectName}, _) ->
    AppSrc = filename:join([ProjectDir, "ebin", ProjectName ++ ".app"]),
    ?assertMatch(ok, sint_cuke_support_funs:delete_if_exists(AppSrc)),
    {ok, State};
given([does,'not',contain,an,'app.src'],
      State = {ProjectDir, ProjectName}, _) ->
    AppSrc = filename:join([ProjectDir, "src", ProjectName ++ ".app.src"]),
    ?assertMatch(ok, sint_cuke_support_funs:delete_if_exists(AppSrc)),
    {ok, State}.

'when'([a, build, step, is, run, on, this, project],
       {ProjectDir, ProjectName}, _) ->
    Ret = sinan:run_sinan(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    {_, TrueRet} = Ret,
    {ok, {ProjectDir, ProjectName, TrueRet}}.

then([build, the, app, normally], State = {_, _, BuildState}, _) ->
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    {ok, State};
then([sinan, should, put, the, app, file, in, 'ebin/.app'],
     State = {_, ProjectName, BuildState}, _) ->
    verify_ebin_app(ProjectName, BuildState),
    {ok, State};
then([warn, the, user, that, the,
      'ebin/app', is, being, ignored],
     State = {_, _, BuildState}, _) ->
    Warnings = sin_state:get_run_warnings(BuildState),
    ?assertMatch(true,
                 lists:any(fun({sin_discover, Warning}) ->
                                   case Warning of
                                       "Unexpected ebin/.app overriding with src/app.src" ->
                                           true;
                                       _ ->
                                           false
                                   end;
                              (_) ->
                                   false
                           end, Warnings)),
                 {ok, State}.

verify_ebin_app(ProjectName, BuildState) ->
    App = sin_state:project_app_by_name(erlang:list_to_atom(ProjectName),
                                        BuildState),

    BasePath = filename:join([App#app.path, "ebin", ProjectName ++
                                  ".app"]),
    ?assertMatch(true,
                 sin_utils:file_exists(sin_state:new(), BasePath)),
    AppContents = file:consult(BasePath),
    AtomName =  erlang:list_to_atom(ProjectName),
    ?assertMatch({ok, [{application, AtomName, _}]}, AppContents),
    {ok, [{_, _, Details}]} = AppContents,
    ?assertMatch({vsn, "0.1.0"}, lists:keyfind(vsn, 1, Details)),
    Details.
