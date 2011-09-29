-module(sin_configurable_dependencies).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([a, generated, project, that, contains, a, dependency, spec], _State, _) ->
    {ok, BaseDir} = ewl_file:create_tmp_dir("/tmp"),
    {ok, DepBuildDir} = ewl_file:create_tmp_dir("/tmp"),
    {ok, DepDir} = ewl_file:create_tmp_dir("/tmp"),
    ProjectName = "super_foo",
    generate_dependencies(DepBuildDir, DepDir,
                          ["futz", "fitz", "footz"],
                          ["0.1.0", "0.2.0", "0.3.0", "0.4.0"]),
    {ProjectDir, _} =
        sin_test_project_gen:single_app_project(BaseDir, ProjectName, "0.1.1"),
    AppSrc = filename:join([ProjectDir, "src", ProjectName ++ ".app.src"]),
    ?assertMatch(ok, file:write_file(AppSrc,
                                     app_contents(ProjectName, "0.1.1"))),
    Config = filename:join([ProjectDir, "sinan.config"]),
    ?assertMatch(ok, file:write_file(Config,
                                     config_contents(ProjectName, "0.1.0", DepDir))),
    {ok, {ProjectDir, ProjectName, DepDir}};
given([has, multiple, releases], {ProjectDir, ProjectName, DepDir}, _) ->
    Config = filename:join([ProjectDir, "sinan.config"]),
    ?assertMatch(ok, file:write_file(Config,
                                     config_multi_contents(ProjectName, "0.1.0", DepDir))),
    {ok, {ProjectDir, ProjectName, DepDir}}.

'when'([a, build, step, is, run, on, this, project, on, each, release],
       {ProjectDir, ProjectName, _}, _) ->
    Results =
        lists:map(fun(RelName) ->
                          Ret = sinan:main(["-s", ProjectDir, "-r", RelName, "release"]),
                          ?assertMatch({ok, _}, Ret),
                          {_, TrueRet} = Ret,
                          TrueRet
                  end, ["one", "two", "three"]),
    {ok, {ProjectDir, ProjectName, Results}};
'when'([a, build, step, is, run, on, this, project],
       {ProjectDir, ProjectName, _}, _) ->
    Ret = sinan:main(["-s", ProjectDir, "release"]),
    ?assertMatch({ok, _}, Ret),
    {_, TrueRet} = Ret,
    {ok, {ProjectDir, ProjectName, TrueRet}}.

then([sinan, builds, the, app, normally], State = {_, _, BuildState}, _) ->
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    {ok, State};
then([sinan, builds, the, app, normally, each, time],
      State = {_, _, BuildStates}, _) ->
    lists:foreach(fun(BuildState) ->
                          ?assertMatch([], sin_state:get_run_errors(BuildState))
                  end, BuildStates),
    {ok, State};
then([correctly, figures, out, the, constrained, dependencies],
     State = {ProjectDir, ProjectName, _}, _) ->
    ReleaseFile = filename:join([ProjectDir, "_build", ProjectName,
                                 releases, ProjectName ++ "-0.1.0",
                                 ProjectName ++ ".rel"]),
    {ok, [{release, {ProjectName, _}, _,
           AppVsns}]} = file:consult(ReleaseFile),

    ?assertMatch({futz, "0.3.0", load}, lists:keyfind(futz, 1, AppVsns)),
    ?assertMatch({fitz, "0.2.0", load}, lists:keyfind(fitz, 1, AppVsns)),
    ?assertMatch({footz, "0.2.0", load}, lists:keyfind(footz, 1, AppVsns)),
    {ok, State};
then([correctly, figures, out, the, constrained, dependencies, for, each, release],
    State = {ProjectDir, _ProjectName, _}, _) ->
    lists:foreach(fun({Name, Vsn}) ->
                          ReleaseFile = filename:join([ProjectDir, "_build", Name,
                                                       releases, Name ++ "-0.1.0",
                                                       Name ++ ".rel"]),
                          {ok, [{release, {Name, _}, _,
                                 AppVsns}]} = file:consult(ReleaseFile),

                          ?assertMatch({futz, Vsn, _}, lists:keyfind(futz, 1, AppVsns)),
                          ?assertMatch({fitz, Vsn, _}, lists:keyfind(fitz, 1, AppVsns)),
                          ?assertMatch({footz, Vsn, _}, lists:keyfind(footz, 1, AppVsns))
                  end, [{"one", "0.1.0"},
                        {"two", "0.2.0"},
                        {"three", "0.3.0"}]),
    {ok, State}.

generate_dependencies(BaseDir, DepDir, Apps, Versions) ->
    lists:foreach(fun(App) ->
                          lists:foreach(fun(Vsn) ->
                                                generate_dependency(BaseDir,
                                                                    DepDir, App, Vsn)
                                        end, Versions)
                  end, Apps).

generate_dependency(BaseDir, DepDir, App, Vsn) ->
    {ProjectDir, _} =
        sin_test_project_gen:single_app_project(BaseDir, App, Vsn),
    ?assertMatch({ok, _}, sinan:main(["-s", ProjectDir, "build"])),
    DepApp = filename:join([ProjectDir, "_build", App, "apps", App ++ "-" ++ Vsn]),
    DepTarget = filename:join(DepDir, App ++ "-" ++ Vsn),
    ewl_file:mkdir_p(DepTarget),
    sin_utils:copy_dir(sin_state:new(), DepTarget, DepApp),
    sin_utils:delete_dir(ProjectDir).

app_contents(ProjectName, Vsn) ->
    ["{application,", ProjectName,",\n",
     "[{description, \"Your Desc HERE\"},\n"
     "{vsn, \"", Vsn, "\"},\n",
     "{modules, [", ProjectName, "_app,\n",
     ProjectName, "_sup]},\n",
     "{registered,[", ProjectName, "_sup]},\n"
     "{applications, [kernel, stdlib, futz, fitz, footz]},\n"
     "{mod, {", ProjectName, "_app,[]}},\n"
     "{start_phases, []}]}.\n"].

config_contents(ProjectName, Vsn, DepDir) ->
    ["{project_name, ", ProjectName, "}.\n"
     "{project_vsn, \"", Vsn, "\"}.\n",
     "{dep_dirs, [\"", DepDir, "\"]}.\n",
     "{dep_constraints, [{futz, \"0.1.0\", \"0.3.0\", between},\n",
     "{fitz, \"0.3.0\", lt}, \n",
     "{footz, \"0.2.0\"}]}."].

config_multi_contents(ProjectName, Vsn, DepDir) ->
    ["{project_name, ", ProjectName, "}.\n"
     "{project_vsn, \"", Vsn, "\"}.\n",
     "{dep_dirs, [\"", DepDir, "\"]}.\n",
     "{{dep_constraints, [{release, one}]}, \n"
     "[{futz, \"0.1.0\"},\n",
     "{fitz, \"0.1.0\"}, \n",
     "{footz, \"0.1.0\"}]}.\n"
     "{{dep_constraints, [{release, two}]}, \n"
     "[{futz, \"0.2.0\"},\n",
     "{fitz, \"0.2.0\"}, \n",
     "{footz, \"0.2.0\"}]}.\n"
     "{{dep_constraints, [{release, three}]}, \n"
     "[{futz, \"0.3.0\"},\n",
     "{fitz, \"0.3.0\"}, \n",
     "{footz, \"0.3.0\"}]}.\n"].
