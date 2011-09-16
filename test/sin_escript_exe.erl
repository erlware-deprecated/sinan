-module(sin_escript_exe).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([a,generated,project], _State, _) ->
    {ok, BaseDir} = ewl_file:create_tmp_dir("/tmp"),
    ProjectName = "super_foo",
    {ProjectDir, _} =
        sin_test_project_gen:single_app_project(BaseDir, ProjectName),
    {ok, {ProjectDir, ProjectName}};
given([a, escript, file, that, should, become, the, base],
      State = {ProjectDir, ProjectName}, _) ->
    AppBin =filename:join([ProjectDir, "bin"]),
    ok = ewl_file:mkdir_p(AppBin),
    ?assertMatch(ok,
                 file:write_file(filename:join([AppBin, "test_escript"]),
                                 escript_contents(ProjectName, false))),
    {ok, State};
given([a, project, name, named, module, with, a, main, function],
      State = {ProjectDir, ProjectName}, _) ->
    AppSrc = filename:join([ProjectDir, "src"]),
    ?assertMatch(ok,
                 file:write_file(filename:join([AppSrc, "super_foo.erl"]),
                                 escript_contents(ProjectName, true))),
    {ok, State};
given([a, escript, directive, in, the, build, config],
      State = {ProjectDir, ProjectName}, _) ->
    ?assertMatch(ok,
                 file:write_file(filename:join(ProjectDir, "sinan.config"),
                                 escript_directive(ProjectName))),
    {ok, State}.



'when'([an, escript, step, is, run, on, this, project],
       {ProjectDir, ProjectName}, _) ->
    Ret = sinan:main(["-s", ProjectDir, "escript"]),
    ?assertMatch({_, _}, Ret),
    {ok, {ProjectDir, ProjectName, Ret}}.

then([produce, an, executable, escript],
     State = {_ProjectDir, ProjectName, Ret}, _) ->
    BuildDir = sin_state:get_value(build_dir, Ret),
    EscriptPath = filename:join([BuildDir, "escript", ProjectName]),
    ?assertMatch(true, sin_utils:file_exists(Ret, EscriptPath)),
    ?assertMatch("We are running!", os:cmd(EscriptPath)),
    {ok, State};
then([build, the, project, normally],
     {ProjectDir, ProjectName, Ret}, _) ->
    ?assertMatch({ok, _}, Ret),
    {ok, NewRet} = Ret,
    {ok, {ProjectDir, ProjectName, NewRet}}.

escript_contents(ProjectName, Module) ->
    Header = case Module of
                 true ->
                     ["-module(", ProjectName, ").\n",
                      "-export([main/1]).\n"];
                 false ->
                     ""
             end,
    lists:flatten([Header,
                   "main(_) -> \n"
                   "    io:format(\"We are running!\").\n"]).

escript_directive(ProjectName) ->
    lists:flatten(["{project_name,  ", ProjectName, "}.\n"
                   "{project_vsn, \"0.1.0\"}.\n"
                   "\n"
                   "{build_dir,  \"_build\"}.\n"
                   "\n"
                   "{ignore_dirs, [\"_\", \".\"]}.\n"
                   "\n"
                   "{ignore_apps, []}. \n"
                   "\n",
                   "{escript, [{source, \"bin/test_escript\"}]}.\n"]).


