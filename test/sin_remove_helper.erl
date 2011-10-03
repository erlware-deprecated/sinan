-module(sin_remove_helper).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([an, empty, directory], _State, _) ->
    BaseDir = ec_file:mkdtemp(),
    {ok, BaseDir}.

'when'([a, project, is, generated], BaseDir, _) ->
    ProjectName = "super_foo",
    {ProjectDir, _} =
        sin_test_project_gen:single_app_project(BaseDir, ProjectName),
    {ok, {ProjectDir, ProjectName}}.

then([should, 'not', generate, the, exe, script],
     State = {ProjectDir, _}, _) ->
    HelperFile = filename:join([ProjectDir, "bin",
                               "erlware_release_start_helper"]),
    ?assertMatch(false,
                 sin_utils:file_exists(sin_state:new(), HelperFile)),
    {ok, State};
then([sinan, should, 'not', generate, the, erlware,helper],
     State = {ProjectDir, ProjectName}, _) ->
    BinFile = filename:join([ProjectDir, "bin",
                             ProjectName]),
    ?assertMatch(false,
                 sin_utils:file_exists(sin_state:new(), BinFile)),
    {ok, State}.

