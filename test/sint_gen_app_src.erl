%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Test the ability to generate an app.src
%%% @end
%%% Created :  5 Sep 2011 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sint_gen_app_src).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

% Step definitions for the sample calculator Addition feature.

given([an, empty, temp, directory, with, no, project], _State,
      _) ->
    BaseDir = ec_file:mkdtemp(),
    {ok, BaseDir}.

'when'([the, sinan, gen, task, is, called], BaseDir, _) ->
    ProjectName = "super_foo",
    {ProjectDir, _} =
        sint_test_project_gen:single_app_project(BaseDir, ProjectName),
    {ok, {ProjectDir, ProjectName}};
'when'([a, build, is, run],
       {ProjectDir, ProjectName}, _) ->
    Ret = sinan:run_sinan(["-s", ProjectDir, "build"]),
    {ok, {ProjectDir, ProjectName, Ret}}.


then([sinan, should, generate, an, 'app.src',
      into, the, src, directory],
     State = {ProjectDir, ProjectName, _}, _) ->
    Path = filename:join([ProjectDir, "src",
                          ProjectName ++ ".app.src"]),

    ?assertMatch(true,
                 sin_utils:file_exists(sin_state:new(), Path)),

    {ok, State};
then([build, the, project, normally],
    State = {ProjectDir, ProjectName, BuildState}, _) ->
    ?assertMatch({ok, _}, BuildState),
    sint_test_project_gen:validate_single_app_project(ProjectDir,
                                                      ProjectName),
    {ok, State}.
