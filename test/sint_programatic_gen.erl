%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Test the ability to generate a project programatically
%%% @end
%%% Created :  5 Sep 2011 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sint_programatic_gen).

-export([given/3, 'when'/3, then/3]).

% Step definitions for the sample calculator Addition feature.

given([an, empty, temp, directory, with, no, project], _State,
      _) ->
    BaseDir = ec_file:mkdtemp(),
    {ok, BaseDir}.

'when'([gen, is, called, pragmatically, with, out,
        available, user, input], BaseDir, _) ->
    ProjectName = "super_foo",
    {ProjectDir, _} =
        sint_test_project_gen:single_app_project(BaseDir, ProjectName),
    {ok, {ProjectDir, ProjectName}};
'when'([a, build, is, run], State = {ProjectDir, _ProjectName}, _) ->
    sinan:run_sinan(["-s", ProjectDir, "build"]),
    {ok, State}.

then([sinan, should, build, the, project, normally],
     {ProjectDir, ProjectName}, _) ->
    sint_test_project_gen:validate_single_app_project(ProjectDir, ProjectName).
