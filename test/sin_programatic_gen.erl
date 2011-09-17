%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Test the ability to generate a project programatically
%%% @end
%%% Created :  5 Sep 2011 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_programatic_gen).

-export([given/3, 'when'/3, then/3]).

% Step definitions for the sample calculator Addition feature.

given([an, empty, temp, directory, with, no, project], _State,
      _) ->
    {ok, BaseDir} = ewl_file:create_tmp_dir("/tmp"),
    {ok, BaseDir}.

'when'([gen, is, called, pragmatically, with, out,
        available, user, input], BaseDir, _) ->
    ProjectName = "super_foo",
    {ProjectDir, _} =
        sin_test_project_gen:single_app_project(BaseDir, ProjectName),
    {ok, {ProjectDir, ProjectName}};
'when'([a, build, is, run], State = {ProjectDir, _ProjectName}, _) ->
    sinan:main(["-s", ProjectDir, "build"]),
    {ok, State}.

then([sinan, should, build, the, project, normally],
     {ProjectDir, ProjectName}, _) ->
    sin_test_project_gen:validate_single_app_project(ProjectDir, ProjectName).
