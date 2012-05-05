%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Test the ability to specify a start directory
%%% @end
%%% Created :  5 Sep 2011 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sint_testability).

-export([given/3, 'when'/3, then/3]).

% Step definitions for the sample calculator Addition feature.

given([a, generated, project, in, a, different, location, then, the, cwd],
      _, _) ->
    sint_test_project_gen:a_generated_project().

'when'([a, build, step, is, run, on, this, project],
       Ret = {ProjectDir, _}, _) ->
    sinan:run_sinan(["-s", ProjectDir, "build"]),
    {ok, Ret};
'when'([a, start, dir, is, passed, to, the, build], Ret, _) ->
    %% Nothing really to be done here since the start dir is passed when
    %% The build is run.
    {ok, Ret}.

then([sinan, should, build, the, project, in, the, location,
      specified, by, the, start, dir], {ProjectDir, ProjectName}, _) ->
    sint_test_project_gen:validate_single_app_project(ProjectDir, ProjectName),
    ok.
