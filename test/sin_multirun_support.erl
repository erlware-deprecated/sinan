%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Add the ability to run sinan multiple times concurrently
%%% @end
%%% Created :  5 Sep 2011 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_multirun_support).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").

%% Step definitions for the sample calculator Addition feature.

given([two, generated, projects], _, _) ->
    BaseDir = ec_file:mkdtemp(),
    ProjectDescs =
        lists:map(fun(Number) ->
                          ProjectName = "foobachoo" ++
                              erlang:integer_to_list(Number),
                          {ProjectDir, _} =
                              sin_test_project_gen:single_app_project(BaseDir,
                                                                      ProjectName),
                          {ProjectName, ProjectDir}
                  end,
                  lists:seq(0, 2)),

    {ok, ProjectDescs}.

'when'([a, build, step, is, run, on, each, project, concurrently],
       ProjectDescs, _) ->
    Results = ec_plists:map(fun({_, ProjectDir}) ->
                                   sinan:main(["-s", ProjectDir,
                                               "build"])
                           end, ProjectDescs),
    {ok, lists:zip(ProjectDescs, Results)}.

then([sinan, should, build, both, projects, without, a, problem],
     ProjectDescs, _) ->
    ?assertMatch(true,
                 lists:all(fun({{ProjectName, ProjectDir}, {ok, _}}) ->
                                   ok == sin_test_project_gen:validate_single_app_project(ProjectDir, ProjectName);
                              ({_, {error, _}}) ->
                                   false
                           end, ProjectDescs)),
    {ok, ProjectDescs}.
