%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Add the ability to test cucumber runs from sinan
%%% @end
%%% Created :  5 Sep 2011 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_cucumber_support).

-export([given/3, 'when'/3, then/3]).

-include_lib("eunit/include/eunit.hrl").

%% Step definitions for the sample calculator Addition feature.

given([a, generated, project], _, _) ->
    {ok, BaseDir} = ewl_file:create_tmp_dir("/tmp"),
    ProjectName = "foobachoo",
    {ProjectDir, _} =
        sin_test_project_gen:single_app_project(BaseDir, ProjectName),
    {ok, {ProjectDir, ProjectName}};
given([a, feature, file, in, the, features, directory, 'of',
       that, project], {ProjectDir, ProjectName}, _) ->
    ?assertMatch(ok,
                 ewl_file:mkdir_p(filename:join([ProjectDir, "features"]))),
    FeatureName = "test_feature",
    FeaturePath = filename:join([ProjectDir, "features", FeatureName ++
                                     ".feature"]),
    file:write_file(FeaturePath, feature()),
    {ok, {ProjectDir, ProjectName, FeatureName, FeaturePath}};
given([an, implementation, 'of', that, feature, that, passes],
      {ProjectDir, ProjectName, FeatureName, FeaturePath}, _) ->
    write_impl(pass_feature_impl(), ProjectDir, FeatureName),
    {ok, {ProjectDir, ProjectName, FeaturePath}};
given([an, implementation, 'of', that, feature, that, fails],
      {ProjectDir, ProjectName, FeatureName, FeaturePath}, _) ->
    write_impl(fail_feature_impl(), ProjectDir, FeatureName),
    {ok, {ProjectDir, ProjectName, FeaturePath}}.

'when'([a, cucumber, step, is, run, on, this, project],
       {ProjectDir, ProjectName, FeaturePath}, _) ->
    Result = sinan:run_sinan(["-s", ProjectDir, "cucumber"]),
    sin_test_project_gen:validate_single_app_project(ProjectDir, ProjectName),
    {ok, {FeaturePath, Result}}.

then([then, sinan, should, run, cucumberl, on,
      the, features, in, the, features, directory],
     State = {FeaturePath, {_, BuildResult}}, _) ->
    ?assertMatch([FeaturePath],
                 sin_config:get_value(BuildResult, "cucumber.features")),
    {ok, State};
then([report, the, build, as, failing],
     State = {_, BuildResult}, _) ->
    ?assertMatch({error, _}, BuildResult),
    {ok, State};
then([report, the, build, as, passing],
     State = {_, BuildResult}, _) ->
    ?assertMatch({ok, _}, BuildResult),
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

write_impl(Impl, ProjectDir, FeatureName) ->
    ?assertMatch(ok,
                 ewl_file:mkdir_p(filename:join([ProjectDir, "test"]))),
    FeatureName = "test_feature",
    FeatureImplPath = filename:join([ProjectDir, "test", FeatureName ++
                                         ".erl"]),
    file:write_file(FeatureImplPath, Impl),
    FeatureImplPath.

feature() ->
    "Feature: Addition\n"
        "  In order to avoid silly mistakes\n"
        "  As a math idiot\n"
        "  I want to be told the sum of two numbers\n"
        "\n"
        "  Scenario: Add two numbers\n"
        "    Given I have entered 50 into the calculator\n"
        "    And I have entered 70 into the calculator\n"
        "    When I press add\n"
        "    Then the result should be 120 on the screen\n".

pass_feature_impl() ->
    "-module(test_feature).\n"
        "\n"
        "-export([setup/0, teardown/1,\n"
        "         given/3, 'when'/3, then/3]).\n"
        "\n"
        "-export([enter/2, press/2]).\n"
        "\n"
        "setup() ->\n"
        "    [].\n"
        "\n"
        "%% Step definitions for the sample calculator Addition feature.\n"
        "given([i, have, entered, N, into, the, calculator], State, _) ->\n"
        "    {ok, enter(State, list_to_integer(atom_to_list(N)))}.\n"
        "\n"
        "'when'([i, press, Op], State, _) ->\n"
        "    {ok, press(State, Op)}.\n"
        "\n"
        "then([the, result, should, be, Result, on, the, screen],\n"
        "     State, _) ->\n"
        "    list_to_integer(atom_to_list(Result)) =:=State.\n"
        "\n"
        "teardown(_State) ->\n"
        "    ok.\n"
        "\n"
        "%% Implementing a simple model here...\n"
        "\n"
        "enter(State, N) ->\n"
        "    [N|State].\n"
        "\n"
        "press(State, add) ->\n"
        "    add(State);\n"
        "press(State, multiply) ->\n"
        "    multiply(State).\n"
        "\n"
        "add([X, Y]) ->\n"
        "    X + Y.\n"
        "\n"
        "multiply([X, Y]) ->\n"
        "    X * Y.\n".

fail_feature_impl() ->
    "-module(test_feature).\n"
        "\n"
        "-export([setup/0, teardown/1,\n"
        "         given/3, 'when'/3, then/3]).\n"
        "\n"
        "-export([enter/2, press/2]).\n"
        "\n"
        "setup() ->\n"
        "    [].\n"
        "\n"
        "%% Step definitions for the sample calculator Addition feature.\n"
        "given([i, have, entered, N, into, the, calculator], State, _) ->\n"
        "    {ok, enter(State, list_to_integer(atom_to_list(N)))}.\n"
        "\n"
        "'when'([i, press, Op], State, _) ->\n"
        "    {ok, press(State, Op)}.\n"
        "\n"
        "then([the, result, should, be, Result, on, the, screen],\n"
        "     State, _) ->\n"
        "    list_to_integer(atom_to_list(Result)) =:=State.\n"
        "\n"
        "teardown(_State) ->\n"
        "    ok.\n"
        "\n"
        "%% Implementing a simple model here...\n"
        "\n"
        "enter(State, N) ->\n"
        "    [N|State].\n"
        "\n"
        "press(State, add) ->\n"
        "    add(State);\n"
        "press(State, multiply) ->\n"
        "    multiply(State).\n"
        "\n"
        "add([X, Y]) ->\n"
        "    X + Y + 22.\n"
        "\n"
        "multiply([X, Y]) ->\n"
        "    X * Y.\n".
