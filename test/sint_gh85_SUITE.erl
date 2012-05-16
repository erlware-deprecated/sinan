%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Erlware, LLC.
%%%-------------------------------------------------------------------
-module(sint_gh85_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/1,
         all/0,
         normal_vsn/1,
         v_git_vsn/1,
         git_vsn/1]).

%%====================================================================
%% TEST SERVER CALLBACK FUNCTIONS
%%====================================================================
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all(doc) ->
    ["Testing the fix for github issue 85 : git based app versions "
     "(https://github.com/erlware/sinan/issues/85)"].

all() ->
    [normal_vsn, git_vsn, v_git_vsn].

%%====================================================================
%% TEST CASES
%%====================================================================

normal_vsn(doc) ->
    ["Create project under git with a normal version"];
normal_vsn(suite) ->
    [];
normal_vsn(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ProjectName = "foo",
    {ProjectDir, _} =
        sint_test_project_gen:single_app_project(PrivDir, ProjectName, "1.0.0"),
    Cmd = lists:flatten(io_lib:format("git init ~s", [ProjectDir])),
    {ok, _} = sin_sh:sh(sin_config:new(), Cmd, []),
    Ret = sinan:run_sinan(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    EbinPath = filename:join([ProjectDir, "_build", ProjectName, "lib",
                              ProjectName ++ "-" ++ "1.0.0", "ebin",
                              ProjectName ++ ".app"]),
    Result = file:consult(EbinPath),
    ?assertMatch({ok, [{application, _, _}]}, Result),
    {ok, [{application, _, Details}]} = Result,
    ?assertMatch("1.0.0", proplists:get_value(vsn, Details)).

git_vsn(doc) ->
    ["Create project with a git based vsn"];
git_vsn(suite) ->
    [];
git_vsn(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ProjectName = "foo_bar",
    {ProjectDir, _} =
        sint_test_project_gen:single_app_project(PrivDir, ProjectName),
    ?assertMatch(ok,
                 file:write_file(filename:join([ProjectDir, "src",
                                                ProjectName ++ ".app.src"]),
                                 app_src(ProjectName,
                                         "git"))),
    {ok, _} = sin_sh:sh(sin_config:new(), "git init", [{cd, ProjectDir}]),
    {ok, _} = sin_sh:sh(sin_config:new(), "git add *",
                        [{cd, ProjectDir}]),
    {ok, _} = sin_sh:sh(sin_config:new(), "git commit -a -m \"tag test\"",
                        [{cd, ProjectDir}]),
    {ok, _} = sin_sh:sh(sin_config:new(), "git tag 2.0.0.0", [{cd, ProjectDir}]),
    Ret = sinan:run_sinan(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    EbinPath = filename:join([ProjectDir, "_build", ProjectName, "lib",
                              ProjectName ++ "-" ++ "2.0.0.0", "ebin",
                              ProjectName ++ ".app"]),
    Result = file:consult(EbinPath),
    ?assertMatch({ok, [{application, _, _}]}, Result),
    {ok, [{application, _, Details}]} = Result,
    ?assertMatch("2.0.0.0", proplists:get_value(vsn, Details)).

v_git_vsn(doc) ->
    ["Create project with a git based vsn and tagged with v"];
v_git_vsn(suite) ->
    [];
v_git_vsn(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    ProjectName = "foo_bar_baz",
    {ProjectDir, _} =
        sint_test_project_gen:single_app_project(PrivDir, ProjectName),
    ?assertMatch(ok,
                 file:write_file(filename:join([ProjectDir, "src",
                                                ProjectName ++ ".app.src"]),
                                 app_src(ProjectName,
                                         "{rm_v, git}"))),
    {ok, _} = sin_sh:sh(sin_config:new(), "git init", [{cd, ProjectDir}]),
    {ok, _} = sin_sh:sh(sin_config:new(), "git add *",
                        [{cd, ProjectDir}]),
    {ok, _} = sin_sh:sh(sin_config:new(), "git commit -a -m \"tag test\"",
                        [{cd, ProjectDir}]),
    {ok, _} = sin_sh:sh(sin_config:new(), "git tag v2.0.0.0", [{cd, ProjectDir}]),
    Ret = sinan:run_sinan(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    EbinPath = filename:join([ProjectDir, "_build", ProjectName, "lib",
                              ProjectName ++ "-" ++ "2.0.0.0", "ebin",
                              ProjectName ++ ".app"]),
    Result = file:consult(EbinPath),
    ?assertMatch({ok, [{application, _, _}]}, Result),
    {ok, [{application, _, Details}]} = Result,
    ?assertMatch("2.0.0.0", proplists:get_value(vsn, Details)).

 app_src(Name, Vsn) ->
     ["%% This is the application resource file (.app file) for the app2,\n"
      "%% application.\n"
      "{application, ", Name, ",\n"
      "[{description, \"Your Desc HERE\"},\n"
      "  {vsn, ", Vsn, "},\n"
      "  {modules, []},\n"
      "  {registered,[", Name, "_sup]},\n"
      "  {applications, [kernel, stdlib]},\n"
      "  {mod, {", Name, "_app,[]}}, \n"
      "  {start_phases, []}]}.\n"].

