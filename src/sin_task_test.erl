%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   Runs the 'test' function on all modules in an application
%%%   if that function exits.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_test).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/2]).

-define(TASK, test).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================

%% @doc provides a description of this task
-spec description() ->  sin_task:task_description().
description() ->

    Desc = "This command runs all eunit and proper tests available in the
        project. Currently this task only supports eunit. <break> <break> By
        default this command trys to only run tests on code that has actually
        changed since the last test run. This isnt always accurate. You may pass
        the 'all' option to force it to run all tests in the system. ",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "test [all]",
          desc = Desc,
          short_desc = "Runs all of the existing eunit unit tests in the project",
          opts = []}.

%% @doc run all tests for all modules in the system
do_task(Config, State) ->
    Apps = lists:map(fun({App, _Vsn, _Deps, _}) ->
                             App
                     end, sin_state:get_value(project_apps, State)),
    test_apps(build_all(Config), State, Apps, []),
    State.


%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc if the command line has an 'all' prespective run all tests instead of
%% just those that changed
-spec build_all(sin_state:state()) ->
    all | changed.
build_all(Config) ->
    case Config:match(additional_args) of
        ["all"] ->
            all;
        _ ->
            changed
    end.

%% @doc Run tests for all the applications specified.
%% @private
-spec test_apps(all | changed,
                sin_state:state(), [string()], [[atom()]]) -> ok.
test_apps(BuildAll, State, [AppName | T], Acc) ->
    io:format("testing app ~p~n", [AppName]),
    Modules = sin_state:get_value({apps, AppName, file_list}, State),
    case Modules == undefined orelse length(Modules) =< 0 of
        true ->
            ewl_talk:say("No modules defined for ~p.",
                         [AppName]),
            ok;
        false ->
            prepare_for_tests(BuildAll, Modules)
    end,
    test_apps(BuildAll, State, T, [Modules | Acc]);
test_apps(_, _, [], Modules) ->
    Modules.

%% @doc Prepare for running the tests. This mostly means seting up the
%% coverage tools.
-spec prepare_for_tests(all | changed,
                        [tuple()]) -> ok.
prepare_for_tests(BuildAll, AllModules) ->
    run_module_tests(BuildAll, AllModules).


%% @doc Run tests for each module that has a test/0 function
-spec run_module_tests(all | changed, [atom()]) -> ok.
run_module_tests(BuildAll, AllModules) ->
    lists:foreach(
      fun({{_, Module, _, _, _},
           {HasChanged, TestImplementations,
            TestedModules, _}}) ->
              case {BuildAll, lists:member(proper, TestImplementations),
                    tested_changed(TestedModules, AllModules)} of
                  {all, _, _} ->
                      proper:module(Module);
                  {_, true, true} ->
                      proper:module(Module);
                  _ ->
                      ok
              end,
              case {BuildAll,
                    lists:member(eunit, TestImplementations), HasChanged} of
                  {all, _, _} ->
                      ewl_talk:say("testing ~p", [Module]),
                      eunit:test(Module);
                  {_, true, changed} ->
                      ewl_talk:say("testing ~p", [Module]),
                      eunit:test(Module);
                  _ ->
                      ok
              end
      end, AllModules).


%% @doc check to see if any of the modules listed in 'TestedModules' have
%% changed. If so return true, else return false.
-spec tested_changed([module()], [tuple]) -> boolean().
tested_changed([], _) ->
    true;
tested_changed(TestedModules, All) ->
    tested_changed1(TestedModules, All).

tested_changed1([TestModule | Rest], FileList) ->
    case ec_lists:find(fun({{_, TargetModule, _, _, _},
                            {changed, _, _, _}}) ->
                               TargetModule == TestModule;
                          (_) ->
                               false
                       end, FileList) of
        {ok, _} ->
            true;
        _ ->
            false
    end,
    tested_changed(Rest, FileList);
tested_changed1([], _) ->
    false.
