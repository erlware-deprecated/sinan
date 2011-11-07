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

-include_lib("sinan/include/sinan.hrl").

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
        changed since the last test run. This isnt always accurate.
        ",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "test [all]",
          desc = Desc,
          short_desc = "Runs all of the existing eunit unit tests in the project",
          opts = []}.

%% @doc run all tests for all modules in the system
do_task(_Config, State) ->
    test_apps(State,
              sin_state:get_value(project_apps, State), []),
    State.


%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Run tests for all the applications specified.
%% @private
-spec test_apps(sin_state:state(), [sinan:app()], [[atom()]]) -> ok.
test_apps(State, [#app{name=AppName, modules=Modules} | T], Acc) ->
    io:format("testing app ~p~n", [AppName]),
    case Modules == undefined orelse length(Modules) =< 0 of
        true ->
            ec_talk:say("No modules defined for ~p.",
                         [AppName]),
            ok;
        false ->
            run_module_tests(Modules)
    end,
    test_apps(State, T, [Modules | Acc]);
test_apps(_, [], Modules) ->
    Modules.

%% @doc Run tests for each module that has a test/0 function
-spec run_module_tests([sin_file_info:mod()]) -> ok.
run_module_tests(AllModules) ->
    lists:foreach(
      fun(#module{name=Name, tags=Tags}) ->
              case sets:is_element(proper, Tags) of
                  true ->
                      proper:module(Name);
                  _ ->
                      ok
              end,
              case sets:is_element(eunit, Tags) of
                  true ->
                      ec_talk:say("testing ~p", [Name]),
                      eunit:test(Name);
                  _ ->
                      ok
              end
      end, AllModules).
