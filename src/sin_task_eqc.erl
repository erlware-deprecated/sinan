%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   Runs the 'proper' function on all modules in an application
%%%   if that function exits.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_eqc).

-behaviour(sin_task).

-include_lib("sinan/include/sinan.hrl").

%% API
-export([description/0, do_task/2]).

-define(TASK, eqc).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================

%% @doc provides a description of this task
-spec description() ->  sin_task:task_description().
description() ->

    Desc = "This command runs all quick check tests available in the
        project. ",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "eqc",
          desc = Desc,
          short_desc = "Runs all of the existing quick check tests in the project",
          opts = []}.

%% @doc run all tests for all modules in the system
do_task(Config, State0) ->
    lists:foldl(
      fun(#app{name=AppName, modules=Modules}, State1) ->
              io:format("Quick Check testing app ~p~n", [AppName]),
              case Modules == undefined orelse length(Modules) =< 0 of
                  true ->
                      ec_talk:say("No modules defined for ~p.",
                                  [AppName]),
                      State1;
                  false ->
                      run_module_tests(State1, filter_modules(Config, Modules))
              end
      end, State0, sin_state:get_value(project_apps, State0)).


%%====================================================================
%%% Internal functions
%%====================================================================

filter_modules(Config, Modules) ->
    AdditionalArgs = Config:match(additional_args),
    case AdditionalArgs of
        [] ->
            Modules;
        FilterModules ->
            [Mod || Mod <- Modules,
                    lists:member(atom_to_list(Mod#module.name),
                                 FilterModules)]
    end.

%% @doc Run tests for each module that has a test/0 function
-spec run_module_tests(sin_state:state(), [sin_file_info:mod()]) -> ok.
run_module_tests(State0, AllModules) ->
    lists:foldl(
      fun(#module{name=Name, tags=Tags}, State1) ->
              case sets:is_element(eqc, Tags) of
                  true ->
                      case eqc:module(Name) of
                          [] ->
                              State1;
                          Errors when is_list(Errors) ->
                              sin_state:add_run_error(Name, eqc_failure, State1)
                      end;
                  _ ->
                      State1
              end
      end, State0, AllModules).
