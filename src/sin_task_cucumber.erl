%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   Runs cucumber for any features that exist in the projecty
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_cucumber).

-behaviour(sin_task).

-include("internal.hrl").
-include_lib("cucumberl/include/cucumberl.hrl").

%% API
-export([description/0, do_task/1, format_exception/1]).

-define(TASK, cucumber).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================

%% @doc provides a description of this task
-spec description() ->  sin_task:task_description().
description() ->
    Desc = "Runs cucumberl for any features that exist in the system",
    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "test",
          desc = Desc,
          short_desc = "Runs all of the cucumber features in the project",
          opts = []}.

%% @doc run all the cucumber features in the system
do_task(BuildRef) ->
    ProjectRoot = sin_config:get_value(BuildRef, "project.dir"),
    Features = find_files(filename:join([ProjectRoot, "features"]),
                                        ".*\\.feature\$"),

    Outcomes =
        case sin_config:get_value(BuildRef, "gen", undefined) of
            undefined ->
                [ {F, run_feature(BuildRef, F)} || F <- Features ];
            Name ->
                [ {F,
                   gen_feature(BuildRef, F,
                               sin_config:get_value(BuildRef, "where"))}
                  || F <- Features, filename:basename(F, ".feature") == Name]
        end,

    case lists:all(fun({_F, X}) -> X =:= ok end, Outcomes) of
        true    ->
            ok;
        false   ->
            ?SIN_RAISE(BuildRef, {test_failures,
                                  lists:filter(fun({_F, X}) ->
                                                       X =/= ok
                                               end, Outcomes)})
    end,
    sin_config:store(BuildRef, "cucumber.features", Features).

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).


%%====================================================================
%%% Internal functions
%%====================================================================
run_feature(BuildRef, FeatureFile) ->
    try
        case cucumberl:run(FeatureFile) of
            {ok, _}  ->
                ok;
            {failed, #cucumberl_stats{failures=Failed}} ->
                io:format("~p failed steps.~n", [length(Failed)]),
                {failed, Failed}
        end
    catch
        throw:{error, nofile} ->
            ewl_talk:say("No behavior implementation for ~s", [FeatureFile]),
            ?SIN_RAISE(BuildRef, {no_implementation, FeatureFile})
    end.

gen_feature(BuildRef, FeatureFile, TargetApp) ->
    TargetDir =
        filename:join(
          [sin_config:get_value(BuildRef, "apps." ++ TargetApp ++ ".basedir"),
           "test"]),
    ok = ewl_file:mkdir_p(TargetDir),
    ok = cucumberl_gen:gen(FeatureFile, TargetDir).

find_files(Dir, Regex) ->
    filelib:fold_files(Dir, Regex, true, fun(F, Acc) -> [F | Acc] end, []).
