%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Erlware
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Uses dialyzer to analyze the project sources and output messages.
%%% @end
%%% @copyright (C) 2007, Erlware
%%% Created : 31 Jan 2007 by Eric Merritt <cyberlync@gmail.com>
%%%---------------------------------------------------------------------------
-module(sin_analyze).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, analyze/1]).

-define(TASK, analyze).
-define(DEPS, [build]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start() -> ok.
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Runs dialyzer on the project and outputs information "
        "to the user. This task may take a significant amount "
        "of time",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = [{"ai", "analyze-init", false}]},
    eta_task:register_task(TaskDesc).



%%--------------------------------------------------------------------
%% @spec do_task(BuildRef, Args) -> ok
%%
%% @doc
%%  dO the task defined in this module.
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    analyze(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%   Run the analyze task.
%%
%% @spec analyze() -> ok.
%% @end
%%--------------------------------------------------------------------
analyze(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK, "Starting analyzation, this may take awhile ..."),
    FlavorArgs = sin_build_config:get_value(BuildRef, "build.args"),
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    PltPath = filename:join([BuildDir, "info", "dialyzer_plt"]),
    case lists:keysearch("analyze-init", 1,  FlavorArgs) of
        {value, {"analyze-init", true}} ->
            generate_local_plt(BuildRef, PltPath);
        _ ->
            run_analyzer(BuildRef, BuildDir, PltPath)
    end,
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec run_analyze(BuildDir, PltPath) -> ok.
%%
%% @doc
%%  Analyze the current project repos and print error and warning
%%  messages.
%% @end
%%--------------------------------------------------------------------
run_analyzer(BuildRef, BuildDir, PltPath) ->
    AppList = sin_build_config:get_value(BuildRef, "project.apps"),
    RepoAppList = sin_build_config:get_value(BuildRef, "project.repoapps"),
    case sin_utils:file_exists(PltPath) of
        false ->
            generate_local_plt(BuildRef, PltPath);
        true ->
            ok
    end,
    BuildPath = filename:join([BuildDir, "apps"]),
    Repo = sin_build_config:get_value(BuildRef, "project.repository"),
    Codepaths = get_code_paths(Repo, RepoAppList, []),
    Codepaths2 = get_code_paths(BuildPath, AppList, [Codepaths]),
    Opts = [{files_rec, Codepaths2}, {from, byte_code},
            {init_plt, PltPath},
            {warnings,   [no_return,
                          no_unused,
                          no_improper_lists,
                          no_tuple_as_fun,
                          no_fun_app,
                          no_match,
                          no_comp,
                          no_guards,
                          no_unsafe_beam,
                          no_fail_call,
                          error_handling]}],
    eta_event:task_event(BuildRef, ?TASK, starting_analysis, "Starting analysis..."),
    output_info(BuildRef, dialyzer:run(Opts)),
    eta_event:task_event(BuildRef, ?TASK, analysis_complete, "analysis complete").

%%--------------------------------------------------------------------
%% @spec generate_local_plt(PltPath) -> ok.
%%
%% @doc
%%  Generate a base plt to make use of.
%% @end
%%--------------------------------------------------------------------
generate_local_plt(BuildRef, PltPath) ->
    eta_event:task_event(BuildRef, ?TASK, generating_plt,
                         "Generating base plt, this could take a "
                         "really long time ..."),
    RepoAppList = sin_build_config:get_value(BuildRef, "project.repoapps"),
    ProjectRepo = sin_build_config:get_value(BuildRef, "project.repository"),
    Codepaths = get_code_paths(ProjectRepo, RepoAppList, []),

    Opts = [{files_rec, Codepaths},
            {from, byte_code},
            {output_plt, PltPath},
            {warnings,   [no_return,
                          no_unused,
                          no_improper_lists,
                          no_tuple_as_fun,
                          no_fun_app,
                          no_match,
                          no_comp,
                          no_guards,
                          no_unsafe_beam,
                          no_fail_call,
                         error_handling]}],
    io:format("~p", [Opts]),
    output_info(BuildRef, dialyzer:run(Opts)),
    eta_event:task_event(BuildRef, ?TASK, plt_generation_complete, "Done generating plt").



%%--------------------------------------------------------------------
%% @spec output_info(Result) -> ok.
%%
%% @doc
%%  Print out teh informational output from dialyzer.
%% @end
%% @private
%%--------------------------------------------------------------------
output_info(BuildRef, {ok, Warnings}) ->
    output_warnings(BuildRef, 1, Warnings);
output_info(BuildRef, {error, Warnings, Errors}) ->
    output_warnings(BuildRef, 1, Warnings),
    output_errors(BuildRef, 1, Errors);
output_info(BuildRef, {error, Message}) ->
    eta_event:task_event(BuildRef, ?TASK, analyze_error, Message).

%%--------------------------------------------------------------------
%% @spec output_warnings(WarningList) -> ok.
%%
%% @doc
%%  Print out dialyzer warnings.
%% @end
%% @private
%%--------------------------------------------------------------------
output_warnings(BuildRef, N, [{{Module, Function, Arity}, Message} | T]) ->
    eta_event:task_event(BuildRef, ?TASK, analyze_warning,
                         {"Warning ~w: ~s:~w ~s", [N, Module, Function, Arity, Message]}),
    output_warnings(BuildRef, N + 1, T);
output_warnings(BuildRef, N, [{{Module, Line}, Message} | T]) ->
    eta_event:task_event(BuildRef, ?TASK,
                         analyze_warning, {"Warning ~w: ~s:~w ~s", [N, Module, Line, Message]}),
    output_warnings(BuildRef, N + 1, T);
output_warnings(_, _, []) ->
    ok.

%%--------------------------------------------------------------------
%% @spec output_errors(ErrorList) -> ok.
%%
%% @doc
%%  Print out errors from dialyzer.
%% @end
%% @private
%%--------------------------------------------------------------------
output_errors(BuildRef, N, [Error | T]) ->
    eta_event:task_event(BuildRef, ?TASK, analyze_error, {"Error ~w: ~s", [N, Error]}),
    output_errors(BuildRef, N + 1, T);
output_errors(_, _, []) ->
    ok.


%%--------------------------------------------------------------------
%% @spec get_code_paths(BuildDir, Apps, Acc) -> EbinPaths.
%%
%% @doc
%%  Generate the code paths for the apps we will check with
%%  dialyzer.
%% @end
%% @private
%%--------------------------------------------------------------------
get_code_paths(BuildDir, [{AppName, Vsn, _} | T], Acc) ->
    get_code_paths(BuildDir, [{AppName, Vsn} | T], Acc);
get_code_paths(BuildDir, [{AppName, Vsn} | T], Acc) ->
    Dir = filename:join([BuildDir,
                         lists:flatten([atom_to_list(AppName), "-", Vsn]),
                         "ebin"]),
    get_code_paths(BuildDir, T, [Dir | Acc]);
get_code_paths(_BuildDir, [], Acc) ->
    Acc.


