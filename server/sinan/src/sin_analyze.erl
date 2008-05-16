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
-compile(export_all).

-define(TASK, analyze).
-define(DEPS, [build]).
-define(MINUTES_PER_APP, 10).
-define(WIP_TIME, 5000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec () -> ok
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
                     opts = []},
    eta_task:register_task(TaskDesc).



%%--------------------------------------------------------------------
%% @doc
%%  dO the task defined in this module.
%% @spec do_task(BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    analyze(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%   Run the analyze task.
%%
%% @spec analyze(BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
analyze(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK,
                         "Starting analyzation, this may take awhile ..."),
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    PltPath = filename:join([BuildDir, "info", "dialyzer_plt"]),
    case sin_build_config:get_value(BuildRef, "tasks.analyze.init", false) of
        true ->
            generate_local_plt(BuildRef, PltPath);
        _ ->
            run_analyzer(BuildRef, BuildDir, PltPath)
    end,
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Analyze the current project repos and print error and warning
%%  messages.
%% @spec (BuildRef, BuildDir, PltPath) -> ok
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
            {init_plt, PltPath}],
    eta_event:task_event(BuildRef, ?TASK, starting_analysis, "Starting analysis..."),
    output_info(BuildRef, dialyzer:run(Opts)),
    eta_event:task_event(BuildRef, ?TASK, analysis_complete, "analysis complete").

%%--------------------------------------------------------------------
%% @doc
%%  Generate a base plt to make use of.
%%
%% @spec (BuildRef, PltPath) -> ok
%% @end
%%--------------------------------------------------------------------
generate_local_plt(BuildRef, PltPath) ->
    RepoAppList = sin_build_config:get_value(BuildRef, "project.repoapps"),
    ProjectRepo = sin_build_config:get_value(BuildRef, "project.repository"),
    Codepaths = get_code_paths(ProjectRepo, RepoAppList, []),
    Minutes = integer_to_list(length(Codepaths) * ?MINUTES_PER_APP),
    eta_event:task_event(BuildRef, ?TASK, generating_plt,
                         "Generating base plt, this could take as "
                         "long as " ++ Minutes ++ " minutes"),
    {ok, Pid} = sin_wip:start_link(BuildRef, ?TASK, ".", 5000),
    try gen_plt(BuildRef, PltPath, Codepaths) after
        sin_wip:quit(Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%%  generate a plt file for use by the project. Plt should be
%% generated over all dependencies
%% @spec (BuildRef, PltPath, Codepaths) -> ok
%% @end
%%--------------------------------------------------------------------
gen_plt(BuildRef, PltPath, Codepaths) ->
    Opts = [{files_rec, Codepaths},
            {from, byte_code},
            {output_plt, PltPath}],
    io:format("~p", [Opts]),
    Output = dialyzer:run(Opts),
    output_info(BuildRef, Output),
    case Output of
        {error, _, _} ->
            ?ETA_RAISE_D(plt_generation_fault,
                         "Unable to generate plt due to errors printed "
                         "previously.");
        _ ->
            ok
    end,
    eta_event:task_event(BuildRef, ?TASK, plt_generation_complete,
                         "Done generating plt").



%%--------------------------------------------------------------------
%% @doc
%%  Print out the informational output from dialyzer.
%% @spec output_info(BuildRef, Result) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
output_info(BuildRef, {ok, Warnings}) ->
    output_warnings(BuildRef, 1, Warnings);
output_info(BuildRef, {error, Warnings, Errors}) ->
    output_warnings(BuildRef, 1, Warnings),
    output_errors(BuildRef, 1, Errors).

%%--------------------------------------------------------------------
%% @doc
%%  Print out dialyzer warnings.
%% @spec output_warnings(BuildRef, Count, WarningList) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
output_warnings(BuildRef, N, [All | T]) ->
    eta_event:task_event(BuildRef, ?TASK, analyze_warning,
                         dialyzer:format_warning(All)),
    output_warnings(BuildRef, N + 1, T);
output_warnings(_, _, []) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Print out errors from dialyzer.
%% @spec output_errors(BuildRef, Count, ErrorList) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
output_errors(BuildRef, N, [Error | T]) ->
    eta_event:task_event(BuildRef, ?TASK, analyze_error, {"Error ~w: ~s", [N, Error]}),
    output_errors(BuildRef, N + 1, T);
output_errors(_, _, []) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%%  Generate the code paths for the apps we will check with
%%  dialyzer.
%% @spec get_code_paths(BuildDir, Apps, Acc) -> EbinPaths
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


