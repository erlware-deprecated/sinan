%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Uses dialyzer to analyze the project sources and output messages.
%%% @end
%%% @copyright (C) 2006-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_dialyzer).

-behaviour(sin_task).

-include_lib("sinan/include/sinan.hrl").

%% API
-export([description/0, do_task/2,
         format_exception/1]).

-define(TASK, dialyzer).
-define(DEPS, [build]).


%%====================================================================
%% API
%%====================================================================
%% @doc provide a description of the system for the caller
-spec description() -> sin_task:task_description().
description() ->

    Desc = "
dialyzer Task
=============

This task runs dialyzer on a project. It checks the 'dialyzer' state of the
project and builds or updates the relevant per project plt files. This task may
take a (very) long time to complete.

One problem that dialyzer has is that all applications being analyzed project
apps and dependencies must be built with debug information. If not, dialyzer
will blow up with some not very useful information. You can get around this by
asking the dialyzer task to ignore certain applications.

Do this in your build config with the following entry.

    {dialyzer_ignore, [IgnoredApps::atom()]}.

The downside, of course, is figuring out which apps actually have the
problem. This task tries to help by printing out a list of applications that
where under analysis when the blowup occurred.

By default all dialyzer warnings are enabled. You can
override this with the directive

    {dialyzer_warnings, [Warnings::term()]}.

Just put the warning options listed in the dialyzer
documentation to get the specific warnings that you would like
to see. Unfortunately, you can not do over ride that on a per
application basis as dialyzer is a whole program analysis
system. ",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          example = "dialyzer",
          short_desc = "Run the Dialyzer analyzer on the project",
          deps = ?DEPS,
          desc = Desc,
          opts = []}.

do_task(Config0, State0) ->
    sin_log:verbose(Config0, "Starting analyzation, this may take awhile ..."),
    BuildDir = sin_state:get_value(build_dir,State0),
    {ProjectPlt, DepPlt} = get_plt_location(BuildDir),
    DepList = sin_state:get_value(release_deps, State0),
    AppList = sin_state:get_value(project_apps, State0),
    sin_log:verbose(Config0, "Doing plt for all dependencies ..."),
    update_dep_plt(Config0, State0, DepPlt, DepList),
    sin_log:verbose(Config0, "Doing plt for project apps ..."),
    update_dep_plt(Config0, State0, ProjectPlt, AppList),

    WarningTypes = Config0:match(dialyzer_warnings, default_warnings()),
    Paths = [filename:join(Path, "ebin") ||
                #app{path=Path} <- AppList],
    Opts = [{analysis_type, succ_typings},
            {from, byte_code},
            {files_rec, Paths},
            {warnings, WarningTypes},
            {plts, [ProjectPlt, DepPlt]}],
    try
        case dialyzer:run(Opts) of
            [] ->
                State0;
            Warnings ->
                ?SIN_RAISE(State0,
                           {warnings, Warnings, AppList})
        end
    catch
        _:{dialyzer_error, Error} ->
            ?SIN_RAISE(State0,
                       {error_processing_apps, Error, AppList})
    end,
    State0.

-spec format_exception(sin_exceptions:exception()) ->
                              string().
format_exception(?SIN_EXEP_UNPARSE(_,
                                   {error_processing_apps, Error, AppList})) ->
    [io_lib:format("~s~n", [Error]),
     "while processing the following apps~n",
     lists:map(fun(#app{name=Name}) ->
                       io_lib:format("   ~s~n", [Name])
               end, AppList)];
format_exception(?SIN_EXEP_UNPARSE(_,
                                   {warnings, Warnings, _AppList})) ->
    [lists:map(fun(Warning) ->
                       dialyzer:format_warning(Warning)
               end, Warnings), "~n"];
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================
get_plt_location(BuildDir) ->
    {filename:join([BuildDir, "project.plt"]),
     filename:join([BuildDir, "deps.plt"])}.

update_dep_plt(Config0, State0, DepPlt, AppList0) ->
    Ignores = Config0:match(dialyzer_ignore, []),
    AppList1 = [App ||
                   App = #app{name=Name} <- AppList0,
                   not lists:member(Name, Ignores)],
    Opts0 =
        case sin_utils:file_exists(State0, DepPlt) of
            true ->
                sin_log:verbose(Config0, "Plt is built, checking/updating ..."),
                [{analysis_type, plt_check},
                 {plts, [DepPlt]}];
            false ->
                sin_log:verbose(Config0, "Building the plt, this is really going to "
                                "take a long time ..."),
                [{analysis_type, plt_build},
                 {output_plt, DepPlt}]
        end,

    Paths = [filename:join(Path, "ebin") ||
                #app{path=Path} <- AppList1],

    Opts = [{files_rec, Paths},
            {from, byte_code}] ++ Opts0,
    try
        dialyzer:run(Opts)
    catch
        _:{dialyzer_error, Error} ->
            ?SIN_RAISE(State0,
                       {error_processing_apps, Error, AppList1})
    end.

default_warnings() ->
    [no_return,
     no_unused,
     no_improper_lists,
     no_fun_app,
     no_match,
     no_opaque,
     no_fail_call,
     error_handling,
     race_conditions,
     unmatched_returns,
     underspecs].
