%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Creates edoc format documentation for the project
%%% @end
%%% @copyright (C) 2006-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_doc).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1]).

-define(TASK, doc).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================

%% @doc return a description of this task for callers
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Runs edoc across all sources in the project and "
        "outputs it into the build area",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = false,
	  deps = ?DEPS,
	  desc = Desc,
	  opts = []}.

%% @doc run edoc on all applications
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    Apps = sin_config:get_value(BuildRef, "project.apps"),
    run_docs(BuildRef, Apps),
    BuildRef.

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc
%%  Run edoc on all the modules in all of the applications.
-spec run_docs(sin_config:config(), [AppInfo::tuple()]) -> ok.
run_docs(BuildRef, [{AppName, _, _, Path} | T]) ->
    DocDir = filename:join([Path, "docs"]),
    filelib:ensure_dir(filename:join([DocDir, "tmp"])),

    try
	edoc:application(AppName,
			 Path,
			 [{dir, DocDir}])
    catch
	throw:Error ->
	    ?SIN_RAISE(Error)
    end,
    run_docs(BuildRef, T);
run_docs(_BuildRef, []) ->
    ok.


