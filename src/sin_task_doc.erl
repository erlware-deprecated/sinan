%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Erlware
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
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Creates edoc format documentation for the project
%%% @end
%%% @copyright (C) 2006-2010 Erlware
%%% Created : 16 Oct 2006 by Eric Merritt <ericbmerritt@gmail.com>
%%%---------------------------------------------------------------------------
-module(sin_task_doc).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1, doc/1]).

-define(TASK, doc).
-define(DEPS, [build]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
description() ->
    Desc = "Runs edoc across all sources in the project and "
        "outputs it into the build area",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = false,
	  deps = ?DEPS,
	  desc = Desc,
	  opts = []}.


%%--------------------------------------------------------------------
%% @doc
%%  Do the task defined in this module.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    doc(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the docs.
%%
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
doc(BuildRef) ->
    Apps = sin_build_config:get_value(BuildRef, "project.apps"),
    run_docs(BuildRef, Apps),
    BuildRef.


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Run edoc on all the modules in all of the applications.
%%
%% @spec (BuildRef, AppList) -> ok
%% @end
%%--------------------------------------------------------------------
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


