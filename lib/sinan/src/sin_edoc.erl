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
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%  Creates edoc format documentation for the project
%%% @end
%%% @copyright (C) 2006, 2007 Erlware
%%% Created : 16 Oct 2006 by Eric Merritt <cyberlync@gmail.com>
%%%---------------------------------------------------------------------------
-module(sin_edoc).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, doc/1]).

-define(TASK, doc).
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
    Desc = "Runs edoc across all sources in the project and "
        "outputs it into the build area",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = []},
    eta_task:register_task(TaskDesc).


%%--------------------------------------------------------------------
%% @spec do_task(BuildRef, Args) -> ok
%%
%% @doc
%%  dO the task defined in this module.
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    doc(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the docs.
%%
%% @spec doc(BuildRef) -> ok.
%% @end
%%--------------------------------------------------------------------
doc(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK),
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    DocDir = filename:join([BuildDir, "docs", "edoc"]),
    filelib:ensure_dir(filename:join([DocDir, "tmp"])),
    Apps = fconf:get_value(BuildRef, "project.apps"),
    run_docs(BuildRef, Apps, [{dir, DocDir}]),
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec run_docs(AppList, Opts) -> ok.
%%
%% @doc
%%  Run edoc on all the modules in all of the applications.
%% @end
%%--------------------------------------------------------------------
run_docs(BuildRef, [{AppName, _, _} | T], Opts) ->
    AppDir = fconf:get_value(BuildRef, {path, ["apps",
                                            atom_to_list(AppName),
                                            "basedir"]}),
    edoc:application(AppName,
                     AppDir,
                     Opts),
    run_docs(BuildRef, T, Opts);
run_docs(_BuildRef, [], _Opts) ->
    ok.
