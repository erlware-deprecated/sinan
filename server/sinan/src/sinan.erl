%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007 Erlware
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
%%%  The module provides a simple api for the sinan system.
%%%  Two possible arguments may be passed in. The start dir that should
%%%  be somewhere inside a project and a list of args for the system.
%%%
%%%  @type startdir() = string(). A dir that is somewhere in a project
%%%  @type args() = {[{TaskName, args}]}
%%%    TaskName = string(). The name of the task the args apply too.
%%% @end
%%% @copyright (C) 2007, Erlware
%%% Created :  8 Dec 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(sinan).

%% API
-export([build/1, build/2,
         analyze/2, analyze/1,
         doc/2, doc/1,
         shell/1, shell/2,
         gen/2, gen/1,
         clean/1, clean/2,
         help/1, help/2,
         depends/1, depends/2,
         test/1, test/2,
         release/1, release/2,
         dist/1, dist/2,
         do_task/3,
         do_task/4,
         start/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Run the build task. {@link sin_erl_builder}
%% @spec build(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
build(StartDir) ->
    build(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  Run the build with the specified args. {@link sin_erl_builder}
%% @spec build(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
build(StartDir, Args) ->
    do_task(build, StartDir, Args).

%%--------------------------------------------------------------------
%% @doc
%%  Run the analyze task. {@link sin_analyze}
%% @spec analyze(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
analyze(StartDir) ->
    analyze(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  Run the analyze task. {@link sin_analyze}
%% @spec analyze(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
analyze(StartDir, Args) ->
    do_task(analyze, StartDir, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the doc task. {@link sin_edoc}
%% @spec doc(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
doc(StartDir) ->
    doc(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  run the doc task. {@link sin_edoc}
%% @spec doc(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
doc(StartDir, Args) ->
    do_task(doc, StartDir, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the shell task. {@link sin_shell}
%% @spec shell(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
shell(StartDir) ->
    shell(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  run the shell task. {@link sin_shell}
%% @spec shell(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
shell(StartDir, Args) ->
   do_task(shell, StartDir, Args).

%%--------------------------------------------------------------------
%% @doc
%%  Run the gen task. {@link sin_gen}
%% @spec gen(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
gen(StartDir) ->
    gen(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  Run the gen task. {@link sin_gen}
%% @spec gen(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
gen(StartDir, Args) ->
    do_task(singen, gen, StartDir, Args).


%%--------------------------------------------------------------------
%% @doc
%%  run the clean task. {@link sin_clean}
%% @spec clean(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
clean(StartDir) ->
    clean(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  run the clean task. {@link sin_clean}
%% @spec clean(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
clean(StartDir, Args) ->
    do_task(clean, StartDir, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the help task. {@link sin_help}
%% @spec help(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
help(StartDir) ->
    help(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  run the help task. {@link sin_help}
%% @spec help(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
help(StartDir, Args) ->
    do_task(sinhelp, help, StartDir, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the depends task. {@link sin_depends}
%% @spec depends(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
depends(StartDir) ->
    depends(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  run the depends task. {@link sin_depends}
%% @spec depends(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
depends(StartDir, Args) ->
    do_task(depends, StartDir, Args).


%%--------------------------------------------------------------------
%% @doc
%%  run the test task. {@link sin_test}
%% @spec test(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
test(StartDir) ->
    test(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  run the test task. {@link sin_test}
%% @spec test(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
test(StartDir, Args) ->
    do_task(test, StartDir, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the release task. {@link sin_release_builder}
%% @spec release(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
release(StartDir) ->
    release(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  run the release task. {@link sin_release_builder}
%% @spec release(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
release(StartDir, Args) ->
    do_task(release, StartDir, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the dist task. {@link sin_dist_builder}
%% @spec dist(StartDir::startdir()) -> ok
%% @end
%%--------------------------------------------------------------------
dist(StartDir) ->
    dist(StartDir, []).

%%--------------------------------------------------------------------
%% @doc
%%  run the dist task. {@link sin_dist_builder}
%% @spec dist(StartDir::startdir(), Args::args()) -> ok
%% @end
%%--------------------------------------------------------------------
dist(StartDir, Args) ->
    do_task(dist, StartDir, Args).


%%--------------------------------------------------------------------
%% @doc
%%  run the specified task, with the default chain
%% @spec do_task(Task, Args) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(Task, StartDir, Args) when is_atom(Task) ->
    do_task(sinan, StartDir, Task, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the specified task
%% @spec do_task(Chain, Task, Args) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(Chain, StartDir, Task, Args) when is_atom(Task) ->
    BuildRef = eta_engine:make_run_id(),
    fconf:start_config(BuildRef,
                       fun sin_parse_handle:parse_config_file/1),
    fconf:store(BuildRef, "build.args", Args),
    fconf:store(BuildRef, "build.start_dir", StartDir),
    eta_engine:run(Chain, Task, BuildRef),
    fconf:stop_config(BuildRef).

%%--------------------------------------------------------------------
%% @doc
%%  Allows sinan to be easily started from the shell. This is a
%%  helper function thats mostly just useful in development.
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(tools),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(edoc),
    application:start(sasl),
    application:start(ibrowse),
    application:start(eunit),
    application:start(ktuo),
    application:start(fconf),
    application:start(ewlib),
    application:start(ewrepo),
    application:start(gs),
    application:start(hipe),
    application:start(xmerl),
    application:start(mnesia),
    application:start(dialyzer),
    application:start(etask),
    application:start(sinan).


%%====================================================================
%% Internal functions
%%====================================================================
