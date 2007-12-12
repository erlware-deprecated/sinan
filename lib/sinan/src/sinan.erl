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
%%% @end
%%% @copyright (C) 2007, Erlware
%%% Created :  8 Dec 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(sinan).

%% API
-export([build/0, build/1,
         analyze/1, analyze/0,
         doc/1, doc/0,
         shell/0, shell/1,
         gen/1, gen/0,
         clean/0, clean/1,
         help/0, help/1,
         depends/0, depends/1,
         test/0, test/1,
         release/0, release/1,
         dist/0, dist/1,
         start/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Run the build task. {@link sin_erl_builder}
%% @spec build() -> ok
%% @end
%%--------------------------------------------------------------------
build() ->
    build([]).

%%--------------------------------------------------------------------
%% @doc
%%  Run the build with the specified args. {@link sin_erl_builder}
%% @spec build(Args) -> ok
%% @end
%%--------------------------------------------------------------------
build(Args) ->
    eta_engine:run(sinan, build, Args).

%%--------------------------------------------------------------------
%% @doc
%%  Run the analyze task. {@link sin_analyze}
%% @spec analyze() -> ok
%% @end
%%--------------------------------------------------------------------
analyze() ->
    analyze([]).

%%--------------------------------------------------------------------
%% @doc
%%  Run the analyze task. {@link sin_analyze}
%% @spec analyze(Args::list()) -> ok
%% @end
%%--------------------------------------------------------------------
analyze(Args) ->
    eta_engine:run(sinan, analyze, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the doc task. {@link sin_edoc}
%% @spec doc() -> ok
%% @end
%%--------------------------------------------------------------------
doc() ->
    doc([]).

%%--------------------------------------------------------------------
%% @doc
%%  run the doc task. {@link sin_edoc}
%% @spec doc() -> ok
%% @end
%%--------------------------------------------------------------------
doc(Args) ->
    eta_engine:run(sinan, doc, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the shell task. {@link sin_shell}
%% @spec shell() -> ok
%% @end
%%--------------------------------------------------------------------
shell() ->
    shell([]).

%%--------------------------------------------------------------------
%% @doc
%%  run the shell task. {@link sin_shell}
%% @spec shell(Args::list()) -> ok
%% @end
%%--------------------------------------------------------------------
shell(Args) ->
    eta_engine:run(sinan, shell, Args).

%%--------------------------------------------------------------------
%% @doc
%%  Run the gen task. {@link sin_gen}
%% @spec gen() -> ok
%% @end
%%--------------------------------------------------------------------
gen() ->
    gen([]).

%%--------------------------------------------------------------------
%% @doc
%%  Run the gen task. {@link sin_gen}
%% @spec gen(Args::list()) -> ok
%% @end
%%--------------------------------------------------------------------
gen(Args) ->
    eta_engine:run(singen, gen, Args).


%%--------------------------------------------------------------------
%% @doc
%%  run the clean task. {@link sin_clean}
%% @spec clean() -> ok
%% @end
%%--------------------------------------------------------------------
clean() ->
    clean([]).

%%--------------------------------------------------------------------
%% @doc
%%  run the clean task. {@link sin_clean}
%% @spec clean(Args::list()) -> ok
%% @end
%%--------------------------------------------------------------------
clean(Args) ->
    eta_engine:run(sinan, clean, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the help task. {@link sin_help}
%% @spec help() -> ok
%% @end
%%--------------------------------------------------------------------
help() ->
    help([]).

%%--------------------------------------------------------------------
%% @doc
%%  run the help task. {@link sin_help}
%% @spec help(Args::list()) -> ok
%% @end
%%--------------------------------------------------------------------
help(Args) ->
    eta_engine:run(sinhelp, help, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the depends task. {@link sin_depends}
%% @spec depends() -> ok
%% @end
%%--------------------------------------------------------------------
depends() ->
    depends([]).

%%--------------------------------------------------------------------
%% @doc
%%  run the depends task. {@link sin_depends}
%% @spec depends(Args::list()) -> ok
%% @end
%%--------------------------------------------------------------------
depends(Args) ->
    eta_engine:run(sinan, depends, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the test task. {@link sin_test}
%% @spec test() -> ok
%% @end
%%--------------------------------------------------------------------
test() ->
    test([]).

%%--------------------------------------------------------------------
%% @doc
%%  run the test task. {@link sin_test}
%% @spec test(Args::list()) -> ok
%% @end
%%--------------------------------------------------------------------
test(Args) ->
    eta_engine:run(sinan, test, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the release task. {@link sin_release_builder}
%% @spec release() -> ok
%% @end
%%--------------------------------------------------------------------
release() ->
    release([]).

%%--------------------------------------------------------------------
%% @doc
%%  run the release task. {@link sin_release_builder}
%% @spec release(Args::list()) -> ok
%% @end
%%--------------------------------------------------------------------
release(Args) ->
    eta_engine:run(sinan, release, Args).

%%--------------------------------------------------------------------
%% @doc
%%  run the dist task. {@link sin_dist_builder}
%% @spec dist() -> ok
%% @end
%%--------------------------------------------------------------------
dist() ->
    dist([]).

%%--------------------------------------------------------------------
%% @doc
%%  run the dist task. {@link sin_dist_builder}
%% @spec dist(Args::list()) -> ok
%% @end
%%--------------------------------------------------------------------
dist(Args) ->
    eta_engine:run(sinan, dist, Args).


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
