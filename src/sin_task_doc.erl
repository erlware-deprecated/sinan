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

-include_lib("sinan/include/sinan.hrl").

%% API
-export([description/0,
         do_task/2,
         format_exception/1]).

-define(TASK, doc).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================

%% @doc return a description of this task for callers
-spec description() -> sin_task:task_description().
description() ->

    Desc = "
doc Task
========

This command runs Erlang's
[Edoc](http://www.erlang.org/doc/apps/edoc/index.html) across all sources in the
project.

Output of the doc task will be as you expect in the docs directory of the build
area of the relevent OTP App. Remember that Sinan never outputs generated files
into the project itself.",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "doc",
          short_desc = "Genarates edoc documentation for the project",
          desc = Desc,
          opts = []}.

%% @doc run edoc on all applications
-spec do_task(sin_config:config(), sin_state:state()) -> sin_state:state().
do_task(Config, State) ->
    Apps = sin_state:get_value(project_apps, State),
    run_docs(Config, State, Apps),
    State.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc
%%  Run edoc on all the modules in all of the applications.
-spec run_docs(sin_config:config(),
               sin_state:state(), [AppInfo::tuple()]) -> ok.
run_docs(Config, State, [#app{name=AppName, path=Path} | T]) ->
    DocDir = filename:join([Path, "docs"]),
    filelib:ensure_dir(filename:join([DocDir, "tmp"])),

    try
        edoc:application(AppName,
                         Path,
                         get_options(Config:specialize([{app, AppName}]),
                                     DocDir))
    catch
        throw:Error ->
            ?SIN_RAISE(State, Error)
    end,
    run_docs(Config, State, T);
run_docs(_Config, _State, []) ->
    ok.

get_options(Config, DocDir) ->
    Options = Config:match(edoc_options, []),
    case proplists:get_value(dir, Options) of
        undefined ->
            [{dir, DocDir} | Options];
        _ ->
            Options
    end.

