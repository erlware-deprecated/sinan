%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%% Prepare the apps for building in the build area
%%% @end
%%% @copyright (C) 2011 Erlware, LLC.
%%%---------------------------------------------------------------------------
-module(sin_task_prepare).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0,
         do_task/2,
         format_exception/1]).

-define(TASK, prepare).
-define(DEPS, [depends]).

%%====================================================================
%% API
%%====================================================================

%% @doc provide a description of the system for the caller
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Prepares the build area for the rest of the tasks "
        "that occur in the system",
    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          example = "prepare",
          short_desc = "build area preparation",
          deps = ?DEPS,
          desc = Desc,
          opts = []}.

%% @doc do the system preparation
-spec do_task(sin_config:matcher(), sin_state:state()) -> sin_state:state().
do_task(_Config, State0) ->
    BuildDir = sin_state:get_value(build_dir, State0),
    Ignorables = sin_state:get_value(ignore_dirs, State0),

    ProjectApps = sin_state:get_value(project_allapps, State0),
    lists:foldl(fun(App, State1) ->
                          prepare_app(State1, BuildDir, App, Ignorables)
                  end, State0, ProjectApps).

-spec prepare_app(sin_state:state(), string(), string(),
                  [string()]) ->
                         sin_config:config().
prepare_app(State0, BuildDir, AppInfo, Ignorables) ->
    {AppName, _AppVsn, _Deps, AppBuildDir} = AppInfo,

    AppDir = sin_state:get_value({apps, AppName, basedir}, State0),

    %% Ignore the build dir when copying or we will create a deep monster in a
    %% few builds
    sin_utils:copy_dir(State0, AppBuildDir, AppDir, "",
                       [BuildDir | Ignorables]),

    State1 = sin_state:store({apps, AppName, builddir},
                             AppBuildDir, State0),

    BaseDetails = sin_state:get_value({apps, AppName, base}, State1),

    DotApp = filename:join([AppBuildDir, "ebin", erlang:atom_to_list(AppName) ++ ".app"]),


    ok = file:write_file(DotApp,
                    io_lib:format("~p.\n",
                                  [{application, AppName,
                                    BaseDetails}])),
    State1.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================
