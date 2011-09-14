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
         do_task/1,
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
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    Ignorables = sin_config:get_value(BuildRef, "ignore_dirs", []),

    ProjectApps = sin_config:get_value(BuildRef, "project.allapps", []),
    lists:foldl(fun(App, Config1) ->
                          prepare_app(Config1, BuildDir, App, Ignorables)
                  end, BuildRef, ProjectApps).

-spec prepare_app(sin_config:config(), string(), string(),
                  [string()]) ->
                         sin_config:config().
prepare_app(BuildRef0, BuildDir, AppInfo, Ignorables) ->
    {AppName, _AppVsn, _Deps, AppBuildDir} = AppInfo,

    AppStrName = erlang:atom_to_list(AppName),
    AppDir = sin_config:get_value(BuildRef0, "apps." ++ AppStrName
                                        ++ ".basedir"),

    %% Ignore the build dir when copying or we will create a deep monster in a
    %% few builds
    sin_utils:copy_dir(BuildRef0, AppBuildDir, AppDir, "",
                       [BuildDir | Ignorables]),

    BuildRef1 = sin_config:store(BuildRef0, "apps." ++ AppStrName ++ ".builddir",
                                 AppBuildDir),

    BaseDetails = sin_config:get_value(BuildRef1,
                                       "apps." ++ AppStrName ++ ".base"),

    DotApp = filename:join([AppBuildDir, "ebin", AppStrName ++ ".app"]),


    ok = file:write_file(DotApp,
                    io_lib:format("~p.\n",
                                  [{application, AppName,
                                    BaseDetails}])),
    BuildRef1.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================
