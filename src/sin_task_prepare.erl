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

    Desc = "This command prepares the system to run subsiquent commands. Though
    it can be run independently it exists mostly as a dependency for other tasks
    and plugins. ",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          example = "prepare",
          short_desc = "build system preparation",
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
    State1 = sin_utils:copy_dir(State0, AppBuildDir, AppDir, "",
                                [BuildDir | Ignorables]),


    State2 = sin_state:store({apps, AppName, builddir},
                             AppBuildDir, State1),

    BaseDetails = populate_modules(State1, AppName),


    DotApp = filename:join([AppBuildDir, "ebin",
                            erlang:atom_to_list(AppName) ++ ".app"]),

    ok = file:write_file(DotApp,
                    io_lib:format("~p.\n",
                                  [{application, AppName,
                                    BaseDetails}])),
    State2.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================
populate_modules(State, AppName) ->
    Details = sin_state:get_value({apps, AppName, base}, State),

    SourceModuleList = lists:map(fun({_, Module, _, _, _}) ->
                                         Module
                                 end,
                                 sin_state:get_value({apps, AppName, src_modules_detail},
                                                     State)),

    lists:reverse(
      lists:foldl(fun(Element = {vsn, _}, Acc) ->
                          [{modules, SourceModuleList}, Element | Acc];
                     (Element, Acc) ->
                          [Element | Acc]
                  end, [], lists:keydelete(modules, 1, Details))).
