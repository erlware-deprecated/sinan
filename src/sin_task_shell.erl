%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Starts a shell with the correct code paths.
%%% @end
%%% @copyright (C) 2006-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_shell).

-behaviour(sin_task).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%% API
-export([description/0, do_task/1]).

-define(TASK, shell).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================
%% @doc return a description of this task
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Starts an erlang shell with all of the correct \n"
        "paths preset so the developer can noodle with the \n"
        "code to his hearts content",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = false,
	  deps = ?DEPS,
	  example = "shell",
	  desc = Desc,
	  short_desc = "Provides an erlang repl on the project",
	  opts = []}.

%% @doc Run the shell command.
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    ProjectApps = sin_config:get_value(BuildRef, "project.apps"),
    ProjectRepoApps = sin_config:get_value(BuildRef, "project.repoapps"),
    Repo = sin_config:get_value(BuildRef, "project.repository"),
    make_shell(BuildRef, ProjectApps, ProjectRepoApps, Repo).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Go through and actually start the shell.
-spec make_shell(sin_config:config(), string(), [string()], string()) ->
    sin_config:config().
make_shell(BuildRef, ProjectApps, ProjectRepoApps, Repo) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    AppDir = filename:join([BuildDir, "apps"]),
    setup_paths(AppDir, ProjectApps),
    setup_paths(Repo, ProjectRepoApps),
    BuildRef.

%% @doc gather a list of paths from the app and add them to the code path
-spec setup_paths(string(), [AppInfo::term()]) -> ok.
setup_paths(RepoDir, [{AppName, Vsn, _, _} | T]) ->
    setup_path(RepoDir, AppName, Vsn),
    setup_paths(RepoDir, T);
setup_paths(RepoDir, [{AppName, Vsn, _} | T]) ->
    setup_path(RepoDir, AppName, Vsn),
    setup_paths(RepoDir, T);
setup_paths(RepoDir, [{AppName, Vsn} | T]) ->
    setup_path(RepoDir, AppName, Vsn),
    setup_paths(RepoDir, T);
setup_paths(_, []) ->
    ok.

%% @doc add the specified app to the code path
-spec setup_path(string(), atom() | string(), string()) -> ok.
setup_path(RepoDir, AppName, Vsn) ->
    NAppName = case is_atom(AppName) of
                   true ->
                       atom_to_list(AppName);
                   false ->
                       AppName
               end,
    DirName = lists:flatten([NAppName, "-", Vsn]),
    Path = filename:join([RepoDir, DirName, "ebin"]),
    code:add_patha(Path).

%%====================================================================
%% Tests
%%====================================================================

