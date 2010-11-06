%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Eric Merritt
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
%%% @author Eric Merritt
%%% @doc
%%%   Starts a shell with the correct code paths.
%%% @end
%%% @copyright (C) 2006-2010 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_shell).

-behaviour(sin_task).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%% API
-export([description/0, do_task/1, shell/1]).

-define(TASK, shell).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================
%%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
description() ->
    Desc = "Starts an erlang shell with all of the correct "
        "paths preset so the developer can noodle with the "
        "code to his hearts content",
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
    shell(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the shell command.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
shell(BuildRef) ->
    ProjectApps = sin_build_config:get_value(BuildRef, "project.apps"),
    ProjectRepoApps = sin_build_config:get_value(BuildRef, "project.repoapps"),
    Repo = sin_build_config:get_value(BuildRef, "project.repository"),
    make_shell(BuildRef, ProjectApps, ProjectRepoApps, Repo).



%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Go through and actually start the shell.
%% @spec (BuildRef, ProjectApps, ProjectRepoApps, Repo) -> ok
%% @end
%%--------------------------------------------------------------------
make_shell(BuildRef, ProjectApps, ProjectRepoApps, Repo) ->
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    AppDir = filename:join([BuildDir, "apps"]),
    setup_paths(AppDir, ProjectApps),
    setup_paths(Repo, ProjectRepoApps),
    shell:server(false, false),
    BuildRef.


%%--------------------------------------------------------------------
%% @doc
%%  Send events for all the paths in the list.
%% @spec (BuildRef, AppDir, DirList) -> Paths
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%%  Send out the requisite events for a specific piece of the
%%  application.
%% @spec (RepoDir, AppName, Vsn) -> ok
%% @end
%%--------------------------------------------------------------------
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

