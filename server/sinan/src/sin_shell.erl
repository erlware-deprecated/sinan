%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
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
%%% @copyright (C) 2007, Erlware
%%%---------------------------------------------------------------------------
-module(sin_shell).

-behaviour(eta_gen_task).

-include("etask.hrl").
-include("eunit.hrl").

%% API
-export([start/0, do_task/1, shell/1, create_cmdline/2]).

-define(TASK, shell).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================
%%%--------------------------------------------------------------------
%% @spec start() -> ok
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Starts an erlang shell with all of the correct "
        "paths preset so the developer can noodle with the "
        "code to his hearts content",
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
    shell(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the shell command.
%% @spec shell() -> ok
%% @end
%%--------------------------------------------------------------------
shell(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK, "Starting a shell ..."),
    ProjectApps = sin_build_config:get_value(BuildRef, "project.apps"),
    ProjectRepoApps = sin_build_config:get_value(BuildRef, "project.repoapps"),
    Repo = sin_build_config:get_value(BuildRef, "project.repository"),
    make_shell(BuildRef, ProjectApps, ProjectRepoApps, Repo),
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec make_shell(ProjectDir, ProjectApps, ProjectRepoApps, Repo) -> ok.
%%
%% @doc
%%  Go through and actually start the shell.
%% @end
%%--------------------------------------------------------------------
make_shell(BuildRef, ProjectApps, ProjectRepoApps, Repo) ->
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    AppDir = filename:join([BuildDir, "apps"]),
    Paths = gather_paths(AppDir, ProjectApps, []) ++
        gather_paths(Repo, ProjectRepoApps, []),
    Prefix = os:getenv("ROOTDIR"),
    ErlBin = filename:join([Prefix, "bin", "erl"]),
    case filelib:is_regular(ErlBin) of
        true ->
            ok;
        false ->
            eta_event:task_fault(BuildRef, ?TASK, {"erl binary missing: ~s", [ErlBin]}),
            throw({error, missing_erl_binary})
    end,
    Cmdline = lists:flatten(["xterm -e ", ErlBin, " -sname sinan_shell ",
                             create_cmdline(Paths, [])]),
    os:cmd(Cmdline).


%%--------------------------------------------------------------------
%% @spec create_cmdline(List, Str) -> CommandLineString.
%%
%% @doc
%%  Takes the individual files and creates a command line for them.
%% @end
%%--------------------------------------------------------------------
create_cmdline([H | T], Str) ->
    Cmd = lists:flatten([" -pa \"", H, "\""]),
    create_cmdline(T, [Cmd, Str]);
create_cmdline([], Str) ->
    lists:flatten(lists:reverse(Str)).

%%--------------------------------------------------------------------
%% @spec gather_paths(AppDir, DirList, Acc) -> Paths.
%%
%% @doc
%%  Gather up the applications and return a list of paths
%%  pairs.
%% @end
%%--------------------------------------------------------------------
gather_paths(RepoDir, [{AppName, Vsn, _} | T], Acc) ->
    gather_paths(RepoDir, [{AppName, Vsn} | T], Acc);
gather_paths(RepoDir, [{AppName, Vsn} | T], Acc) ->
    DirName = lists:flatten([atom_to_list(AppName), "-", Vsn]),
    Name = filename:join([RepoDir, DirName, "ebin"]),
    gather_paths(RepoDir, T, [Name | Acc]);
gather_paths(_, [], Acc) ->
    Acc.


%%====================================================================
%% Tests
%%====================================================================
create_cmdline_test() ->
    "" = create_cmdline([], ""),
    " -pa \"/my/test/path\"" = create_cmdline(["/my/test/path"], ""),
    " -pa \"/p1\" -pa \"/p2\"" = create_cmdline(["/p1", "/p2"], "").
