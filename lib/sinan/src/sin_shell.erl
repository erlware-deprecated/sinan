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
%%% @copyright 2007 Eric Merritt
%%%---------------------------------------------------------------------------
-module(sin_shell).

%% API
-export([shell/2, create_cmdline/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  Run the shell command.
%% @spec shell() -> ok
%% @end
%%--------------------------------------------------------------------
shell(BuildRef, _) ->
    ewl_talk:say("Starting a shell ..."),
    ProjectApps = fconf:get_value(BuildRef, "project.apps"),
    ProjectRepoApps = fconf:get_value(BuildRef, "project.repoapps"),
    Repo = fconf:get_value(BuildRef, "project.repository"),
    make_shell(BuildRef, ProjectApps, ProjectRepoApps, Repo).


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
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    AppDir = filename:join([BuildDir, "apps"]),
    Paths = gather_paths(AppDir, ProjectApps, []) ++ 
        gather_paths(Repo, ProjectRepoApps, []),
    Cmdline = lists:flatten([" xterm -e erl -sname sinan_shell ", 
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
