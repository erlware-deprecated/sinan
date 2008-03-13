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
%%%   Builds up a distributable application (in the sense of a unix application,
%%%   not an otp application). It looks for a top level bin and adds
%%%   all of the apps to the system. Its a little stupid right now, but as
%%%   I get a better understanding of my needs its usefulness will grow.
%%% @end
%%% @copyright (C) 2007, Erlware
%%%---------------------------------------------------------------------------
-module(sin_dist_builder).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, dist/1]).

-define(TASK, dist).
-define(DEPS, [release]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start() -> ok
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Creates an tarball of the distribution including "
        "release information. Check documentation for the "
        "dist task for configuration information ",
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
    dist(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the dist task.
%% @spec dist() -> ok.
%% @end
%%--------------------------------------------------------------------
dist(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK),
    ProjectDir = fconf:get_value(BuildRef, "project.dir"),
    ProjectApps = fconf:get_value(BuildRef, "project.apps"),
    ProjectRepoApps = fconf:get_value(BuildRef, "project.repoapps"),
    Repo = fconf:get_value(BuildRef, "project.repository"),
    make_tar(BuildRef, ProjectDir, ProjectApps, ProjectRepoApps, Repo),
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec make_tar(ProjectDir, ProjectApps, ProjectRepoApps, Repo) -> ok.
%%
%% @doc
%%  Go through and actually build up the tar file.
%% @end
%%--------------------------------------------------------------------
make_tar(BuildRef, ProjectDir, ProjectApps, ProjectRepoApps, Repo) ->
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    TarDir = filename:join([BuildDir, "tar"]),
    filelib:ensure_dir(filename:join([TarDir, "tmp"])),
    ProjectName = get_project_name(BuildRef),
    LibDir = filename:join([ProjectName, "lib"]),
    AppDir = filename:join([BuildDir, "apps"]),
    List1 = gather_dirs(LibDir, Repo, ProjectRepoApps, []),
    List2 = gather_dirs(LibDir, AppDir, ProjectApps, List1),
    List3 = List2 ++ copy_additional_dirs(BuildRef, ProjectName, ProjectDir) ++
        get_release_dirs(BuildRef, ProjectName, BuildDir),
    create_tar_file(BuildRef, filename:join([TarDir,
                                   lists:flatten([ProjectName, ".tar.gz"])]),
                                  List3).

%%--------------------------------------------------------------------
%% @spec create_tar_file(FileName, TarContents) -> ok.
%%
%% @doc
%%  Actually create the tar file and write in all of the contents.
%% @end
%%--------------------------------------------------------------------
create_tar_file(BuildRef, FileName, TarContents) ->
    case erl_tar:open(FileName, [compressed, write]) of
        {error, _} ->
            eta_event:task_fault(BuildRef, ?TASK,
                                 {"Unable to open tar file ~s, unable to build "
                                  "distribution.", [FileName]}),
            throw(unable_to_build_dist);
        {ok, Tar} ->
            lists:foreach(fun({Name, NewName}) ->
                                  erl_tar:add(Tar, Name, NewName,
                                              [dereference])
                          end, TarContents),
            erl_tar:close(Tar)
    end.

%%--------------------------------------------------------------------
%% @spec copy_additional_dirs(TopLevel, ProjectDir) -> ok.
%%
%% @doc
%%  Create addition file links for the system.
%% @end
%%--------------------------------------------------------------------
copy_additional_dirs(BuildRef, TopLevel, ProjectDir) ->
    case fconf:get_value(BuildRef, "dist.include_dirs") of
        undefined ->
            [];
        RequiredDirs ->
            lists:map(fun(Elem) ->
                              Name = filename:join([ProjectDir, Elem]),
                              NewName = filename:join([TopLevel, Elem]),
                              {Name, NewName}
                      end, RequiredDirs)
    end.

%%--------------------------------------------------------------------
%% @spec gather_local_dirs(LibDir, AppDir, DirList, Acc) -> TarablePairs.
%%
%% @doc
%%  Gather up theapplications and return a list of {DirName, InTarName}
%%  pairs.
%% @end
%%--------------------------------------------------------------------
gather_dirs(LibDir, RepoDir, [{AppName, Vsn, _} | T], Acc) ->
    gather_dirs(LibDir, RepoDir, [{AppName, Vsn} | T], Acc);
gather_dirs(LibDir, RepoDir, [{AppName, Vsn} | T], Acc) ->
    DirName = lists:flatten([atom_to_list(AppName), "-", Vsn]),
    Name = filename:join([RepoDir, DirName]),
    NewName = filename:join([LibDir, DirName]),
    gather_dirs(LibDir, RepoDir, T, [{Name, NewName} | Acc]);
gather_dirs(_, _, [], Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @spec get_project_name() -> ProjectName.
%%
%% @doc
%%  Get the project name from the config.
%% @end
%%--------------------------------------------------------------------
get_project_name(BuildRef) ->
    Version = case fconf:get_value(BuildRef, "project.vsn") of
                  undefined ->
                      eta_event:task_fault(BuildRef, ?TASK,
                                           "No project version defined in build config"
                                           " aborting!"),
                      throw(no_project_version);
                  Vsn ->
                      Vsn
              end,
    Name = case fconf:get_value(BuildRef, "project.name") of
               undefined ->
                   eta_event:task_fault(BuildRef, ?TASK,
                                        "No project name defined in build config "
                                        "aborting!"),
                   throw(no_project_name);
               Nm ->
                   Nm
           end,
    lists:flatten([Name, "-", Version]).


%%--------------------------------------------------------------------
%% @doc
%%  Get the release information for the system.
%%
%% @spec get_release_dirs(BuildDir) -> ok.
%% @end
%%--------------------------------------------------------------------
get_release_dirs(BuildRef, TopLevel, BuildDir) ->
    Version = case fconf:get_value(BuildRef, "project.vsn") of
                  undefined ->
                      eta_event:task_fault(BuildRef, ?TASK,
                                           "No project version defined in build config"
                                           " aborting!"),
                      throw(no_project_version);
                  Vsn ->
                      Vsn
              end,
    Name = case fconf:get_value(BuildRef, "project.name") of
               undefined ->
                   eta_event:task_fault(BuildRef, ?TASK,
                                        "No project name defined in build config "
                                        "aborting!"),
                   throw(no_project_name);
               Nm ->
                   Nm
           end,
    case fconf:get_value(BuildRef, "dist.include_release_info") of
        Value when Value == true; Value == undefined ->
            [{filename:join([BuildDir, "releases", Version,
                            lists:flatten([Name, ".boot"])]),
              filename:join([TopLevel, "releases", Version,
                             lists:flatten([Name, ".boot"])])},
             {filename:join([BuildDir, "releases", Version,
                             lists:flatten([Name, ".script"])]),
              filename:join([TopLevel, "releases", Version,
                             lists:flatten([Name, ".script"])])},
             {filename:join([BuildDir, "releases", Version,
                             lists:flatten([Name, ".rel"])]),
              filename:join([TopLevel, "releases", Version,
                             lists:flatten([Name, ".rel"])])}];
        _ ->
            []
    end.


