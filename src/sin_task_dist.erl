%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Eric Merritt
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
%%% @copyright (C) 2007-2010 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_dist).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1, dist/1]).

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
description() ->
    Desc = "Creates an tarball of the distribution including "
        "release information. Check documentation for the "
        "dist task for configuration information ",
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
    dist(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the dist task.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
dist(BuildRef) ->
    ProjectDir = sin_build_config:get_value(BuildRef, "project.dir"),
    ProjectApps = sin_build_config:get_value(BuildRef, "project.apps"),
    ProjectRepoApps = sin_build_config:get_value(BuildRef, "project.repoapps"),
    Repo = sin_build_config:get_value(BuildRef, "project.repository"),
    make_tar(BuildRef, ProjectDir, ProjectApps, ProjectRepoApps, Repo),
    BuildRef.


%%====================================================================
%%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%%  Go through and actually build up the tar file.
%% @spec make_tar(BuildRef, ProjectDir,
%%   ProjectApps, ProjectRepoApps, Repo) -> ok
%% @end
%%--------------------------------------------------------------------
make_tar(BuildRef, ProjectDir, ProjectApps, ProjectRepoApps, Repo) ->
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    TarDir = filename:join([BuildDir, "tar"]),
    filelib:ensure_dir(filename:join([TarDir, "tmp"])),
    ProjectName = get_project_release_name(BuildRef),
    ReleaseName =
	case sin_build_config:get_value(BuildRef, "-r") of
	    undefined ->
		ProjectName;
	    R ->
		R ++ "-" ++
		    sin_build_config:get_value(BuildRef,
					       "releases." ++ R ++ ".vsn")
	end,
    LibDir = filename:join([ProjectName, "lib"]),
    AppDir = filename:join([BuildDir, "apps"]),
    List1 = gather_dirs(LibDir, Repo, ProjectRepoApps, []),
    List2 = gather_dirs(LibDir, AppDir, ProjectApps, List1),
    List3 = List2 ++ copy_additional_dirs(BuildRef, ProjectName, ProjectDir) ++
        get_release_dirs(BuildRef, ProjectName, BuildDir, ProjectDir) ++
        add_defaults(ProjectDir, ProjectName),
    create_tar_file(BuildRef,
		    filename:join([TarDir,
				   lists:flatten([ReleaseName, ".tar.gz"])]),
                    List3).


%%--------------------------------------------------------------------
%% @doc
%%  Add default directories/files to list of things to include in the dist.
%% @spec (ProjectDir, TopLevel) -> Defaultlist
%% @end
%%--------------------------------------------------------------------
add_defaults(ProjectDir, TopLevel) ->
    Control = "control",
    Bin = "bin",
    lists:foldl(fun(Ele, Acc) ->
                        File = filename:join([ProjectDir, Ele]),
                        case sin_utils:file_exists(File) of
                            true ->
                                [{File, filename:join([TopLevel, Ele])} | Acc ];
                            false ->
                                Acc
                        end
                end,
                [],
                [Control, Bin]).


%%--------------------------------------------------------------------
%% @doc
%%  Actually create the tar file and write in all of the contents.
%% @spec (BuildRef, FileName, TarContents) -> ok
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
%% @doc
%%  Create addition file links for the system.
%% @spec (BuildRef, TopLevel, ProjectDir) -> Dirs
%% @end
%%--------------------------------------------------------------------
copy_additional_dirs(BuildRef, TopLevel, ProjectDir) ->
    NewDirs =
	case sin_build_config:get_value(BuildRef, "tasks.dist.include_dirs") of
	    undefined ->
		[];
	    RequiredDirs ->
		lists:map(fun(Elem) ->
				  NewElem =
				      case is_binary(Elem) of
					  true ->
					      binary_to_list(Elem);
					  false ->
					      Elem
				      end,
				  Name = filename:join([ProjectDir, NewElem]),
				  NewName = filename:join([TopLevel, NewElem]),
				  {Name, NewName}
				end,
			  RequiredDirs)
	end,
    hooks_dir(TopLevel, ProjectDir) ++
	erts_dir(BuildRef, TopLevel) ++
	NewDirs.

%%--------------------------------------------------------------------
%% @doc
%%  Check to see if there is faxien hooks in the hooks dir. If so
%% copy it.
%% @spec (TopLevel, ProjectDir) -> Dirs
%% @end
%%--------------------------------------------------------------------
hooks_dir(TopLevel, ProjectDir) ->
    HooksDir = filename:join([ProjectDir, "_hooks"]),
    case sin_utils:file_exists(HooksDir) andalso
	filelib:fold_files(HooksDir, "fax.*\.erl",
			   false, fun(_,_) -> true end, false) of
	true ->
	    [{HooksDir, filename:join([TopLevel, "_hooks"])}];
	_ ->
	    []
    end.

%%--------------------------------------------------------------------
%% @doc
%%  If an erts version should be included in the dist include it
%% copy it.
%% @spec (BuildRef, TopLevel) -> Dirs
%% @end
%%--------------------------------------------------------------------
erts_dir(BuildRef, TopLevel) ->
    Prefix = code:root_dir(),
    ErtsVersion = erlang:system_info(version),
    ErtsToInclude = filename:join([Prefix, "erts-" ++ ErtsVersion]),
    case sin_utils:to_bool(
	   sin_build_config:get_value(BuildRef,
				      "tasks.dist.include_erts")) of
	true ->
	    [{ErtsToInclude,
	      filename:join([TopLevel, "erts-" ++ ErtsVersion])}];
	_ ->
	    []
    end.



%%--------------------------------------------------------------------
%% @doc
%%  Gather up the applications and return a list of {DirName, InTarName}
%%  pairs.
%% @spec (LibDir, AppDir, DirList, Acc) -> TarablePairs
%% @end
%%--------------------------------------------------------------------
gather_dirs(LibDir, RepoDir, [{AppName, Vsn, _, Path} | T], Acc) ->
    DirName = lists:flatten([atom_to_list(AppName), "-", Vsn]),
    NewName = filename:join([LibDir, DirName]),
    gather_dirs(LibDir, RepoDir, T, [{Path, NewName} | Acc]);
gather_dirs(_, _, [], Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @doc
%%  Get the project release name from the config.
%% @spec (BuildRef) -> ProjectName
%% @end
%%--------------------------------------------------------------------
get_project_release_name(BuildRef) ->
    Version =
	case sin_build_config:get_value(BuildRef, "project.vsn") of
	    undefined ->
		eta_event:task_fault(BuildRef, ?TASK,
				     "No project version defined in build "
				     "config  aborting!"),
		throw(no_project_version);
	    Vsn ->
                      Vsn
              end,
    Name = get_project_name(BuildRef),
    lists:flatten([Name, "-", Version]).


%% @doc
%%  Get the project name from the config.
%% @end
-spec get_project_name(sin_build_config:config()) -> string().
get_project_name(BuildRef) ->
    case sin_build_config:get_value(BuildRef, "project.name") of
	undefined ->
	    eta_event:task_fault(BuildRef, ?TASK,
				 "No project name defined in build config "
				 "aborting!"),
	    throw(no_project_name);
	Nm ->
	    Nm
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Get the release information for the system.
%%
%% @spec (BuildDir, ProjectName, BuildDir, ProjectDir) -> ok
%% @end
%%--------------------------------------------------------------------
get_release_dirs(BuildRef, ProjectName, BuildDir, ProjectDir) ->
    Name = get_project_name(BuildRef),
    SourceReleases = filename:join([BuildDir, "releases", ProjectName]),
    TargetReleases = filename:join([ProjectName, "releases", ProjectName]),
    Result =
        case sin_build_config:get_value(BuildRef,
					"tasks.dist.include_release_info") of
            Value when Value == true; Value == undefined ->
                [{filename:join([SourceReleases,
                                 lists:flatten([Name, ".boot"])]),
                  filename:join([TargetReleases,
                                 lists:flatten([Name, ".boot"])])},
                 {filename:join([SourceReleases,
                                 lists:flatten([Name, ".script"])]),
                  filename:join([TargetReleases,
                                 lists:flatten([Name, ".script"])])},
                 {filename:join([SourceReleases,
                                 lists:flatten([Name, ".rel"])]),
                  filename:join([TargetReleases,
                                 lists:flatten([Name, ".rel"])])}];
            _ ->
                []
        end,
    Result ++ get_config_info(ProjectDir, TargetReleases).


%%--------------------------------------------------------------------
%% @doc
%%   Copy information in config into the releases directory
%% @spec (ProjectDir, TargetReleases) -> ListResult
%% @end
%%--------------------------------------------------------------------
get_config_info(ProjectDir, TargetReleases) ->
    Target = filename:join([ProjectDir, "config"]),
    case sin_utils:file_exists(Target) of
        true ->
            {ok, Files} = file:list_dir(Target),
            lists:foldl(fun(File, Acc) ->
                                gather_config_info(Target,
						   TargetReleases, File, Acc)
                        end,
                        [],
                        Files);
        _ ->
            []
    end.


%%--------------------------------------------------------------------
%% @doc
%%  If its not a directory return it in the correct format
%% @spec (Source, TargetReleases, File, Acc) -> ListResult
%% @end
%%--------------------------------------------------------------------
gather_config_info(Source, TargetReleases, File, Acc) ->
    ActualName = filename:join([Source, File]),
    case filelib:is_dir(ActualName) of
        true ->
            Acc;
        false ->
            [{ActualName,
              filename:join(TargetReleases, File)} | Acc]
    end.
