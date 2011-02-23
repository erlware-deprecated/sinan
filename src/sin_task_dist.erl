%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Builds up a distributable application (in the sense of a unix application,
%%%   not an otp application). It looks for a top level bin and adds
%%%   all of the apps to the system. Its a little stupid right now, but as
%%%   I get a better understanding of my needs its usefulness will grow.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_dist).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1]).

-define(TASK, dist).
-define(DEPS, [release]).

%%====================================================================
%% API
%%====================================================================

%% @doc provides a description of the sytem, for help and other reasons
-spec description() -> sin_task:task_description().
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

%% @doc Build a dist tarball for this project
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    dist(BuildRef).

%% @doc Build a dist tarball for this project
-spec dist(sin_config:config()) -> sin_config:config().
dist(BuildRef) ->
    ProjectDir = sin_config:get_value(BuildRef, "project.dir"),
    ProjectApps = sin_config:get_value(BuildRef, "project.apps"),
    ProjectRepoApps = sin_config:get_value(BuildRef, "project.repoapps"),
    Repo = sin_config:get_value(BuildRef, "project.repository"),
    make_tar(BuildRef, ProjectDir, ProjectApps, ProjectRepoApps, Repo),
    BuildRef.

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Go through and actually build up the tar file.
-spec make_tar(sin_config:config(), string(), [term()],
	       [term()], string()) ->
    sin_config:config().
make_tar(BuildRef, ProjectDir, ProjectApps, ProjectRepoApps, Repo) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    TarDir = filename:join([BuildDir, "tar"]),
    filelib:ensure_dir(filename:join([TarDir, "tmp"])),
    ProjectName = get_project_release_name(BuildRef),
    ReleaseName =
	case sin_config:get_value(BuildRef, "-r") of
	    undefined ->
		ProjectName;
	    R ->
		R ++ "-" ++
		    sin_config:get_value(BuildRef,
					       "releases." ++ R ++ ".vsn")
	end,
    LibDir = filename:join([ProjectName, "lib"]),
    AppDir = filename:join([BuildDir, "apps"]),
    List1 = gather_dirs(LibDir, Repo, ProjectRepoApps, []),
    List2 = gather_dirs(LibDir, AppDir, ProjectApps, List1),
    List3 = List2 ++ copy_additional_dirs(BuildRef, ProjectName, ProjectDir) ++
        get_release_dirs(BuildRef, ProjectName, BuildDir, ProjectDir) ++
        add_defaults(ProjectDir, ProjectName),
    create_tar_file(filename:join([TarDir,
				   lists:flatten([ReleaseName, ".tar.gz"])]),
                    List3).

%% @doc Add default directories/files to list of things to include in the dist.
-spec add_defaults(string(), string()) -> [{string(), string()}].
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

%% @doc Actually create the tar file and write in all of the contents.
-spec create_tar_file(string(), [string()]) ->
    ok.
create_tar_file(FileName, TarContents) ->
    case erl_tar:open(FileName, [compressed, write]) of
        {error, _} ->
	    ewl_talk:say("Unable to open tar file ~s, unable to build "
			 "distribution.", [FileName]),
            throw(unable_to_build_dist);
        {ok, Tar} ->
            lists:foreach(fun({Name, NewName}) ->
                                  erl_tar:add(Tar, Name, NewName,
                                              [dereference])
                          end, TarContents),

            erl_tar:close(Tar)
    end.

%% @doc Create addition file links for the system.
-spec copy_additional_dirs(sin_config:config(), string(), string()) ->
    [string()].
copy_additional_dirs(BuildRef, TopLevel, ProjectDir) ->
    NewDirs =
	case sin_config:get_value(BuildRef, "tasks.dist.include_dirs") of
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

%% @doc Check to see if there are faxien hooks in the hooks dir. If so copy it.
-spec hooks_dir(string(), string()) -> {string(), string()}.
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

%% @doc If an erts version should be included in the dist include it copy it.
-spec erts_dir(sin_config:config(), string()) -> {string(), string()}.
erts_dir(BuildRef, TopLevel) ->
    Prefix = code:root_dir(),
    ErtsVersion = erlang:system_info(version),
    ErtsToInclude = filename:join([Prefix, "erts-" ++ ErtsVersion]),
    case sin_utils:to_bool(
	   sin_config:get_value(BuildRef,
				      "tasks.dist.include_erts")) of
	true ->
	    [{ErtsToInclude,
	      filename:join([TopLevel, "erts-" ++ ErtsVersion])}];
	_ ->
	    []
    end.

%% @doc Gather up the applications and return a list of {DirName, InTarName}
%% pairs.
-spec gather_dirs(string(), string(), [string()], list()) ->
    [{string(), string()}].
gather_dirs(LibDir, RepoDir, [{AppName, Vsn, _, Path} | T], Acc) ->
    DirName = lists:flatten([atom_to_list(AppName), "-", Vsn]),
    NewName = filename:join([LibDir, DirName]),
    gather_dirs(LibDir, RepoDir, T, [{Path, NewName} | Acc]);
gather_dirs(_, _, [], Acc) ->
    Acc.

%% @doc Get the project release name from the config.
-spec get_project_release_name(sin_config:config()) -> string().
get_project_release_name(BuildRef) ->
    Version =
	case sin_config:get_value(BuildRef, "project.vsn") of
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

%% @doc Get the project name from the config.
-spec get_project_name(sin_config:config()) -> string().
get_project_name(BuildRef) ->
    case sin_config:get_value(BuildRef, "project.name") of
	undefined ->
	    eta_event:task_fault(BuildRef, ?TASK,
				 "No project name defined in build config "
				 "aborting!"),
	    throw(no_project_name);
	Nm ->
	    Nm
    end.

%% @doc Get the release information for the system.
-spec get_release_dirs(sin_config:config(), string(), string(), string()) -> [{string(), string()}].
get_release_dirs(BuildRef, ProjectName, BuildDir, ProjectDir) ->
    Name = get_project_name(BuildRef),
    SourceReleases = filename:join([BuildDir, "releases", ProjectName]),
    TargetReleases = filename:join([ProjectName, "releases", ProjectName]),
    Result =
        case sin_config:get_value(BuildRef,
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

%% @doc Copy information in config into the releases directory
-spec get_config_info(string(), string) -> [{string(), string()}].
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

%% @doc If its not a directory return it in the correct format
-spec gather_config_info(string(), string(), string(), [{string(), string()}]) ->
    [{string(), string()}].
gather_config_info(Source, TargetReleases, File, Acc) ->
    ActualName = filename:join([Source, File]),
    case filelib:is_dir(ActualName) of
        true ->
            Acc;
        false ->
            [{ActualName,
              filename:join(TargetReleases, File)} | Acc]
    end.
