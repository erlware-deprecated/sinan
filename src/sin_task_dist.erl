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
-export([description/0, do_task/2, format_exception/1]).

-define(TASK, dist).
-define(DEPS, [release]).

%%====================================================================
%% API
%%====================================================================

%% @doc provides a description of the sytem, for help and other reasons
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Creates an tarball of the distribution including release \n"
        "information. Check documentation for the dist task for configuration \n"
        "information ",
    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "dist",
          short_desc = "Provides a standard erlang distribution tarball",
          desc = Desc,
          opts = []}.

%% @doc Build a dist tarball for this project
-spec do_task(sin_config:matcher(), sin_state:state()) -> sin_state:state().
do_task(Config, State) ->
    ProjectDir = sin_state:get_value(project_dir, State),
    ProjectApps = sin_state:get_value(project_apps, State),
    ProjectRepoApps = sin_state:get_value(project_repoapps, State),
    make_tar(Config, State, ProjectDir, ProjectApps, ProjectRepoApps).

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Go through and actually build up the tar file.
-spec make_tar(sin_config:matcher(),
               sin_state:state(), string(), [term()],
               [term()]) ->
                      sin_state:state().
make_tar(Config, State, ProjectDir, ProjectApps, ProjectRepoApps) ->
    BuildDir = sin_state:get_value(build_dir, State),
    TarDir = filename:join([BuildDir, "tar"]),
    filelib:ensure_dir(filename:join([TarDir, "tmp"])),
    ProjectName = Config:match(project_name),
    ReleaseName =
        try
            RStr = Config:match('-r'),
            R = erlang:list_to_atom(RStr),
            {R, Vsn, _Apps} = lists:keyfind(R, 1, Config:match(releases)),
            RStr ++ "-" ++ Vsn
        catch
            throw:not_found ->
                erlang:atom_to_list(ProjectName) ++ "-" ++ Config:match(project_vsn)
        end,

    LibDir = filename:join([ProjectName, "lib"]),
    List1 = gather_dirs(LibDir, ProjectRepoApps, []),
    List2 = gather_dirs(LibDir, ProjectApps, List1),
    List3 = List2 ++ copy_additional_dirs(Config, State, ProjectName, ProjectDir) ++
        get_release_dirs(Config, State, ProjectName, BuildDir, ProjectDir) ++
        add_defaults(State, ProjectDir, ProjectName),
    create_tar_file(State, filename:join([TarDir,
                                   lists:flatten([ReleaseName, ".tar.gz"])]),
                    List3),
    State.

%% @doc Add default directories/files to list of things to include in the dist.
-spec add_defaults(sin_state:state(),
                   string(), string()) -> [{string(), string()}].
add_defaults(State, ProjectDir, TopLevel) ->
    Bin = "bin",
    lists:foldl(fun(Ele, Acc) ->
                        File = filename:join([ProjectDir, Ele]),
                        case sin_utils:file_exists(State, File) of
                            true ->
                                [{File, filename:join([TopLevel, Ele])} | Acc ];
                            false ->
                                Acc
                        end
                end,
                [],
                [Bin]).

%% @doc Actually create the tar file and write in all of the contents.
-spec create_tar_file(sin_state:state(), string(), [string()]) ->
    ok.
create_tar_file(State, FileName, TarContents) ->
    case erl_tar:open(FileName, [compressed, write]) of
        Error = {error, _} ->
            ewl_talk:say("Unable to open tar file ~s, unable to build "
                         "distribution.", [FileName]),
            ?SIN_RAISE(State, {unable_to_build_dist, Error});
        {ok, Tar} ->
            lists:foreach(fun({Name, NewName}) ->
                                  erl_tar:add(Tar, Name, NewName,
                                              [dereference])
                          end, TarContents),

            erl_tar:close(Tar)
    end.

%% @doc Create addition file links for the system.
-spec copy_additional_dirs(sin_config:matcher(), sin_state:state(), string(), string()) ->
    [string()].
copy_additional_dirs(Config, State, TopLevel, ProjectDir) ->
    NewDirs =
        case Config:match(include_dirs, undefined) of
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
    hooks_dir(State, TopLevel, ProjectDir) ++
        erts_dir(Config, TopLevel) ++
        NewDirs.

%% @doc Check to see if there are faxien hooks in the hooks dir. If so copy it.
-spec hooks_dir(sin_state:state(),
                string(), string()) -> {string(), string()}.
hooks_dir(State, TopLevel, ProjectDir) ->
    HooksDir = filename:join([ProjectDir, "_hooks"]),
    case sin_utils:file_exists(State, HooksDir) andalso
        filelib:fold_files(HooksDir, "fax.*\.erl",
                           false, fun(_,_) -> true end, false) of
        true ->
            [{HooksDir, filename:join([TopLevel, "_hooks"])}];
        _ ->
            []
    end.

%% @doc If an erts version should be included in the dist include it copy it.
-spec erts_dir(sin_config:matcher(), string()) -> {string(), string()}.
erts_dir(Config, TopLevel) ->
    Prefix = code:root_dir(),
    ErtsVersion = erlang:system_info(version),
    ErtsToInclude = filename:join([Prefix, "erts-" ++ ErtsVersion]),
    case Config:match(include_erts, false) of
        true ->
            [{ErtsToInclude,
              filename:join([TopLevel, "erts-" ++ ErtsVersion])}];
        _ ->
            []
    end.

%% @doc Gather up the applications and return a list of {DirName, InTarName}
%% pairs.
-spec gather_dirs(string(), [string()], list()) ->
    [{string(), string()}].
gather_dirs(LibDir, [{AppName, Vsn, _, Path} | T], Acc) ->
    DirName = lists:flatten([atom_to_list(AppName), "-", Vsn]),
    NewName = filename:join([LibDir, DirName]),
    gather_dirs(LibDir, T, [{Path, NewName} | Acc]);
gather_dirs(_, [], Acc) ->
    Acc.

%% @doc Get the release information for the system.
-spec get_release_dirs(sin_config:matcher(),
                       sin_state:state(),  string(), string(), string()) -> [{string(), string()}].
get_release_dirs(Config, State, ProjectName, BuildDir, ProjectDir) ->
    Name = Config:match(project_name),
    SourceReleases = filename:join([BuildDir, "releases", ProjectName]),
    TargetReleases = filename:join([ProjectName, "releases", ProjectName]),
    Result =
        case Config:match(include_release_info, undefined) of
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
    Result ++ get_config_info(State, ProjectDir, TargetReleases).

%% @doc Copy information in config into the releases directory
-spec get_config_info(sin_state:state(),
                      string(), string) -> [{string(), string()}].
get_config_info(State, ProjectDir, TargetReleases) ->
    Target = filename:join([ProjectDir, "config"]),
    case sin_utils:file_exists(State, Target) of
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
