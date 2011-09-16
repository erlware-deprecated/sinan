%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Builds up a escript distributable application (in the sense of a unix application,
%%%   not an otp application). It looks for an escript directive
%%%   all of the apps to the system.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_escript).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/2, format_exception/1]).

-define(TASK, escript).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================

%% @doc provides a description of the sytem, for help and other reasons
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Creates an escript of the project including ",
    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "escript",
          short_desc = "Provides a standard erlang escript",
          desc = Desc,
          opts = []}.

%% @doc Build an escript for this project
-spec do_task(sin_config:matcher(), sin_state:state()) -> sin_state:state().
do_task(Config, State) ->
    ProjectDir = sin_state:get_value(project_dir, State),
    ProjectApps = sin_state:get_value(project_apps, State),
    make_escript(Config, State, ProjectDir, ProjectApps).

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Go through and actually build up the escript blob
-spec make_escript(sin_config:matcher(),
                   sin_state:state(), string(), [term()]) ->
                          sin_state:state().
make_escript(Config, State, ProjectDir, ProjectApps) ->
    BuildDir = sin_state:get_value(build_dir, State),
    EscriptDir = filename:join([BuildDir, "escript"]),
    filelib:ensure_dir(filename:join([EscriptDir, "tmp"])),
    ProjectName = Config:match(project_name),
    ReleaseName =
        try
            Config:match('-r')
        catch
            throw:not_found ->
                erlang:atom_to_list(ProjectName)
        end,
    EscriptOptions = Config:match(escript, []),
    PossibleSourceFile = get_source_file(State, ProjectDir, EscriptOptions),

    Body =
        case PossibleSourceFile of
            [] ->
                gather_dirs(State, EscriptDir, ProjectApps);
            _ ->
                ewl_talk:say("With escript you may have source files "
                             "or archive files, but you may not have "
                             "both. Since script files are defined "
                             "I am omiting dependency archives"),
                []
        end,

    EscriptTarget = filename:join([EscriptDir, ReleaseName]),

    EmuArgs =
        case lists:keyfind(emu_args, 1, EscriptOptions) of
            {emu_args, ArgList} when is_list(ArgList) ->
                {emu_args, ArgList};
            false ->
                [];
            {emu_args, BadArgs} ->
                ewl_talk:say("emu_args to escript must be a list! not ~p",
                             [BadArgs]),
                ?SIN_RAISE(State, {bad_emu_args, BadArgs})
        end,

    case escript:create(EscriptTarget,
                        lists:flatten([shebang, EmuArgs,
                                       PossibleSourceFile, Body])) of
        ok ->
            ewl_talk:say("Escript created at ~s",
                         [EscriptTarget]);
        Error = {error, _} ->
            ewl_talk:say("Enable to create escript at ~s due to ~p!",
                         [EscriptTarget, Error]),
            ?SIN_RAISE(State, {unable_to_create_escript, Error})
    end,
    %% Execute owner 8#00100, Execute group 8#00010, Execute other 8#00001
    %% Read owner 8#00400, Read group 8#00040, Read other 8#00004
    %% Write owner 8#00200, Write group 8#00020, Write other 8#00002
    %% The mode is the sum of permissions
    file:change_mode(EscriptTarget, 8#00100 + 8#00010 + 8#00001 +
                         8#00400 + 8#00040 + 8#00004 +
                         8#00200 + 8#00020 + 8#00002),
    State.

-spec gather_dirs(sin_state:state(), string(), [tuple()]) ->
    [string()].
gather_dirs(State, EscriptDir, [{AppName, Vsn, _, Path}]) ->

    AppNameVsn = erlang:atom_to_list(AppName) ++ "-" ++ Vsn,
    EscriptPath = filename:join([EscriptDir,
                                 AppNameVsn ++ ".ez"]),
    CWD = filename:dirname(Path),
    case zip:create(EscriptPath,
                    [AppNameVsn],
                    [{cwd, CWD},
                     {compress, all},
                     {uncompress,[".beam",".app"]}]) of
        {ok, EscriptPath} ->
            ok;
        {error, enoent} ->
            ewl_talk:say("Error trying to write ez archive "
                         "for application ~s in ~s to ~s. This is "
                         "probably due to dot files (.* .#* "
                         "like emacs archive files in the "
                         "application directory. Do a sinan clean,"
                         "clean out the project directory and "
                         "try again", [AppNameVsn, CWD, EscriptPath]),
            ?SIN_RAISE(State, {error_creating_archive, AppNameVsn, CWD});
        Error ->
            ewl_talk:say("Unknown error (~p) occured while "
                         "trying to write ~s in ~s to ~s",
                         [Error, AppNameVsn, CWD, EscriptPath]),
            ?SIN_RAISE(State, {error_creating_archive, Error})
    end,
    {archive, EscriptPath}.


get_source_file(State, ProjectDir, EscriptOptions) ->
    Sources =
        lists:foldl(fun({source, SourceFile}, Acc) ->
                            AbsoluteSourcePath = filename:join(ProjectDir,
                                                              SourceFile),
                            case file:read_file(AbsoluteSourcePath) of
                                {ok, Source} ->
                                    [{source, Source} | Acc];
                                Error = {error, _} ->
                                    ?SIN_RAISE(State, {unable_to_read_source,
                                                       AbsoluteSourcePath, Error})
                            end;
                       (_, Acc) ->
                            Acc
                    end, [], EscriptOptions),
    case Sources of
        [] ->
            [];
        [Source] ->
            [Source];
        _ ->
            ewl_talk:say("You may only have one source entry in your "
                         "escript directive"),
            ?SIN_RAISE(State, {multiple_source_entries, Sources})
    end.
