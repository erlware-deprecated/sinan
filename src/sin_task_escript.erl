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

-include_lib("sinan/include/sinan.hrl").

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

    Desc = "This takes the current project and turns it into an executable
    escript. <break> <break>

     Be aware though that there are significant
    limitations in escript. These are not limitations of sinan, but limitations
    in the built in escript functionality. These limitations are <break> <break>

    - Your escript may be built off of a single script OR Erlang OTP
    Applications but not both <break> <break> <break>

    The system will warn you
    if you violate these restrictions. <break> <break>

    The escript task allows for a few options in the sinan.config file. The
    options are specified as follows: <break> <break>

    {escript, [{OptionKey, OptionValue}]}. <break> <break>

    The keys available are: <break> <break>

    {source, <Path To Source File Rooted in the Project Dir>} <break>

    {emu_args, \"String Of Escript Emulator Args\"}. <break> <break>

    {include_apps, [the, non-project, apps, i, want, in, my,
        escript]}. <break> <break>

    So a fully configured escript config would look like: <break> <break>

     {escript,
       [{source, \"bin/my_cool_escript_file\"}, <break>
       {emu_args, \"-smp disable\"},
       {include_apps, [kernel, stdlib, my_dep]}. <break> <break>

    See the escript documentation for details and remember to only pass the
    script option if you want that to be your escript.",

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
    ReleaseApps = sin_state:get_value(release_apps, State),
    ProjectApps = sin_state:get_value(project_apps, State),
    BuildDir = sin_state:get_value(build_dir, State),
    EscriptDir = filename:join([BuildDir, "escript"]),
    EscriptWorkingDir = filename:join(EscriptDir, ".ez"),
    ec_file:mkdir_path(EscriptWorkingDir),
    ReleaseName = sin_state:get_value(release, State),

    EscriptOptions = Config:match(escript, []),
    PossibleSourceFile = get_source_file(State, ProjectDir, EscriptOptions),

    Body =
        case PossibleSourceFile of
            [] ->
                make_archive(State,
                             ReleaseName,
                             EscriptWorkingDir,
                             gather_dirs(State, EscriptWorkingDir,
                                         filter_apps(ReleaseApps,
                                                     EscriptOptions)
                                         ++
                                             ProjectApps, []));
            _ ->
                ec_talk:say("With escript you may have source files "
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
                ec_talk:say("emu_args to escript must be a list! not ~p",
                             [BadArgs]),
                ?SIN_RAISE(State, {bad_emu_args, BadArgs})
        end,

    case escript:create(EscriptTarget,
                        lists:flatten([shebang, EmuArgs,
                                       PossibleSourceFile, Body])) of
        ok ->
            ec_talk:say("Escript created at ~s",
                         [EscriptTarget]);
        Error = {error, _} ->
            ec_talk:say("Enable to create escript at ~s due to ~p!",
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
    sin_utils:delete_dir(EscriptWorkingDir),
    State.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================

-spec gather_dirs(sin_state:state(), string(), [tuple()], [string()]) ->
    [string()].
gather_dirs(State0, EscriptTargetDir,
            [#app{name=AppName, vsn=Vsn, path=Path} | T], FileList) ->
    FileName = erlang:atom_to_list(AppName) ++ "-" ++ Vsn,
    Target = filename:join(EscriptTargetDir, FileName),
    ok = ec_file:mkdir_path(Target),
    State1 = sin_utils:copy_dir(State0, Target, Path),
    gather_dirs(State1, EscriptTargetDir, T, [FileName | FileList]);
gather_dirs(_State, _, [], FileList) ->
    FileList.

make_archive(State, ProjectName, CWD, FileList) ->
    EscriptPath = filename:join([CWD,
                                 erlang:atom_to_list(ProjectName) ++ ".ez"]),
    case zip:create(EscriptPath,
                    FileList,
                    [{cwd, CWD},
                     {compress, all},
                     {uncompress,[".beam",".app"]}]) of
        {ok, EscriptPath} ->
            ok;
        {error, enoent} ->
            ec_talk:say("Error trying to write ez archive "
                         "for applications in ~s. This is "
                         "probably due to dot files (.* .#* "
                         "like emacs archive files in the "
                         "application directory. Do a sinan clean,"
                         "clean out the project directory and "
                         "try again", [EscriptPath]),
            ?SIN_RAISE(State, {error_creating_archive, EscriptPath});
        Error ->
            ec_talk:say("Unknown error (~p) occured while "
                         "trying to write ~s to ~s",
                         [Error, CWD, EscriptPath]),
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
                                    ?SIN_RAISE(State,
                                               {unable_to_read_source,
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
            ec_talk:say("You may only have one source entry in your "
                         "escript directive"),
            ?SIN_RAISE(State, {multiple_source_entries, Sources})
    end.

filter_apps(Apps, EscriptOptions) ->
    IncludedApps =
        case lists:keyfind(include_apps, 1, EscriptOptions) of
            {include_apps, IApps} ->
                IApps;
            _ ->
                []
        end,
    lists:filter(fun(#app{name=AppName}) ->
                         lists:member(AppName, IncludedApps)
                 end, Apps).

