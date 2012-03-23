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

-include_lib("sinan/include/sinan.hrl").

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

    Desc = "This command creates a release, then tars that release into a
    standard erlang distribution tarball that can be deployed in the standard
    erlang manner. Check the erlang documentation about sys_tools and
    distribution tarballs. Configuration options are as follows:
     <break>
     <break>
        {include_dirs, List}.
    <break>
    <break>

    This is a list of directories rooted at the project that you would like
    included in the tarball. You do not need to include your OTP Application
    directories or metadata files as they are included automatically. However,
    you should include any additionally directories that you would like to
    ship.

    <break>
    <break>
         {include_erts, true | false}.
     <break>
    <break>

    This is a boolean that indicates to the system whether or not you want the
    Erlang runtime system included in the tarball. This allows you to distribute
    the vm with your release but has the drawback of turning your tarball into a
    platform specific thing.",

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
    BuildRoot = sin_state:get_value(build_root, State),
    TarDir = filename:join([BuildRoot, "tar"]),
    filelib:ensure_dir(filename:join([TarDir, "tmp"])),
    ReleaseName = erlang:atom_to_list(sin_state:get_value(release, State)) ++ "-"
        ++ sin_state:get_value(release_vsn, State),
    BuildDir = sin_state:get_value(build_dir, State),
    List1 = exclude_erts_dir(Config, ReleaseName, gather_dirs(BuildDir, ReleaseName)),
    create_tar_file(Config, State, filename:join([TarDir,
                                   lists:flatten([ReleaseName, ".tar.gz"])]),
                    List1),
    State.


%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Actually create the tar file and write in all of the contents.
-spec create_tar_file(sin_config:config(), sin_state:state(), string(), [string()]) ->
    ok.
create_tar_file(Config, State, FileName, TarContents) ->
    case erl_tar:open(FileName, [compressed, write]) of
        Error = {error, _} ->
            sin_log:log(Config, "Unable to open tar file ~s, unable to build "
                         "distribution.", [FileName]),
            ?SIN_RAISE(State, {unable_to_build_dist, Error});
        {ok, Tar} ->
            lists:foreach(fun({Name, NewName}) ->
                                  erl_tar:add(Tar, Name, NewName,
                                              [dereference])
                          end, TarContents),

            erl_tar:close(Tar)
    end.

exclude_erts_dir(Config, TopLevel, Entries) ->
    case Config:match(include_erts, false) of
        true ->
            ErtsVersion = erlang:system_info(version),
            [Entry || Entry={_, Erts} <- Entries,
                      Erts =/= filename:join(TopLevel, "erts-" ++ ErtsVersion)];
        _ ->
            Entries
    end.

%% @doc Gather up the applications and return a list of {DirName, InTarName}
%% pairs.
-spec gather_dirs(string(), string()) ->
    [{string(), string()}].
gather_dirs(ActualDir, TarDir) ->
    {ok, Files} = file:list_dir(ActualDir),
    [{filename:join(ActualDir, File), filename:join(TarDir, filename:basename(File))} ||
        File <- Files,
    File =/= ".sig"].

