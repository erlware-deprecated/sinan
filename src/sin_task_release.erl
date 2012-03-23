%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Builds the *.script and *.boot from the project rel file.
%%% @end
%%% @copyright (C) 2006-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_release).

-behaviour(sin_task).

-include_lib("sinan/include/sinan.hrl").

%% API
-export([description/0, do_task/2, format_exception/1]).

-define(TASK, release).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================

%% @doc describes this task to the system
-spec description() -> sin_task:task_description().
description() ->

    Desc = "This command creates all the artifacts needed to start the current
        project as an otp release. This creates the *.rel, *.boot and *.script
        files into the output area of the project. Those files maybe found at:
        <break> <break> <build-area>/realeases/<project-name>-<vsn> |
        <release-name>-<vsn> <break> <break> Check the erlang documentation for
        releases to understand what this means. <break> <break>"

        "The release task tasks a single configuration that looks like
         {types, [{AppName1, RelType},<break>
                  {AppName2, RelType}]}. <break>

        This configuration allows you to specify the type of application this is
        for the release. (Again review the release information for details).
        <break><break> You may substitute your own release file (completely
        replacing the generated file) by placing a file in
        releases/<release-name>.rel<break><break>

        You may also add additional options to the sys_tools:make_script
        call. The one you probably want to add most is the local option, but you
        may add any valid option. You do this by adding the following call to
        your sinan.config.

        <break><break>

         {script_args, [local]}.

        <break><break>

        Not that the value for script_args must always be a list" ,

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "release",
          short_desc = "Creates an otp release for the system",
          desc = Desc,
          opts = []}.

%% @doc create an otp release
-spec do_task(sin_config:matcher(), sin_state:state()) -> sin_state:state().
do_task(Config, State0) ->
    BuildDir = sin_state:get_value(build_dir, State0),
    ReleaseDir = sin_state:get_value(release_dir, State0),
    ReleaseName = sin_state:get_value(release, State0),
    Version = sin_state:get_value(release_vsn, State0),
    ReleaseInfo = generate_rel_file(Config, State0, ReleaseDir,
                                    ReleaseName, Version),
    State1 = sin_state:store(rel, ReleaseInfo, State0),
    create_bin_file(State1, BuildDir,
                    ReleaseName, Version, get_erts_info()),
    copy_or_generate_sys_config_file(Config, ReleaseDir, Version),
    copy_apps(Config, State1, BuildDir),
    include_erts(Config, State1, BuildDir),
    make_boot_script(State1, Config, ReleaseInfo),
    State1.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(?SIN_EXEP_UNPARSE(_, release_script_generation_error, Description)) ->
    Description;
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Generate release information from info available in the project.
-spec generate_rel_file(sin_config:config(), sin_state:state(), string(),
                        string(), string()) ->
    {ReleaseFile::string(), ReleaseInfo::term()}.
generate_rel_file(Config, State, ReleaseDir, Name, Version) ->
    ReleaseName = sin_state:get_value(release, State),
    ReleaseVsn = sin_state:get_value(release_vsn, State),

    RootDir = sin_state:get_value(project_dir, State),

    Release =
        case sin_release:get_release(State, RootDir, ReleaseName,
                                     ReleaseVsn) of
            no_file ->
                Erts = get_erts_info(),
                Deps0 =
                    process_deps(Config:match(types, []),
                                 sin_state:get_value(release_runtime_deps, State) ++
                                     sin_state:get_value(project_apps, State), []),
                Deps1 = lists:map(fun({App, AppVersion}) ->
                                          {App, AppVersion}
                                  end, Deps0),

                {release, {erlang:atom_to_list(Name), Version}, {erts, Erts},
                 lists:sort(Deps1)};
            RelInfo ->
                RelInfo
        end,
    {save_release(Config, State, ReleaseDir, Name, Version, Release), Release}.

%% @doc Process the dependencies into a format useful for the rel depends area.
-spec process_deps([atom()],
                   [AppInfo::tuple()], [AppInfo::tuple()]) ->
    [AppInfo::tuple()].
process_deps(Types, [#app{name=App, vsn=Vsn} | T], Acc) ->
    case lists:keyfind(App, 1, Types) of
        {App, Type} ->
            process_deps(Types, T, [{App, Vsn, list_to_atom(Type)} | Acc]);
        _ ->
            process_deps(Types, T, [{App, Vsn} | Acc])
    end;
process_deps(_, [], Acc) ->
    Acc.

%% @doc Save the release terms to a releases file for later use by the system.
-spec save_release(sin_config:config(), sin_state:state(), string(),
                   string(), string(), term()) ->
    {Location::string(), RelBase::string()}.
save_release(Config, State, RelDir, Name, Version, RelInfo) ->
    Location = filename:join(RelDir, Version),
    filelib:ensure_dir(filename:join([Location, "tmp"])),
    Relbase = filename:join([Location, Name]),
    Relf = lists:flatten([Relbase, ".rel"]),
    case file:open(Relf, write) of
        {error, _} ->
            sin_log:normal(Config, "Couldn't open ~s for writing. Unable to "
                           "write release information",
                           [Relf]),
            ?SIN_RAISE(State,
                       unable_to_write_rel_info);
        {ok, IoDev} ->
            io:format(IoDev, "~p.", [RelInfo]),
            file:close(IoDev)
    end,
    {Location, Relbase}.

%% @doc Get the system erts version.
-spec get_erts_info() -> ErtsVersion::string().
get_erts_info() ->
    erlang:system_info(version).

%% @doc Gather up the path information and make the boot/script files.
-spec make_boot_script(sin_state:state(), sin_config:config(), {{string(), string()}, term()}) ->
    ok.
make_boot_script(State, Config, {{Location, File}, {release, {Name, _}, _, _}}) ->
    Options = [{path, [Location | get_code_paths(State)]},
               no_module_tests, silent] ++ Config:match(script_args, []),
    case make_script(Name, File, Location, Options)  of
        ok ->
            ok;
        error ->
            ?SIN_RAISE(State, release_script_generation_error);
        {ok, _, []} ->
            ok;
        {ok,Module,Warnings} ->
            Detail = lists:flatten(Module:format_warning(Warnings)),
            ?SIN_RAISE(State, release_script_generation_error,
                       "~s~n", [Detail]);
        {error,Module,Error} ->
            Detail = lists:flatten(Module:format_error(Error)),
            ?SIN_RAISE(State, release_script_generation_error,
                       "~s~n", [Detail])
    end.

-spec make_script(string(), string(), string(), [term()]) ->
                         ok | error | {ok, term(), []} |
                         {ok, atom(), [term()]} |
                         {ok, atom(), [term()]}.
make_script(Name, File, Location, Options) ->
    %% Erts 5.9 introduced a non backwards compatible option to
    %% erlang this takes that into account
    Erts = erlang:system_info(version),
    case Erts == "5.9" orelse ec_string:compare_versions(Erts, "5.9") of
        true ->
            systools_make:make_script(Name,
                                      File, [no_warn_sasl,
                                             {outdir, Location} | Options]);
        _ ->
            systools_make:make_script(Name,
                                      File, [{outdir, Location} | Options])
    end.

%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_generate_sys_config_file(sin_config:config(), string(),
                                       string()) ->
                                              ok.
copy_or_generate_sys_config_file(Config, RelDir, Version) ->
    RelSysConfPath = filename:join([RelDir, Version, "sys.config"]),
    case Config:match(config_dir, undefined) of
        undefined ->
            generate_sys_config_file(RelSysConfPath);
        ConfigDir ->
            ConfSysConfPath = filename:join([ConfigDir, "sys.config"]),
            case filelib:is_regular(ConfSysConfPath) of
                false ->
                    generate_sys_config_file(RelSysConfPath);
                true ->
                    file:copy(ConfSysConfPath, RelSysConfPath)
            end
    end.

%% @doc write a generic sys.config to the path RelSysConfPath
-spec generate_sys_config_file(string()) -> ok.
generate_sys_config_file(RelSysConfPath) ->
    {ok, Fd} = file:open(RelSysConfPath, [write]),
    io:format(Fd,
              "%% Thanks to Ulf Wiger at Ericcson for these comments:~n"
              "%%~n"
              "%% This file is identified via the erl command line option -config File.~n"
              "%% Note that File should have no extension, e.g.~n"
              "%% erl -config .../sys (if this file is called sys.config)~n"
              "%%~n"
              "%% In this file, you can redefine application environment variables.~n"
              "%% This way, you don't have to modify the .app files of e.g. OTP applications.~n"
              "[].~n", []),
    file:close(Fd).



%% @doc Generates the correct set of code paths for the system.
-spec get_code_paths(sin_state:state()) -> sin_config:config().
get_code_paths(State) ->
    [filename:join([Path, "ebin"]) ||
        #app{path=Path} <-
            sin_state:get_value(release_runtime_deps, State) ++
            sin_state:get_value(project_apps, State)].

%% @doc Optionally add erts directory to release, if defined.
-spec include_erts(sin_config:config(), sin_state:state(), string()) -> ok.
include_erts(Config, State, ReleaseRootDir) ->
    ErtsDir = sin_utils:get_erts_dir(),
    sin_utils:copy_dir(Config, State, ReleaseRootDir, ErtsDir, [keep_parent]).

copy_apps(Config, State, BuildDir) ->
    LibDir = filename:join(BuildDir, "lib"),
    Apps = sin_state:get_value(release_runtime_deps, State),
    lists:foreach(fun(#app{path=Path}) ->
                          sin_utils:copy_dir(Config, State, LibDir, Path, [keep_parent])
                  end, Apps).
create_bin_file(State, BuildDir, RelName, RelVsn, ErtsVsn) ->
    BinDir = filename:join([BuildDir, "bin"]),
    filelib:ensure_dir(filename:join(BinDir, "tmp")),
    VsnRel = filename:join(BinDir, erlang:atom_to_list(RelName) ++ "-" ++ RelVsn),
    BareRel = filename:join(BinDir, erlang:atom_to_list(RelName)),
    StartFile = bin_file_contents(RelName, RelVsn, ErtsVsn),
    case sin_utils:file_exists(State, VsnRel) of
        false ->
            ok = file:write_file(VsnRel, StartFile),
            ok = file:change_mode(VsnRel, 8#777);
        _ ->
            ok
    end,

    case sin_utils:file_exists(State, BareRel) of
        false ->
            ok = file:write_file(BareRel, StartFile),
            ok = file:change_mode(BareRel, 8#777);
        _ ->
            ok
    end.





bin_file_contents(RelName, RelVsn, ErtsVsn) ->

[<<"#!/bin/sh

set -e

SCRIPT_DIR=`dirname $0`
RELEASE_ROOT_DIR=`cd $SCRIPT_DIR/.. && pwd`
REL_NAME=">>, erlang:atom_to_list(RelName), <<"
REL_VSN=">>, RelVsn, <<"
ERTS_VSN=">>, ErtsVsn, <<"
ERTS_DIR=
SYS_CONFIG=
ROOTDIR=

find_erts_dir() {
    local erts_dir=$RELEASE_ROOT_DIR/erts-$ERTS_VSN
    if [ -d \"$erts_dir\" ]; then
        ERTS_DIR=$erts_dir;
        ROOTDIR=$RELEASE_ROOT_DIR
    else
        local erl=`which erl`
        local erl_root=`$erl -noshell -eval \"io:format(\\\"~s\\\", [code:root_dir()]).\" -s init stop`
        ERTS_DIR=$erl_root/erts-$ERTS_VSN
        ROOTDIR=$erl_root
    fi

}

find_sys_config() {
    local possible_sys=$RELEASE_ROOT_DIR/releases/$REL_VSN/sys.config
    if [ -f \"$possible_sys\" ]; then
        SYS_CONFIG=\"-config $possible_sys\"
    fi
}

find_erts_dir
find_sys_config

export ROOTDIR=$RELEASE_ROOT_DIR
export BINDIR=$ERTS_DIR/bin
export EMU=beam
export PROGNAME=erl
export LD_LIBRARY_PATH=$ERTS_DIR/lib

REL_DIR=$RELEASE_ROOT_DIR/releases/$REL_VSN

$BINDIR/erlexec $SYS_CONFIG -boot $REL_DIR/$REL_NAME $@
">>].

