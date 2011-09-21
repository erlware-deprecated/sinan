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

-include("internal.hrl").

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
        releases to understand what this means. ",

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
    {ReleaseName, Version} =
        try
            R = erlang:list_to_atom(Config:match('-r')),
            {_, Vsn, _} = lists:keyfind(R, 1, Config:match(releases)),
            {R, Vsn}
        catch
            throw:not_found ->
                {Config:match(project_name), Config:match(project_vsn)}
        end,
    ReleaseInfo = generate_rel_file(Config, State0, BuildDir, ReleaseName, Version),
    State1 = sin_state:store({project_release_info}, ReleaseInfo, State0),
    copy_or_generate_sys_config_file(Config, BuildDir, ReleaseName, Version),
    make_boot_script(State1, ReleaseInfo),
    State1.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Generate release information from info available in the project.
-spec generate_rel_file(sin_config:config(), sin_state:state(), string(), string(), string()) ->
    {ReleaseFile::string(), ReleaseInfo::term()}.
generate_rel_file(Config, State, BuildDir, Name, Version) ->
    {ProjectName, ProjectVsn} =
        try
            ReleaseName = erlang:list_to_atom(Config:match('-r')),
            {ReleaseName, Vsn, _} = lists:keyfind(ReleaseName, 1,
                                                  Config:match(releases)),
            {ReleaseName, Vsn}
        catch
            throw:not_found ->
                {Config:match(project_name),  Config:match(project_vsn)}
        end,

    RootDir = sin_state:get_value(project_dir, State),

    Release =
        case sin_release:get_release(State, RootDir, ProjectName,
                                     ProjectVsn) of
            no_file ->
                Erts = get_erts_info(),
                Deps =
                    process_deps(State,
                                 element(1, sin_state:get_value(project_deps, State)), []),
                Deps2 =
                    process_deps(State,
                                 element(2, sin_state:get_value(project_deps, State)), []),
                Deps3 = lists:map(fun({App, AppVersion}) ->
                                          {App, AppVersion, load}
                                  end, Deps2),

                {release, {erlang:atom_to_list(Name), Version}, {erts, Erts},
                 lists:ukeymerge(1, lists:sort(Deps), lists:sort(Deps3))};
            RelInfo ->
                RelInfo
        end,
    {save_release(State, BuildDir, Name, Version, Release), Release}.

%% @doc Process the dependencies into a format useful for the rel depends area.
-spec process_deps(sin_state:state(),
                   [AppInfo::tuple()], [AppInfo::tuple()]) ->
    [AppInfo::tuple()].
process_deps(State, [{App, Vsn, _, _} | T], Acc) ->
    NewApp = stringify(App),
    case {sin_state:get_value({project_release, NewApp, type}, State),
          sin_state:get_value({project_release, NewApp, include_apps}, State)} of
        {undefined, undefined} ->
            process_deps(State, T, [{App, Vsn} | Acc]);
        {Type, undefined} ->
            process_deps(State, T, [{App, Vsn, list_to_atom(Type)} | Acc]);
        {undefined, IncList} ->
            process_deps(State, T,
                         [{App, Vsn, process_inc_list(IncList, [])} | Acc]);
        {Type, IncList} ->
            process_deps(State,
                         T, [{App, Vsn, list_to_atom(Type),
                              process_inc_list(IncList, [])} | Acc])
    end;
process_deps(_State, [], Acc) ->
    Acc.

%% @doc Process the optional include list into a list of atoms.
-spec process_inc_list([string()], [atom()]) ->
    [atom()].
process_inc_list([H | T], Acc) ->
    process_inc_list(T, [list_to_atom(H) | Acc]);
process_inc_list([], Acc) ->
    Acc.

%% @doc Save the release terms to a releases file for later use by the system.
-spec save_release(sin_state:state(), string(),
                   string(), string(), term()) ->
    {Location::string(), RelBase::string()}.
save_release(State, BuildDir, Name, Version, RelInfo) ->
    Location = filename:join([BuildDir, "releases", atom_to_list(Name) ++ "-" ++ Version]),
    filelib:ensure_dir(filename:join([Location, "tmp"])),
    Relbase = filename:join([Location, Name]),
    Relf = lists:flatten([Relbase, ".rel"]),
    case file:open(Relf, write) of
        {error, _} ->
            ewl_talk:say("Couldn't open ~s for writing. Unable to "
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
-spec make_boot_script(sin_state:state(), {{string(), string()}, term()}) ->
    ok.
make_boot_script(State, {{Location, File}, {release, {Name, _}, _, _}}) ->
    Options = [{path, [Location | get_code_paths(State)]},
               no_module_tests, silent],
    case systools_make:make_script(Name,
                                   File, [{outdir, Location} | Options]) of
        ok ->
            ok;
        error ->
            ?SIN_RAISE(State, release_script_generation_error);
        {ok, _, []} ->
            ok;
        {ok,Module,Warnings} ->
            Detail = Module:format_warning(Warnings),
            ewl_talk:say("~s", Detail),
            ?SIN_RAISE(State, release_script_generation_error,
                       "~s~n", [lists:flatten(Detail)]);
        {error,Module,Error} ->
            Detail = Module:format_error(Error),
            ewl_talk:say("~s", Detail),
            ?SIN_RAISE(State, release_script_generation_error,
                       "~s~n", [lists:flatten(Detail)])
    end.


%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_generate_sys_config_file(sin_config:config(),
                                       string(), string(), string()) ->
    ok.
copy_or_generate_sys_config_file(Config, BuildDir, Name, Version) ->
    RelSysConfPath = filename:join([BuildDir, "releases", atom_to_list(Name) ++ "-" ++
                                    Version, "sys.config"]),
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
    ProjApps = sin_state:get_value(project_apps, State),
    ProjPaths = lists:merge(
                  lists:map(
                    fun({App, _, _, _}) ->
                            sin_state:get_value({apps, App, code_paths}, State)
                    end, ProjApps)),
    RepoPaths = lists:map(
                  fun ({_App, _Vsn, _, Path}) ->
                          filename:join([Path, "ebin"])
                  end, sin_state:get_value(project_repoapps, State)),
    lists:merge([ProjPaths, RepoPaths]).

%% @doc Convert the value to a string if it is an atom
-spec stringify(string() | atom()) -> string().
stringify(Value) when is_list(Value) ->
    Value;
stringify(Value) when is_atom(Value) ->
    atom_to_list(Value).
