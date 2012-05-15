%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Erlware
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
%%%  The module handles building the
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%%--------------------------------------------------------------------------
-module(sin_discover).

-include_lib("eunit/include/eunit.hrl").
-include_lib("sinan/include/sinan.hrl").

%% API
-export([discover/2,
         format_exception/1]).

-define(CONFIG_NAME, "sinan.config").

%%====================================================================
%% API
%%====================================================================

%% @doc Run the discover task.
discover(CmdLineConfig, State0) ->
    ProjectDir =
        find_project_root(State0, sin_state:get_value(start_dir, State0)),
    {Config, State1} = create_base_config(CmdLineConfig, State0, ProjectDir),
    {AppDirs, State2} =
        look_for_app_dirs(Config, State1, ProjectDir),
    Apps = [sin_app_meta:build_meta(Config, State2, AppDir)
            || AppDir <- AppDirs],

    populate_config_and_state(ProjectDir, Config,
                              sin_state:store(project_applist, Apps, State2),
                              Apps).


%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(?SIN_EXEP_UNPARSE(_, {ebin_src_conflict, Dir})) ->
    io_lib:format("conflict: ~s has both an ebin/*.app "
                  "and a src/*.app.src.", [Dir]);
format_exception(?SIN_EXEP_UNPARSE(_, unable_to_intuit_config)) ->
    "Unable to generate a project configuration from available metadata.";
format_exception(?SIN_EXEP_UNPARSE(_, {no_app_directories,
                                    ProjectDir})) ->
    io_lib:format("Unable to find any application directories "
                  "searching down from ~s aborting now.", [ProjectDir]);
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================

create_base_config(CommandLineConfig, State0, ProjectDir) ->
    HomeDir = get_home_dir(CommandLineConfig),
    State1 = sin_state:store(home_dir, HomeDir, State0),
    {FileConfig, State2} = get_file_config(ProjectDir, State1),
    DefaultConfig =
        sin_config:new_from_terms(default_config_terms(), []),
    HomeConfig = get_home_config(State2, HomeDir),
    {sin_config:merge(CommandLineConfig,
                      sin_config:merge(FileConfig,
                                       sin_config:merge(HomeConfig,
                                                        DefaultConfig))),
     State2}.

%% @doc This trys to find the build config if it can. If the config is found it
%% is parsed in the normal way and returned to the user.  however, if it is not

%% found then the system attempts to unuit the build config from the project
%% properties.
get_file_config(ProjectDir, State) ->
    ConfigPath = filename:join([ProjectDir, ?CONFIG_NAME]),
    try
        {sin_config:new_from_file(ConfigPath, []),
         sin_state:store(intuited_config, false, State)}
    catch
        throw:{error_accessing_file, ConfigPath, enoent} ->
            {sin_config:new(), sin_state:store(intuited_config, true, State)}
    end.

%% @doc Take a raw config and merge it with the default config information
populate_config_and_state(ProjectDir, Config0, State0, Apps) ->
    Config1 = case sin_state:get_value(intuited_config, false, State0) of
                  false ->
                      Config0;
                  true ->
                      intuit_build_config(Config0, State0, Apps)
              end,
    BuildDir = sin_config:match(build_dir, Config1),
    ReleaseDir = filename:join([ProjectDir,
                                BuildDir,
                                get_release_name(Config1)]),
    State1 = sin_state:store([{release, get_release_name(Config1)},
                              {release_vsn, sin_config:match(project_vsn, Config1)},
                              {build_root, filename:join([ProjectDir,
                                                           BuildDir])},
                              {build_dir, ReleaseDir},
                              {apps_dir, filename:join(ReleaseDir, "lib")},
                              {release_dir, filename:join(ReleaseDir, "releases")},
                              {project_dir, ProjectDir}], State0),
    {Config0, State1}.

%% @doc If this is a single app project then the config is generated from the
%% app name and version combined with the usual default config information. If
%% this is not a single app project then a build config cannot be intuited and
%% an exception is thrown.
intuit_build_config(Config0, State, Apps) ->
    case intuit_from_command_line(Config0) of
        undefined ->
            case Apps of
                [#app{name=AppName,vsn=AppVsn}] ->
                    sin_config:add_all([{project_name, AppName},
                                        {project_vsn, AppVsn}], Config0);
                _ ->
                    ?SIN_RAISE(State, unable_to_intuit_config)
            end;
        Config1 ->
            Config1
    end.

%% @doc The override is command line override values that are provided by the
%% user. If the user provides a project name and version we can use that
intuit_from_command_line(Config) ->
    %% These values are already in the config since they are populated
    %% automatically. We are just going to make sure they are actually there.
    case sin_config:match(project_name, undefined, Config) of
        undefined ->
            undefined;
        _PName ->
            case sin_config:match(project_vsn, undefined, Config) of
                undefined ->
                    undefined;
                _PVsn ->
                    Config
            end
    end.

%% @doc Find the root of the project. The project root may be marked by a _build
%% dir, a build config (sinan.cfg, _build.cfg). If none of those are found then
%% just return the directory we started with.
-spec find_project_root(sin_state:state(), Dir::string()) -> string().
find_project_root(State, Dir) ->
    try
        find_project_root_by_markers(State, Dir, ["_build", ?CONFIG_NAME])
    catch
        throw:no_parent_dir ->
            %% If we couldn't find samething to indicate the project root we
            %% will use the current directory
            Dir
    end.

%% @doc Find the project root given a list of markers. We look for these markers
%% starting in the base dir and then working our way up to the parent dirs. If
%% we hit the top of the directory structure without finding anything we throw
%% an exception.
-spec find_project_root_by_markers(sin_state:state(),
                                   Dir::string(),
                                   RootMarkers::[string()]) -> string().
find_project_root_by_markers(State, Dir, RootMarkers) ->
    case root_marker_exists(State, Dir, RootMarkers) of
        true ->
            Dir;
        false ->
            find_project_root_by_markers(State,  parent_dir(Dir), RootMarkers)
    end.

%% @doc Check to see if the root marker exists in the directory specified.
-spec root_marker_exists(sin_state:state(),
                         Dir::string(), RootMarkers::string()) ->
    boolean().
root_marker_exists(State, Dir, [RootMarker | Rest]) ->
    case has_root_marker(State, Dir, RootMarker) of
        true ->
            true;
        false ->
            root_marker_exists(State, Dir, Rest)
    end;
root_marker_exists(_, _Dir, []) ->
    false.

%% @doc Given a single root marker and a directory return a boolean indicating
%% if the root marker exists in that file.
-spec has_root_marker(sin_state:state(),
                      Dir::string(), RootMarker::string()) ->
    boolean().
has_root_marker(State, Dir, RootMarker) ->
    sin_utils:file_exists(State, filename:join(Dir, RootMarker)).

%% @doc Given a directory returns the name of the parent directory.
-spec parent_dir(Filename::string()) -> DirName::string().
parent_dir(Filename) ->
    parent_dir(filename:split(Filename), []).

%% @doc Given list of directories, splits the list and returns all dirs but the
%%  last as a path.
parent_dir([_H], []) ->
    throw(no_parent_dir);
parent_dir([], []) ->
    throw(no_parent_dir);
parent_dir([_H], Acc) ->
    filename:join(lists:reverse(Acc));
parent_dir([H | T], Acc) ->
    parent_dir(T, [H | Acc]).

%% @doc Rolls through subdirectories of the build directory looking for
%% directories that have a src and an ebin subdir. When it finds one it stops
%% recursing and adds the directory to the list to return.
-spec look_for_app_dirs(sin_config:config(),
                        sin_state:state(),
                        ProjectDir::string()) ->
    AppDirs::[string()].
look_for_app_dirs(Config, State0,  ProjectDir) ->
    Ignorables = sin_config:match(ignore_dirs, Config),
    State1 = sin_state:store(ignore_dirs, Ignorables, State0),
    case process_possible_app_dir(State1, ProjectDir,
                                  Ignorables, []) of
        [] ->
            ?SIN_RAISE(State1, {no_app_directories, ProjectDir});
        Else ->
            {Else, State1}
    end.

%% @doc Process the app dir to see if it is an application directory.
-spec process_possible_app_dir(sin_state:state(),
                               TargetDir::string(),
                               Ignorables::[string()], Acc::list()) ->
                                      ListOfDirs::[{ebin | appsrc, string()}].
process_possible_app_dir(State, TargetDir, Ignorables, Acc) ->
    case filelib:is_dir(TargetDir) andalso not
        sin_utils:is_dir_ignorable(TargetDir, Ignorables) of
        true ->
            {ok, SubDirs} = file:list_dir(TargetDir),
            case is_app_directory(TargetDir, SubDirs) of
                true ->
                    [TargetDir | Acc];
                false ->
                    lists:foldl(fun(Sub, NAcc) ->
                                        Dir = filename:join([TargetDir, Sub]),
                                        process_possible_app_dir(State,
                                                                 Dir,

                                                                 Ignorables,
                                                                 NAcc)
                                end, Acc, SubDirs)
            end;
        false ->
            Acc
    end.

is_app_directory(BaseDir, SubDirs) ->
    has_ebin(BaseDir, SubDirs) orelse has_src(BaseDir, SubDirs).

-spec has_src(string(), [string()]) ->
                          false | {appsrc | ebin, string()}.
has_src(BaseDir, SubDirs) ->
    case lists:member("src", SubDirs) of
        true ->
            sin_utils:get_file_with_ext(
              filename:join(BaseDir, "src"), ".app.src") =/= false;
       false ->
            false
    end.

-spec has_ebin(string(), [string()]) ->
                          false | {appsrc | ebin, string()}.
has_ebin(BaseDir, SubDirs) ->
    case lists:member("ebin", SubDirs) of
        true ->
            sin_utils:get_file_with_ext(
              filename:join(BaseDir, "ebin"), ".app") =/= false;
       false ->
            false
    end.



%% @doc the minimal default configuration info
default_config_terms() ->
    [{build_dir,  "_build"},
     {ignore_dirs, [".", "_"]},
     {ignore_apps, []},
     {compile_args, [debug_info,  report_warnings]},
     {config_dir, "config"}].

get_release_name(Config) ->
    try
        erlang:list_to_atom(sin_config:match('-r', Config))
    catch
        throw:not_found ->
            ProjectName = sin_config:match(project_name, Config),
            case erlang:is_list(ProjectName) of
                true ->
                    erlang:list_to_atom(ProjectName);
                false ->
                    ProjectName
            end
    end.

get_home_dir(Config) ->
    sin_config:match(user_dir, os:getenv("HOME"), Config).

get_home_config(State, HomeDir) ->
    HomeConfigPath = filename:join(HomeDir, ".sinan.config"),
    case sin_utils:file_exists(State, HomeConfigPath) of
        true ->
            try
                sin_config:new_from_file(HomeConfigPath, [])
            catch
                throw:{error_accessing_file,
                       HomeConfigPath, enoent} ->
                    sin_config:new()
            end;
        false ->
            sin_config:new()
    end.
