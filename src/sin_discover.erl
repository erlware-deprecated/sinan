%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%%-------------------------------------------------------------------
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
-spec discover(sin_config:config(),
               sin_state:state()) ->
                      sin_state:state().
discover(Config0, State0) ->
    ProjectDir = find_project_root(State0, sin_state:get_value(start_dir, State0)),
    {Config1, State1}
        = process_raw_config(ProjectDir,
                             find_config(ProjectDir, Config0, State0),
                             Config0,
                             State0),
    {AppDirs, State2} = look_for_app_dirs(Config1, State1,
                                          sin_state:get_value(build_dir, State1),
                                          ProjectDir),
    {Config1, build_app_info(State2, AppDirs, [])}.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc This trys to find the build config if it can. If the config is found it
%% is parsed in the normal way and returned to the user.  however, if it is not
%% found then the system attempts to unuit the build config from the project
%% properties.
-spec find_config(string(),
                  sin_config:config(), sin_state:state()) -> sin_config:config().
find_config(ProjectDir, Config, State) ->
    ConfigPath = filename:join([ProjectDir, ?CONFIG_NAME]),
    try
        sin_config:new_from_file(ConfigPath, [])
    catch
        throw:{error_accessing_file, ConfigPath, enoent} ->
            intuit_build_config(ProjectDir, Config, State)
    end.

%% @doc Take a raw config and merge it with the default config information
-spec process_raw_config(string(),
                         sin_config:config(),
                         sin_config:config(),
                         sin_state:state()) ->
    sin_config:config().
process_raw_config(ProjectDir, FileConfig, CommandLineConfig, State0) ->
    DefaultConfig =
        sin_config:new_from_terms(default_config_terms(), []),
    Config0 = sin_config:merge(CommandLineConfig, sin_config:merge(FileConfig, DefaultConfig)),
    BuildDir = sin_config:match(build_dir, Config0),
    State1 = sin_state:store([{release, get_release_name(Config0)},
                              {release_vsn, sin_config:match(project_vsn, Config0)},
                              {build_root, filename:join([ProjectDir,
                                                           BuildDir])},
                              {build_dir, filename:join([ProjectDir,
                                                         BuildDir,
                                                         get_release_name(Config0)])},
                              {project_dir, ProjectDir}], State0),


    {Config0, State1}.

%% @doc If this is a single app project then the config is generated from the
%% app name and version combined with the usual default config information. If
%% this is not a single app project then a build config cannot be intuited and
%% an exception is thrown.
-spec intuit_build_config(string(),
                          sin_config:config(),
                          sin_state:state()) ->
    sin_config:config().
intuit_build_config(ProjectDir, Config, State) ->
    %% App name is always the same as the name of the project dir
    AppName = filename:basename(ProjectDir),
    AppFilePath = get_app_file(ProjectDir, AppName, Config),
    try
        case file:consult(AppFilePath) of
            {error, enoent} ->
                ?SIN_RAISE(State, unable_to_intuit_config,
                           "Unable to generate a project config");
            {error, Error} ->
                ewl_talk:say("~s:~s", [AppFilePath, file:format_error(Error)]),
                ?SIN_RAISE(State, {unable_to_read_config,
                                   AppFilePath, Error});
            {ok, [{application, _, Rest}]} ->
                build_out_intuited_config(AppName, Rest)
        end
    catch
        throw:no_app_metadata_at_top_level ->
            intuit_from_command_line(Config, State)
    end.


%% @doc the app file could be in ebin or src/app.src. we need to
%% check for both
-spec get_app_file(string(), string(), sin_config:config()) ->
                          string().
get_app_file(ProjectDir, AppName, Config) ->
    EbinDir = filename:join([ProjectDir,
                             "ebin",
                             AppName ++ ".app"]),
    SrcDir = filename:join([ProjectDir,
                            "src",
                             AppName ++ ".app.src"]),
    case {sin_utils:file_exists(Config, EbinDir),
          sin_utils:file_exists(Config, SrcDir)} of
        {_, true} ->
            SrcDir;
        {true, _} ->
            EbinDir;
         _ ->
            throw(no_app_metadata_at_top_level)
    end.

%% @doc Given information from the app dir that was found, create a new full
%% populated correct build config.
-spec build_out_intuited_config(AppName::string(), Rest::[{atom(), term()}]) ->
    sin_config:config().
build_out_intuited_config(AppName, Rest) ->
   {value, {vsn, ProjectVsn}} = lists:keysearch(vsn, 1, Rest),
    sin_config:add_all([{project_name, erlang:list_to_atom(AppName)},
                        {project_vsn, ProjectVsn}], sin_config:new()).

%% @doc The override is command line override values that are provided by the
%% user. If the user provides a project name and version we can use that
-spec intuit_from_command_line(sin_config:config(), sin_state:state()) ->
    sin_config:config().
intuit_from_command_line(Config, State) ->
    case sin_config:match(project_name, undefined, Config) of
        undefined ->
            ?SIN_RAISE(State, unable_to_intuit_config);
        _PName ->
            ok
    end,
    case sin_config:match(project_vsn, undefined, Config) of
        undefined ->
            ?SIN_RAISE(State, unable_to_intuit_config);
        _PVsn ->
            ok
    end,
    Config.

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

%% @doc Given a list of app dirs retrieves the application names.
-spec build_app_info(sin_state:state(),
                     List::list(),
                     Acc::list()) ->
    Config::sin_config:config().
build_app_info(State0, [H|T], Acc) ->
    {AppName, AppFile, AppDir} = get_app_info(H),
    case file:consult(AppFile) of
        {ok, [{application, AppName, Details}]} ->

            Applications = case lists:keyfind(applications, 1, Details) of
                               {applications, AppList} ->
                                   AppList;
                               false ->
                                   []
                           end,
            IncludedApps = case lists:keyfind(included_applications, 1, Details) of
                               {included_applications, IncAppList} ->
                                   IncAppList;
                               false ->
                                   []
                           end,

            State1 = source_details(AppDir, AppName,
                                    sin_state:store([{{apps, AppName, dotapp}, AppFile},
                                                     {{apps, AppName, basedir}, AppDir},
                                                     {{apps, AppName, deps}, Applications ++ IncludedApps}],
                                                    State0)),
            State3 =
                lists:foldl(fun({Key, Value}, State4) ->
                                    sin_state:store({apps, AppName, Key}, Value, State4)
                            end,
                            sin_state:store({apps, AppName, base},
                                            Details, State1),
                            Details),

            build_app_info(State3,  T, [AppName | Acc]);
        {error, {_, Module, Desc}} ->
            Error = Module:format_error(Desc),
            ?SIN_RAISE(State0, {invalid_app_file, Error},
                       "*.app file is invalid for ~s at ~s",
                       [AppName, AppFile]);
        {error, Error} ->
            ?SIN_RAISE(State0, {no_app_file, Error},
                       "No *.app file found for ~s at ~s",
                       [AppName, AppFile])
    end;
build_app_info(State, [], Acc) ->
    sin_state:store(project_applist, Acc, State).

-spec get_app_info({ebin | appsrc, string()}) -> {string(), string()}.
get_app_info({ebin, Dir}) ->
    AppName = filename:basename(Dir),
    AppFile = filename:join([Dir, "ebin", string:concat(AppName, ".app")]),
    {erlang:list_to_atom(AppName), AppFile, Dir};
get_app_info({appsrc, Dir}) ->
    AppName = filename:basename(Dir),
    AppFile = filename:join([Dir, "src", string:concat(AppName, ".app.src")]),
    {erlang:list_to_atom(AppName), AppFile, Dir}.

-spec source_details(string(), string(), sin_config:config()) ->
    sin_config:config().
source_details(Dir, AppName, State0) ->
    SrcDir = filename:join([Dir, "src"]),
    TestDir = filename:join([Dir, "test"]),

    SrcModules = gather_modules(SrcDir),
    TestModules = gather_modules(TestDir),
    sin_state:store({apps, AppName, src_modules_detail}, SrcModules,
                    sin_state:store({apps, AppName, all_modules_detail},
                                    SrcModules ++ TestModules, State0)).

%% @doc Rolls through subdirectories of the build directory looking for
%% directories that have a src and an ebin subdir. When it finds one it stops
%% recursing and adds the directory to the list to return.
-spec look_for_app_dirs(sin_config:config(),
                        sin_state:state(),
                        BuildDir::string(),
                        ProjectDir::string()) ->
    AppDirs::[string()].
look_for_app_dirs(Config, State0, BuildDir, ProjectDir) ->
    Ignorables = sin_config:match(ignore_dirs, Config) ++ [BuildDir],
    State1 = sin_state:store(ignore_dirs, Ignorables, State0),
    case process_possible_app_dir(State1, BuildDir, ProjectDir,
                                  Ignorables, []) of
        [] ->
            ?SIN_RAISE(State1, no_app_directories,
                       "Unable to find any application directories."
                       " aborting now");
        Else ->
            {Else, State1}
    end.

%% @doc Process the app dir to see if it is an application directory.
-spec process_possible_app_dir(sin_state:state(),
                               BuildDir::string(),
                               TargetDir::string(),
                               Ignorables::[string()], Acc::list()) ->
                                      ListOfDirs::[{ebin | appsrc, string()}].
process_possible_app_dir(State, BuildDir, TargetDir, Ignorables, Acc) ->
    PossibleAppName = filename:basename(TargetDir),
    case filelib:is_dir(TargetDir) andalso not
        sin_utils:is_dir_ignorable(TargetDir, Ignorables) of
        true ->
            {ok, Dirs} = file:list_dir(TargetDir),
            Ebin = has_src_ebin_dotapp(State, PossibleAppName,
                                       TargetDir, Dirs),
            AppSrc =  has_src_appsrc(State, PossibleAppName,
                                     TargetDir, Dirs),
            case {AppSrc, Ebin} of
                {true, true} ->
                    ?SIN_RAISE(State,
                               "conflict: ~s has both an ebin/*.app "
                               "and a src/*.app.src ", [TargetDir]);
                {true, _} ->
                    [{appsrc, TargetDir} | Acc];
                {_, true}  ->
                    [{ebin, TargetDir} | Acc];
                _ ->
                    lists:foldl(fun(Sub, NAcc) ->
                                        Dir = filename:join([TargetDir, Sub]),
                                        process_possible_app_dir(State,
                                                                 BuildDir,
                                                                 Dir,

                                                                 Ignorables,
                                                                 NAcc)
                                end, Acc, Dirs)
            end;
        false ->
            Acc
    end.

-spec has_src_ebin_dotapp(sin_state:state(),
                          string(), string(), [string()]) ->
                                 boolean().
has_src_ebin_dotapp(State, BaseName, BaseDir, SubDirs) ->
    lists:member("ebin", SubDirs) andalso
        lists:member("src", SubDirs) andalso
        sin_utils:file_exists(State,
                              filename:join([BaseDir, "ebin",
                                             BaseName ++
                                                 ".app"])).

-spec has_src_appsrc(sin_state:state(),
                     string(), string(), [string()]) ->
                            boolean().
has_src_appsrc(State, BaseName, BaseDir, SubDirs) ->
    lists:member("src", SubDirs) andalso
        sin_utils:file_exists(State,
                              filename:join([BaseDir, "src",
                                             BaseName ++
                                                 ".app.src"])).


%% @doc Gather the list of modules that currently may need to be built.
gather_modules(SrcDir) ->
    case filelib:is_dir(SrcDir) of
        true ->
            filelib:fold_files(SrcDir,
                               "(.+\.erl|.+\.yrl|.+\.asn1|.+\.asn)$",
                   true, % Recurse into subdirectories of src
                   fun(File, Acc) ->
                           Ext = filename:extension(File),
                           AtomExt = list_to_atom(Ext),
                           TestImplementations =
                               case string:str(File, "_SUITE.erl") of
                                   0 ->
                                       [];
                                   _ ->
                                       [common_test]
                                end,
                           [{File, module_name(File), Ext, AtomExt,
                             TestImplementations} | Acc]
                   end, []);
        false ->
             []
     end.

%% @doc Extract the module name from the file name.
module_name(File) ->
    list_to_atom(filename:rootname(filename:basename(File))).

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
            sin_config:match(project_name, Config)
    end.

%%====================================================================
%% tests
%%====================================================================


