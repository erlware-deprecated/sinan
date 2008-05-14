%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Erlware
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
%%%  Supports building individual erl files in an application
%%% @end
%%% @copyright (C) 2007, Erlware
%%%--------------------------------------------------------------------------
-module(sin_erl_builder).

-behaviour(eta_gen_task).

-include("file.hrl").
-include("etask.hrl").
-include("eunit.hrl").


%% API
-export([start/0, do_task/1, build/1]).


-record(env,  {project_dir,
               build_dir,
               apps_build_dir,
               sig_dir,
               app_list,
               deps,
               repo}).

-define(TASK, build).
-define(DEPS, [check_depends]).



%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Compiles all of the compilable files in the project",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = []},
    eta_task:register_task(TaskDesc).

%%--------------------------------------------------------------------
%% @doc
%%  Do the task defined in this module.
%% @spec do_task(BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    build(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  run the build task.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
build(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK),
    ensure_build_dir(BuildRef),
    Apps = sin_build_config:get_value(BuildRef, "project.apps"),
    NApps = reorder_apps_according_to_deps(Apps, Apps, []),
    NArgs = [],
    build_apps(BuildRef, NApps, NArgs),
    eta_event:task_stop(BuildRef, ?TASK).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Given a list of apps and dependencies creates an ordered build
%%  list for the apps.
%% @spec (ListOfApps, AllApps, Acc) -> OrderedBuildList
%% @end
%% @private
%%--------------------------------------------------------------------
reorder_apps_according_to_deps([{App, _, {Deps, _}} | T], AllApps, Acc) ->
    case map_deps(App, Deps, AllApps, []) of
        [] ->
            reorder_apps_according_to_deps(T, AllApps,
                                           [{'NONE', to_list(App)} | Acc]);
        Else ->
            reorder_apps_according_to_deps(T, AllApps,
                                           Else ++ Acc)
    end;
reorder_apps_according_to_deps([], _AllApps, Acc) ->
    case eta_topo:sort(Acc) of
        {ok, DepList} ->
            DepList;
        {cycle, CycleList} ->
            ?ETA_RAISE_DA(cycles_detected,
                          "A cycle was detected in the dependency graph "
                          " I don't know how to build cycles. ~p",
                          [CycleList])
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Map the lists of dependencies for 'App' into a pairs for the
%%  topo sort.
%% @spec (App, Deps, AllApps, Acc) -> NAcc
%% @end
%% @private
%%--------------------------------------------------------------------
map_deps(App, [DApp | T], AllApps, Acc) ->
    case in_app_list(DApp, AllApps) of
        true ->
            map_deps(App, T, AllApps, [{to_list(DApp), to_list(App)} | Acc]);
        false ->
            map_deps(App, T, AllApps, Acc)
    end;
map_deps(_App, [], _AllApps, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc
%%  Change an atom to a list of the argument is an atom, otherwise
%%  just return the arg.
%% @spec (Atom) -> List
%% @end
%% @private
%%--------------------------------------------------------------------
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Atom) when is_list(Atom) ->
    Atom.

%%--------------------------------------------------------------------
%% @doc
%%  Check to see if the specified value is in the app list.
%% @spec (App, AppList) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
in_app_list(App, [{App, _, _} | _]) ->
    true;
in_app_list(App, [_H | T]) ->
    in_app_list(App, T);
in_app_list(_App, []) ->
    false.


%%--------------------------------------------------------------------
%% @doc
%%  Build the apps in the list.
%% @spec (BuildRef, Apps, Args) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
build_apps(BuildRef, Apps, Args) ->
    AppList = sin_build_config:get_value(BuildRef, "project.apps"),
    Deps = sin_build_config:get_value(BuildRef, "project.deps"),
    ProjectDir = sin_build_config:get_value(BuildRef, "project.dir"),
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    AppBDir = filename:join([BuildDir, "apps"]),
    SigDir = filename:join([BuildDir, "sigs"]),
    Repo = sin_build_config:get_value(BuildRef, "project.repository"),
    build_apps(BuildRef, #env{project_dir=ProjectDir,
                              build_dir=BuildDir,
                              apps_build_dir=AppBDir,
                              sig_dir=SigDir,
                              app_list=AppList,
                              deps=Deps,
                              repo=Repo},
               Apps, Args).




%%--------------------------------------------------------------------
%% @doc
%%  build the apps as they come up in the list.
%% @spec (BuildRef, BuildSupInfo, AppList, Args) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
build_apps(BuildRef, BuildSupInfo, ['NONE' | T], Args) ->
    %% We ignore an app type of none, its a remnent of the
    %% reorder process.
    build_apps(BuildRef, BuildSupInfo, T, Args);
build_apps(BuildRef, BuildSupInfo, [App | T], Args) ->
    build_app(BuildRef, BuildSupInfo, App, Args),
    build_apps(BuildRef, BuildSupInfo, T, Args);
build_apps(_, _BuildSupInfo, [], _Args) ->
    ok.

%%-------------------------------------------------------------------
%% @doc
%%  Build an individual otp application.
%% @spec (BuildRef, Env, AppName, Args) -> ok
%% @end
%% @private
%%-------------------------------------------------------------------
build_app(BuildRef, Env, AppName, Args) ->
    AppVsn = sin_build_config:get_value(BuildRef, "apps." ++ AppName ++ ".vsn"),
    AppDir = sin_build_config:get_value(BuildRef, "apps." ++ AppName ++ ".basedir"),
    BuildTarget = lists:flatten([AppName, "-", AppVsn]),
    AppBuildDir = filename:join([Env#env.apps_build_dir, BuildTarget]),
    Target = filename:join([AppBuildDir, "ebin"]),
    SrcDir = filename:join([AppDir, "src"]),
    {EbinPaths, Includes} = setup_code_path(BuildRef, Env, AppName),
    sin_build_config:store(BuildRef, "apps." ++ AppName ++ ".code_paths",
                   [Target | EbinPaths]),
    Options = Args ++ [{outdir, Target}, strict_record_tests,
                       return_errors, return_warnings,
                       {i, filename:join([AppDir, "include"])} | Includes],
    Ignorables = sin_build_config:get_value(BuildRef, "ignore_dirs", []),
    sin_utils:copy_dir(AppBuildDir, AppDir, "", Ignorables),
    code:add_patha(Target),
    Modules = gather_modules(BuildRef, AppName, SrcDir),
    NModules = lists:map(fun({File, _AbsName, Ext}) ->
                                 build_file(BuildRef, SrcDir, File, Ext,
                                            Options, Target)
                         end, Modules),
    check_for_errors(NModules),
    sin_utils:remove_code_paths([Target | EbinPaths]).


%%--------------------------------------------------------------------
%% @doc
%%  Check the module list for errors throw an exceptions.
%% @spec (ModuleList) -> ok
%% @end
%%--------------------------------------------------------------------
check_for_errors([{sinan, error} | _]) ->
    ?ETA_RAISE(build_errors);
check_for_errors([_ | T]) ->
    check_for_errors(T);
check_for_errors([]) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Gather code paths and includes from the dependency list.
%% @spec (BuildRef, Env, AppName) -> {Paths, Includes}
%% @end
%% @private
%%--------------------------------------------------------------------
setup_code_path(BuildRef, Env, AppName) ->
    AtomApp = list_to_atom(AppName),
    case get_app_from_list(AtomApp, Env#env.app_list) of
        not_in_list ->
            ?ETA_RAISE_DA(app_name_not_in_list,
                         "App ~s is not in the list of project apps. "
                         "This shouldn't happen!!",
                         [AppName]);
        {_, _, {Deps, _}} ->
            case sin_build_config:get_value(BuildRef, "eunit") of
                "disabled" ->
                    extract_info_from_deps(BuildRef, Deps, Env#env.app_list,
                                           Env#env.repo,
                                           Env#env.apps_build_dir,
                                           Env#env.deps, [], []);
                _ ->
                    extract_info_from_deps(BuildRef, [eunit | Deps],
                                           Env#env.app_list, Env#env.repo,
                                           Env#env.apps_build_dir,
                                           Env#env.deps, [], [])
                end
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Gather path and include information from the dep list.
%% @spec (BuildRef, AppList, AppList, Repo,
%%                       AppBDir, Deps, Acc, IAcc) -> {Paths, Includes}
%% @end
%% @private
%%--------------------------------------------------------------------
extract_info_from_deps(BuildRef, [App | T], AppList, Repo,
                       AppBDir, Deps, Acc, IAcc) ->
    BuildTarget = lists:flatten([atom_to_list(App), "-", get_vsn(App, Deps)]),
    case in_app_list(App, AppList) of
        true ->
            Ebin = filename:join([AppBDir, BuildTarget, "ebin"]),
            Include = {i, filename:join([AppBDir, BuildTarget, "include"])};
        false ->
            Ebin = filename:join([Repo, BuildTarget, "ebin"]),
            Include = {i, filename:join([Repo, BuildTarget, "include"])}
    end,
    code:add_patha(Ebin),
    extract_info_from_deps(BuildRef, T, AppList, Repo, AppBDir, Deps, [Ebin | Acc],
                           [Include | IAcc]);
extract_info_from_deps(_, [], _AppList, _Repo, _AppBDir, _Deps, Acc, IAcc) ->
    {Acc, IAcc}.

%%--------------------------------------------------------------------
%% @doc
%%  Get the version for the app.
%% @spec (AppName, DepList) -> Vsn
%% @end
%% @private
%%--------------------------------------------------------------------
get_vsn(App, [{App, Vsn} | _T]) ->
    Vsn;
get_vsn(App, [_H | T]) ->
    get_vsn(App, T);
get_vsn(App, []) ->
    ?ETA_RAISE_DA(miss_app,
                 "Unable to get the version for ~w. This shouldn't "
                 "happen!",
                 [App]).
%%--------------------------------------------------------------------
%% @doc
%%  Get the app from the app list.
%% @spec (App, AppList) -> Entry | not_in_list
%% @end
%% @private
%%--------------------------------------------------------------------
get_app_from_list(App, [Entry = {App, _, _} | _]) ->
    Entry;
get_app_from_list(App, [_H | T]) ->
    get_app_from_list(App, T);
get_app_from_list(_App, []) ->
    not_in_list.

%%--------------------------------------------------------------------
%% @doc
%%  Gather the list of modules that currently may need to be built.
%% @spec (BuildRef, AppName, SrcDir) -> ModuleList
%% @end
%% @private
%%--------------------------------------------------------------------
gather_modules(BuildRef, AppName, SrcDir) ->
    ModuleList = sin_build_config:get_value(BuildRef,
                                 "apps." ++ AppName ++ ".modules"),
    FileList =
        filelib:fold_files(SrcDir,
                           "(.+\.erl|.+\.yrl|.+\.asn1)$",
                   false,
                   fun(File, Acc) ->
                           Ext = filename:extension(File),
                           [{File,
                             list_to_atom(module_name(filename:basename(File),
                                                      Ext, [])),
                             Ext} | Acc]
                   end, []),
    reorder_list(BuildRef, ModuleList,
                 filter_file_list(BuildRef, FileList, ModuleList, []),
                 []).

%%--------------------------------------------------------------------
%% @doc
%%  Extract the module name from the file name.
%% @spec (Ext, Ext, Acc) -> ModuleName
%% @end
%% @private
%%--------------------------------------------------------------------
module_name(Ext, Ext, Acc) ->
    lists:reverse(Acc);
module_name([H | T], Ext, Acc) ->
    module_name(T, Ext, [H | Acc]).


%%--------------------------------------------------------------------
%% @doc
%%  Reorder the list into dependent modules
%% @spec (BuildRef, Lst, FileList, Acc) -> NewModuleList
%% @end
%%--------------------------------------------------------------------
reorder_list(BuildRef, Lst, FileList, Acc) ->
    reorder_list(BuildRef, Lst, FileList, Acc, ok).

%%--------------------------------------------------------------------
%% @doc
%%  Reorder the list according to whats in the *.app. This will
%% allow intra application compile time dependencies.
%% @spec (BuildRef, ModList, FileList, Acc, OkFlag) -> NewList
%% @end
%% @private
%%--------------------------------------------------------------------
reorder_list(BuildRef, [H | T], FileList, Acc, OkFlag) ->
    case get_file_list(H, FileList) of
        not_in_list ->
            eta_event:task_fault(BuildRef, ?TASK,
                                 {"The module specified by ~w is not "
                                  "on the filesystem!! Not building.", [H]}),
            reorder_list(BuildRef, T, FileList, Acc, not_ok);
        Entry ->
            reorder_list(BuildRef, T, FileList, [Entry | Acc], OkFlag)
    end;
reorder_list(_, [], _FileList, Acc, ok) ->
    lists:reverse(Acc);
reorder_list(_, [], _FileList, _, not_ok) ->
    ?ETA_RAISE(build_errors).

%%--------------------------------------------------------------------
%% @doc
%%  Get the entry specified by name from the list in module list.
%% @spec (ModuleName, FileList) -> Entry | not_in_list
%% @end
%% @private
%%--------------------------------------------------------------------
get_file_list(Name, [Entry = {_, Name, _} | _T]) ->
    Entry;
get_file_list(Name, [_ | T]) ->
    get_file_list(Name, T);
get_file_list(_Name, []) ->
    not_in_list.

%%--------------------------------------------------------------------
%% @doc
%%  Filter the list of files keeping those that are in the
%%  module list.
%% @spec (BuildRef, FileList, ModuleList, Acc) -> NewFileList
%% @end
%% @private
%%--------------------------------------------------------------------
filter_file_list(BuildRef, [Entry = {File, AbsName, _} | T], ModuleList, Acc) ->
    case lists:member(AbsName, ModuleList) of
        true ->
            filter_file_list(BuildRef, T, ModuleList, [Entry | Acc]);
        false ->
            eta_event:task_event(BuildRef, ?TASK, module_missing,
                                 {"Module (~w) in file ~s is not in the "
                                  "module list. Removing from build queue.",
                                  [AbsName, File]}),
            filter_file_list(BuildRef, T, ModuleList, Acc)
    end;
filter_file_list(_, [], _ModuleList, Acc) ->
    Acc.

%%-------------------------------------------------------------------
%% @doc
%%    Build the file specfied by its arguments
%% @spec (BuildDir, SrcDir, File, Ext, Options, Target) -> ok
%% @end
%% @private
%%-------------------------------------------------------------------
build_file(BuildRef, SrcDir, File, Ext, Options, Target) ->
    FileName = filename:join([SrcDir, File]),
    build_file(BuildRef, FileName, Ext, Options, Target).

%%-------------------------------------------------------------------
%% @doc
%%   Do the actual compilation on the file.
%% @spec (BuildRef, File, Ext, Options, Target) -> ErrInfo
%% @end
%% @private
%%-------------------------------------------------------------------
build_file(BuildRef, File, ".erl", Options, Target) ->
   case needs_building(File, ".erl", Target, ".beam") of
       true ->
           eta_event:task_event(BuildRef, ?TASK, file_build,
                                {"Building ~s", [File]}),
           case compile:file(File, Options) of
               {ok, ModuleName} ->
                   ModuleName;
               {ok, ModuleName, []} ->
                   ModuleName;
               {ok, ModuleName, Warnings} ->
                  eta_event:task_event(BuildRef, ?TASK, file_warning,
                                       gather_fail_info(Warnings, [], "warning")),
                   ModuleName;
               {error, Errors, Warnings} ->
                   eta_event:task_event(BuildRef, ?TASK, file_error,
                                        [gather_fail_info(Errors, [], "error"),
                                         gather_fail_info(Warnings, [], "warning")]),
                  {sinan,  error};
               error ->
                   eta_event:task_fault(BuildRef, ?TASK,
                                        "Unknown error occured during build"),
                   {sinan, error}
           end;
       false ->
           ok
   end;
build_file(BuildRef, File, ".yrl", Options, Target) ->
    case needs_building(File, ".yrl", Target, ".erl") of
        true ->
            ErlFile = filename:basename(File, ".yrl"),
            ErlName = filename:join([Target,
                                     lists:flatten([ErlFile, ".erl"])]),
            eta_event:task_event(BuildRef, ?TASK, file_build,
                                 {"Building ~s", [File]}),
            case yecc:file(File, [{parserfile, ErlName} |
                                  strip_options(Options, [])]) of
                {ok, _ModuleName} ->
                    build_file(BuildRef, ErlName, ".erl",
                               Options, Target);
                {ok, _ModuleName, []} ->
                    build_file(BuildRef, ErlName, ".erl",
                               Options, Target);
                {ok, _ModuleName, Warnings} ->
                    eta_event:task_event(BuildRef, ?TASK, file_warning,
                                         gather_fail_info(Warnings, [], "warning")),
                    ok;
                {error, Errors, Warnings} ->
                    eta_event:task_event(BuildRef, ?TASK, file_error,
                                         [gather_fail_info(Errors, [], "error"),
                                          gather_fail_info(Warnings, [], "warning")]),
                    error
            end;
        false ->
            ok
    end;
build_file(BuildRef, File, _, _Options, _Target) ->
    eta_event:task_event(BuildRef, ?TASK, file_error,
                         {"Got file ~s with an extention I do not know how to build. "
                          "Ignoring!",
                          [File]}).

%%--------------------------------------------------------------------
%% @doc
%%  Strip options for the yecc. Otherwise we get a bad arg error.
%%
%% @spec (Opts, Acc) -> Acc2
%% @end
%%--------------------------------------------------------------------
strip_options([Opt = {parserfile, _} | T], Acc) ->
    strip_options(T, [Opt | Acc]);
strip_options([Opt = {includefile, _} | T], Acc) ->
    strip_options(T, [Opt | Acc]);
strip_options([Opt = {report_errors, _} | T], Acc) ->
    strip_options(T, [Opt | Acc]);
strip_options([Opt = {report_warnings, _} | T], Acc) ->
    strip_options(T, [Opt | Acc]);
strip_options([Opt = {report, _} | T], Acc) ->
    strip_options(T, [Opt | Acc]);
strip_options([Opt = {return_warnings, _} | T], Acc) ->
    strip_options(T, [Opt | Acc]);
strip_options([Opt = {verbose, _} | T], Acc) ->
    strip_options(T, [Opt | Acc]);
strip_options([_ | T], Acc) ->
    strip_options(T, Acc);
strip_options([], Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc
%%   Check to see if the file needs building. If it does run the
%%   passed in build fin. If thats successful then update the sig.
%% @spec (FileName, Ext, TargetDir, TargetExt)
%%   -> true | false
%% @end
%%--------------------------------------------------------------------
needs_building(FileName, Ext, TargetDir, TargetExt) ->
    Name = filename:basename(FileName, Ext),
    NewFile = lists:flatten([Name, TargetExt]),
    TFileName = filename:join([TargetDir, NewFile]),
    sin_sig:target_changed(FileName, TFileName).



%%--------------------------------------------------------------------
%%
%% @doc
%%  Ensure that the build dir exists and is ready to accept files.
%% @spec (BuildRef) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
ensure_build_dir(BuildRef) ->
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    AppsDir = lists:flatten([BuildDir, "apps", "tmp"]),
    filelib:ensure_dir(AppsDir).



%%-------------------------------------------------------------------
%% @spec (ListOfProblems, Acc, Type) -> Acc2
%% @doc
%%   Gather up all the errors and warnings for output.
%% @end
%% @private
%%-------------------------------------------------------------------
gather_fail_info([{File, ListOfProblems} | T], Acc, Type) ->
    gather_fail_info(T, gather_fail_info(File, ListOfProblems, Acc, Type),
                     Type);
gather_fail_info([], Acc, _Type) ->
    lists:reverse(Acc).

%%-------------------------------------------------------------------
%% @doc
%%  Actual get the failer detail information and add it to the
%%  accumulator.
%% @spec (File, ListOfProblems, Acc, WoE) -> Acc2
%% @end
%% @private
%%-------------------------------------------------------------------
gather_fail_info(File, [{Line, Type, Detail} | T], Acc, WoE)
  when is_atom(Line) ->
    gather_fail_info(File, T, [lists:flatten([File, $:,
                                              atom_to_list(Line),
                                              $:, WoE, $:,
                                              Type:format_error(Detail),
                                              $\n]) | Acc], WoE);
gather_fail_info(File, [{Line, Type, Detail} | T], Acc, WoE)
  when is_integer(Line) ->
    gather_fail_info(File, T, [lists:flatten([File, $:,
                                              integer_to_list(Line),
                                              $:, WoE, $:,
                                              Type:format_error(Detail),
                                              $\n]) | Acc], WoE);
gather_fail_info(File, [{Type, Detail} | T], Acc, WoE) ->
    gather_fail_info(File, T, [lists:flatten([File,
                                              ":noline:", WoE, $:,
                                              Type:format_error(Detail),
                                              $\n]) | Acc], WoE);
gather_fail_info(_File, [], Acc, _) ->
    Acc.

%%====================================================================
%% Tests
%%====================================================================
reorder_app_test() ->
    AppList = [{app1, "123", {[app2, stdlib], undefined}},
               {app2, "123", {[app3, kernel], undefined}},
               {app3, "123", {[kernel], undefined}}],
    NewList  = reorder_apps_according_to_deps(AppList, AppList, []),
    ?assertMatch(['NONE', "app3", "app2", "app1"], NewList),
    AppList2 = [{app1, "123", {[app2, zapp1, stdlib], undefined}},
                {app2, "123", {[app3, kernel], undefined}},
                {app3, "123", {[kernel], undefined}},
                {zapp1, "vsn", {[app2, app3, zapp2], undefined}},
                {zapp2, "vsn", {[kernel], undefined}},
                {zapp3, "vsn", {[], undefined}}],
    NewList2 = reorder_apps_according_to_deps(AppList2, AppList2, []),
    ?assertMatch(['NONE', "zapp2", "app3", "app2", "zapp1", "zapp3", "app1"],
                 NewList2).




