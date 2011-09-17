%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Supports building individual erl files in an application
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%--------------------------------------------------------------------------
-module(sin_task_build).

-behaviour(sin_task).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%% API
-export([description/0,
         do_task/2,
         gather_fail_info/2,
         strip_options/1,
         get_target/4,
         format_exception/1]).

-record(env,  {project_dir,
               build_dir,
               apps_build_dir,
               sig_dir,
               app_list,
               deps}).

-define(TASK, build).
-define(DEPS, [prepare, depends]).

%%====================================================================
%% API
%%====================================================================

%% @doc return a description of this task to the caller
-spec description() -> sin_task:task_description().
description() ->

    Desc = "This command builds all of the source files for all of the otp
    applications in the project. Currently, it support erl and yrl
    files. <break> <break> It currently supports the following configuration
    options: <break> {compile_args, List}. <break> <break> This is a list of
    compile argumnents to be used when building the system. See the erlang
    compile module for details of what compile args are available. <break>
    <break> {print_args, true | false}. <break> <break> This is a boolean
    indicating whether or not you would like to see the args used on each file
    as it is compiled. Defaults to false. <break> <break> You may specialize any
    of these arguments on the release, application or the module or any mix of
    those three things. For example, lets say you had application foo and bar
    and you wanted to pass debug info to application foo but not to
    application. That could be accomplishied as follows: <break> <break>
    {compile_args, [{app, foo}], [debug_info]}. <break> {compile_args, [{app,
    bar}], []}. <break> <break> You may also specify this down to the module
    level. So lets say that in application bar you want to specifiy debug info
    for the module 'my_important_module' you could do that with a module
    speciliazation as follows: <break> <break> {compile_args, [{app, bar},
    {module, my_important_module}], [debug_info]}. <break> <break> In this case,
    if you have both the latter configuration options, the compile argument
    'debug_info' will only be passed to the module 'my_important_module' in
    application bar and not to any other module in application bar. ",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "build",
          short_desc = "compiles the files in the project",
          desc = Desc,
          opts = []}.

%% @doc run the build task.
-spec do_task(sin_config:config(), sin_state:state()) -> sin_state:state().
do_task(Config, State) ->
    ensure_build_dir(State),
    Apps = sin_state:get_value(project_allapps, State),
    NApps = reorder_apps_according_to_deps(State, Apps),
    build_apps(Config, State, NApps).

%% @doc Gather up all the errors and warnings for output.
-spec gather_fail_info([term()], string()) ->
    [string()].
gather_fail_info(ListOfProblems, Type) ->
    R = lists:foldr(fun ({File, Problems}, Acc) ->
                            gather_fail_info(File, Problems, Acc, Type)
                    end, [], ListOfProblems),
    lists:reverse(R).

%% @doc Strip options for the yecc. Otherwise we get a bad arg error.
strip_options(Opts) ->
    lists:foldr(
      fun (Opt = {parserfile, _}, Acc) ->
              [Opt | Acc];
          (Opt = {includefile, _}, Acc) ->
              [Opt | Acc];
          (Opt = {report_errors, _}, Acc) ->
              [Opt | Acc];
          (Opt = {report_warnings, _}, Acc) ->
              [Opt | Acc];
          (Opt = {report, _}, Acc) ->
              [Opt | Acc];
          (Opt = {return_warnings, _}, Acc) ->
              [Opt | Acc];
          (Opt = {verbose, _}, Acc) ->
              [Opt | Acc];
          (_, Acc) ->
              Acc
      end, [], Opts).


%% @doc Check to see if the file needs building. If it does run the passed in
%% build fin. If thats successful then update the sig.
get_target(FileName, Ext, TargetDir, TargetExt) ->
    Name = filename:basename(FileName, Ext),
    NewFile = lists:flatten([Name, TargetExt]),
    filename:join([TargetDir, NewFile]).


%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Given a list of apps and dependencies creates an ordered build list for
%% the apps.
-spec reorder_apps_according_to_deps(sin_state:state(),
                                     [AppInfo::term()]) -> list().
reorder_apps_according_to_deps(State, AllApps) ->
    ReOrdered = lists:foldr(
                  fun ({App, _, {Deps, IncDeps}, _}, Acc) ->
                          AllDeps = Deps++IncDeps,
                          case map_deps(App, AllDeps, AllApps) of
                              [] ->
                                  [{'NONE', App} | Acc];
                              Else ->
                                  Else ++ Acc
                          end
                  end, [], AllApps),
    case sin_topo:sort(ReOrdered) of
        {ok, DepList} ->
            DepList;
        {cycle, CycleList} ->
            ?SIN_RAISE(State, cycles_detected,
                       "A cycle was detected in the dependency graph "
                       " I don't know how to build cycles. ~p",
                       [CycleList])
    end.

%% @doc Map the lists of dependencies for 'App' into a pairs for the topo sort.
-spec map_deps(atom(), list(), list()) -> list().
map_deps(App, Deps, AllApps) ->
         lists:foldr(fun (DApp, Acc) ->
                             case lists:keymember(DApp, 1, AllApps) of
                                 true ->
                                     [{DApp, App} | Acc];
                                 false ->
                                     Acc
                             end
                     end, [], Deps).

%% @doc Build the apps in the list.
build_apps(Config, State, Apps) ->
    AppList = sin_state:get_value(project_allapps, State),
    AllDeps = sin_state:get_value(project_alldeps, State),
    ProjectDir = sin_state:get_value(project_dir, State),
    BuildDir = sin_state:get_value(build_dir, State),
    AppBDir = filename:join([BuildDir, "apps"]),
    SigDir = filename:join([BuildDir, "sigs"]),
    build_apps(Config, State, #env{project_dir=ProjectDir,
                                   build_dir=BuildDir,
                                   apps_build_dir=AppBDir,
                                   sig_dir=SigDir,
                                   app_list=AppList,
                                   deps=AllDeps},
               Apps).

%% @doc build the apps as they come up in the list.
build_apps(Config, State0, BuildSupInfo, AppList) ->
    Cache0 = dict:new(),
    {_Cache, State1} =
        lists:foldl(fun ('NONE', Acc) ->
                            % We ignore an app type of none, its a remnent
                            % of the reorder process.
                            Acc;
                        (App, {Cache1, State1}) ->
                            build_app(Config, State1, Cache1, BuildSupInfo, App)
                    end, {Cache0, State0}, AppList),
    State1.

%% @doc Build an individual otp application.
build_app(Config0, State0, Cache0, Env, AppName) ->
    Config1 = Config0:specialize([{app, AppName}]),
    AppBuildDir =
        sin_state:get_value({apps, AppName, builddir}, State0),
    AppDir = sin_state:get_value({apps, AppName, basedir}, State0),

    Target = filename:join([AppBuildDir, "ebin"]),

    {EbinPaths, Includes} = setup_code_path(State0, Env, AppName),

    code:add_patha(Target),
    {Cache1, FileList, State1} = process_source_files(State0,
                                                      Cache0,
                                                      Includes,
                                                      gather_modules(State0,
                                                                     AppName)),
    State2 = sin_state:store([{{apps, AppName, code_paths},
                               [Target | EbinPaths]},
                              {{apps, AppName, file_list},
                               FileList}],
                             State1),

    State3 = build_sources(Config1, State2, FileList,
                           Includes, AppDir, Target),

    {Cache1, State3}.

%% @doc go through each source file building with the correct build module.
-spec build_sources(sin_config:config(), sin_state:state(), [tuple()], [string()],
                    string(), string()) ->
    ok.
build_sources(Config0, State0, FileList, Includes, AppDir,  Target) ->
    lists:foldl(fun({{File, Module, _, _, _},
                     {changed, _, _, BuildModule}}, State1) ->
                      Config1 = Config0:specialize([{module, Module}]),
                      Options = Config1:match(compile_args, [])
                          ++ [{outdir, Target},
                              strict_record_tests,
                              return_errors, return_warnings,
                              {i, filename:join([AppDir, "include"])},
                              %% Search directory with .hrl files
                              Includes],
                      event_compile_args(Config1, Options),
                      State2 = BuildModule:build_file(Config1, State1,
                                                      File, Options, Target),
                      update_changed(File, State2);
                 (_, State1) ->
                        State1
              end, State0, FileList).

%% @doc if the right config values is specified event the compile args
event_compile_args(Config, Options) ->
        case Config:match(print_args, undefined) of
            undefined ->
                ok;
            true ->
                ewl_talk:say("Compile args:~n~p", [Options]);
            "True" ->
                ewl_talk:say("Compile args:~n~p", [Options])
         end.

%% @doc process dependencies for each source file, the sort the list in build
%% order.
process_source_files(State0, Cache,
                     Includes, FileList) ->
   {NewCache, TmpNewList, State3} =
        lists:foldl(fun(FileInfo, {Cache2, Acc, State1}) ->
                            {Cache3, NewFileInfo, State2} =
                                process_source_file(State1,
                                                    Cache2,
                                                    FileList,
                                                    Includes,
                                                    FileInfo),
                            {Cache3, [NewFileInfo | Acc], State2}
                    end, {Cache, [], State0}, FileList),
    {NewCache, topo_sort_file_list(TmpNewList), State3}.

topo_sort_file_list(FileList) ->
    {ok, Data} =
        sin_topo:sort(
          lists:flatten(
            lists:map(fun({{File, _, _, _, []}, _}) ->
                              {'NONE', File};
                         ({{File, _, _, _, Deps}, _}) ->
                              lists:map(fun({DepFile, _}) ->
                                                {DepFile, File}
                                        end, Deps)
                      end,
                      FileList))),
    lists:reverse(
      lists:foldl(fun('NONE', Acc) ->
                          Acc;
                     (File, Acc) ->
                          case ec_lists:find(fun({{LFile, _, _, _, _}, _}) ->
                                                     LFile == File
                                             end, FileList) of
                              error ->
                                  Acc;
                              {ok, Item} ->
                                  [Item | Acc]
                          end
                  end,
                  [], Data)).

process_source_file(State0,
                    Cache,
                    FileList,
                    Includes,
                    {File, Module, Ext, AtomExt, TestImplementations}) ->
    BuildModule = get_build_module(State0, AtomExt),
    {Changed, {NewCache, {Deps, NewTI, TestedModules}}, State1} =
        has_changed(State0, Cache, File, Includes,
                    TestImplementations,
                    FileList),
    {NewCache, {{File, Module, Ext, AtomExt, Deps},
                {Changed, NewTI, TestedModules, BuildModule}}, State1}.

has_changed(State0, Cache, File, Includes,
            TestImplementations,
            FileList) ->
        case contents_changed(File, State0) of
            true ->
                {Ret, State1} = save_real_dependencies(State0,
                                                       Cache,
                                                       FileList, TestImplementations,
                                                       File, Includes),
                {changed, Ret, State1};
            false ->
                dependencies_have_changed(State0, Cache, FileList,
                                          TestImplementations, File, Includes)
        end.

dependencies_have_changed(State0, Cache,
                          FileList, TestImplementations,
                          File, Includes) ->
        case sin_sig:get_sig_info(File, State0) of
            error ->
                {Ret, State1} = save_real_dependencies(State0,
                                                       Cache, FileList,
                                                       TestImplementations, File,
                                                       Includes),
                {changed, Ret, State1};
            {ok, Terms}  ->
                case check_deps_for_change(Terms) of
                    true ->
                        {Ret, State1} = save_real_dependencies(State0,
                                                               Cache,
                                                               FileList,
                                                               TestImplementations,
                                                               File,
                                                               Includes),
                        {changed, Ret, State1};
                    false ->
                        {not_changed, {Cache, Terms}, State0}
                end
        end.

check_deps_for_change({Deps, _, _}) ->
   check_deps_for_change(Deps);
check_deps_for_change([{File, TS} | Rest]) ->
    case file:read_file_info(File) of
        {ok, TargetInfo}
        when TargetInfo#file_info.mtime > TS ->
            true;
        _ ->
            check_deps_for_change(Rest)
    end;
check_deps_for_change([]) ->
    false.

save_real_dependencies(State0,
                       Cache,
                       FileList, TestImplementations, File, Includes) ->
    {NewCache, Dependencies} = resolve_dependencies(State0,
                                                    Cache, FileList,
                                                    TestImplementations,
                                                    File,
                                                    Includes),
    State1 = sin_sig:save_sig_info(File, Dependencies, State0),
    {{NewCache,  Dependencies}, State1}.

resolve_dependencies(State,
                     Cache, FileList, TestImplementations,  File, Includes) ->
    {DepArtifacts, NewTestImplementations, TestedModules} =
        parse_attribute_dependencies(TestImplementations, File, Includes),

    {NewCache, NewDeps} = realize_dependent_artifacts(State,
                                                      Cache, DepArtifacts,
                                                      FileList),
    {NewCache, {NewDeps,
                NewTestImplementations, TestedModules}}.

realize_dependent_artifacts(State, Cache, DepArtifacts, FileList) ->
    lists:foldl(fun({file, File}, {Cache2, Acc}) ->
                        {Cache3, TS} = get_ts(State, Cache2, File),
                       {Cache3, [{File, TS} | Acc]};
                  ({module, Module}, {Cache2, Acc}) ->
                       File = realize_module(State, Module, FileList),
                       {Cache3, TS} = get_ts(State, Cache2, File),
                       {Cache3, [{File, TS} | Acc]}
              end, {Cache, []}, DepArtifacts).

get_ts(State, Cache, File) ->
    case dict:find({ts, File}, Cache) of
        {ok, TS} ->
            {Cache, TS};
        error ->
            TS = case file:read_file_info(File) of
                {ok, FileInfo} ->
                         FileInfo#file_info.mtime;
                     Error ->
                         ?SIN_RAISE(State, {error_reading_timestamp, File, Error})
                 end,
            {dict:store({ts, File}, TS, Cache), TS}
    end.
realize_module(State, Module, FileList) ->
    case lists:keyfind(Module, 2, FileList) of
        {File, Module, _, _, _} ->
            File;
        false ->
            %% Its a dependent file then, somewhere in the code path
            case code:which(Module) of
                FileName when is_list(FileName) ->
                    FileName;
                _ ->
                    ?SIN_RAISE(State, {dependent_module_not_found, Module})
            end
    end.

parse_attribute_dependencies(TestImplementations, File, Includes) ->
    %% Drop off the first attribute, we don't care so much about that, it just
    %% points to itself.
    {ok, Forms} =
        epp:parse_file(File, Includes, []),
    lists:foldl(fun(Attr, Acc) ->
                        parse_form(File, Attr, Acc)
                end,
                {[], TestImplementations, []},
                Forms).

parse_form(_File, {attribute, _ , file, {[], _}}, Acc) ->
    Acc;
parse_form(File, {attribute, _ , file, {File, _}}, Acc) ->
    Acc;
parse_form(_File, {attribute, _ , file, {Include, _}},
           {Deps, TestImplementations, TestedModules}) ->
    {[{file, Include} | Deps], TestImplementations, TestedModules};
parse_form(_File, {attribute, _, compile, {parse_transform, proper_transformer}},
           {Deps, TestImplementations, TestedModules}) ->
    {Deps, [proper | TestImplementations], TestedModules};
parse_form(_File, {attribute, _, compile, {parse_transform, eunit_autoexport}},
           {Deps, TestImplementations, TestedModules}) ->
    {Deps, [eunit | TestImplementations], TestedModules};
parse_form(_File, {attribute, _, compile, {parse_transform, Module}},
           {Deps, TestImplementations, TestedModules}) ->
    {[{module, Module} | Deps], TestImplementations, TestedModules};
parse_form(_File, {attribute, _, behaviour, Module},
           {Deps, TestImplementations, TestedModules}) ->
    {[{module, Module} | Deps], TestImplementations,
     TestedModules};
parse_form(_File, {attribute, _, behavior, Module},
           {Deps, TestImplementations, TestedModules}) ->
    {[{module, Module} | Deps], TestImplementations,
     TestedModules};
parse_form(_File, {attribute, _, tested_modules, ModList},
          {Deps, TestImplementations, TestedModules}) ->
    {Deps, TestImplementations, ModList ++ TestedModules};
parse_form(_, _, Acc) ->
    Acc.

contents_changed(File, State) ->
    case sin_sig:changed(build, File, State) of
        false ->
            false;
        _ ->
            true
    end.

update_changed(File, State) ->
    sin_sig:update(build, File, State).

get_build_module(_, '.erl') ->
    sin_compile_erl;
get_build_module(_, '.yrl') ->
    sin_compile_yrl;
get_build_module(State, Ext) ->
    ?SIN_RAISE(State, {unsupported_file_type, Ext}).

%% @doc Gather code paths and includes from the dependency list.
setup_code_path(State, Env, AppName) ->
    case get_app_from_list(AppName, Env#env.app_list) of
        not_in_list ->
            ?SIN_RAISE(State, app_name_not_in_list,
                       "App ~s is not in the list of project apps. "
                       "This shouldn't happen!!",
                       [AppName]);
        {_, _, {Deps, IncDeps}, _} ->
            get_compile_time_deps(State,
                                  extract_info_from_deps(State,
                                                         Deps ++ IncDeps,
                                                         element(1, Env#env.deps),
                                                         [], [], []))
        end.
%% @doc gather up the static compile time dependencies
get_compile_time_deps(State, {Acc, IAcc}) ->
    lists:foldl(fun({_, _, _, Path}, {NA, NIA}) ->
                        Ebin = filename:join([Path, "ebin"]),
                        Include = {i, filename:join([Path, "include"])},
                        {[Ebin | NA], [Include | NIA]}
                end,
                {Acc, IAcc},
                sin_state:get_value(project_compile_deps, State)).

%% @doc Gather path and include information from the dep list.
extract_info_from_deps(State, [AppName | T],
                       AppList, Marked, Acc, IAcc) ->
    case lists:member(AppName, Marked) of
        false ->
            case get_app_from_list(AppName, AppList) of
                not_in_list ->
                    ?SIN_RAISE(State,
                               app_name_not_in_list,
                               "App ~s is not in the list of project apps. "
                               "This shouldn't happen!!!",
                               [AppName]);
                {_, _, {Deps, IncDeps}, Path} ->
                    Ebin = filename:join([Path, "ebin"]),
                    Include = {i, filename:join([Path, "include"])},
                    code:add_patha(Ebin),
                    extract_info_from_deps(State, T, AppList ++ Deps ++
                                           IncDeps,
                                           Marked,
                                           [Ebin | Acc],
                                           [Include | IAcc])
            end;
        true ->
            extract_info_from_deps(State, T, AppList, Marked, Acc,
                                   IAcc)
    end;
extract_info_from_deps(_, [], _, _, Acc, IAcc) ->
    {Acc, IAcc}.

%% @doc Get the app from the app list.
get_app_from_list(App, AppList) ->
    case lists:keysearch(App, 1, AppList) of
        {value, Entry} ->
            Entry;
        false ->
            not_in_list
    end.

%% @doc Gather the list of modules that currently may need to be built.
gather_modules(State, AppName) ->
    sin_state:get_value({apps, AppName, all_modules_detail}, State).


%% @doc Ensure that the build dir exists and is ready to accept files.
ensure_build_dir(State) ->
    BuildDir = sin_state:get_value(build_dir, State),
    AppsDir = lists:flatten([BuildDir, "apps", "tmp"]),
    filelib:ensure_dir(AppsDir).


%% @doc Actual get the failer detail information and add it to the accumulator.
gather_fail_info(File, ListOfProblems, Acc, WoE) ->
    lists:foldl(
      fun ({Line, Type, Detail}, Acc1) when is_atom(Line) ->
              [lists:flatten([File, $:, atom_to_list(Line),
                              $:, WoE, $:, Type:format_error(Detail),
                              $\n]) | Acc1];
          ({Line, Type, Detail}, Acc1) when is_integer(Line) ->
              [lists:flatten([File, $:, integer_to_list(Line),
                              $:, WoE, $:, Type:format_error(Detail),
                              $\n]) | Acc1];
          ({Type, Detail}, Acc1) ->
              [lists:flatten([File, ":noline:", WoE, $:,
                              Type:format_error(Detail), $\n]) | Acc1]
      end, Acc, ListOfProblems).

%%====================================================================
%% Tests
%%====================================================================
reorder_app_test() ->
    AppList = [{app1, "123", {[app2, stdlib], []}, "path"},
               {app2, "123", {[app3, kernel], []}, "path"},
               {app3, "123", {[kernel], []}, "path"}],
    NewList  = reorder_apps_according_to_deps(sin_config:new(), AppList),
    ?assertMatch(['NONE', app3, app2, app1], NewList),
    AppList2 = [{app1, "123", {[app2, zapp1, stdlib], []}, "path"},
                {app2, "123", {[app3, kernel], []}, "path"},
                {app3, "123", {[kernel, zapp2], []}, "path"},
                {zapp1, "vsn", {[app2, app3, zapp2], []}, "path"},
                {zapp2, "vsn", {[kernel], []}, "path"},
                {zapp3, "vsn", {[], []}, "path"}],
    NewList2 = reorder_apps_according_to_deps(sin_config:new(), AppList2),
    ?assertMatch(['NONE', zapp2, app3, app2, zapp1, app1,
                  zapp3], NewList2).



