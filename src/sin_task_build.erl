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
-include_lib("sinan/include/sinan.hrl").

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
               includes,
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
do_task(Config, State0) ->
    ensure_build_dir(State0),
    Apps0 = sin_state:get_value(release_apps, State0),
    NApps = reorder_apps_according_to_deps(State0, Apps0),
    {State1, Apps1} = build_apps(Config, State0, NApps),
    sin_state:store(release_apps, Apps1, State1).

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
                                     [sinan:app()]) -> list().
reorder_apps_according_to_deps(State, AllApps) ->
    ReOrdered = lists:foldr(
                  fun (#app{name=AppName}, Acc) ->
                          AllDeps = sin_state:get_value({apps, AppName, deps}, State),
                          case map_deps(AppName, AllDeps, AllApps) of
                              [] ->
                                  [{'NONE', AppName} | Acc];
                              Else ->
                                  Else ++ Acc
                          end
                  end, [], AllApps),
    case sin_topo:sort(ReOrdered) of
        {ok, DepList} ->
            remap_info(State, DepList, AllApps);
        {cycle, CycleList} ->
            ?SIN_RAISE(State, cycles_detected,
                       "A cycle was detected in the dependency graph "
                       " I don't know how to build cycles. ~p",
                       [CycleList])
    end.

-spec remap_info(sin_state:state(), [atom()], [sinan:app()]) ->
                        [sinan:app()].
remap_info(State, ['NONE' | Rest], AllApps) ->
    remap_info(State, Rest, AllApps);
remap_info(State, Rest, AllApps) ->
    lists:map(fun(AppName0) ->
                      case ec_lists:find(fun(#app{name=AppName1}) ->
                                                 AppName1 == AppName0
                                         end, AllApps) of
                          {ok, App} ->
                              App;
                          error ->
                              ?SIN_RAISE(State, {unable_to_find_app_info,
                                                 this_shouldnt_happen, AppName0})
                      end
              end, Rest).


%% @doc Map the lists of dependencies for 'App' into a pairs for the topo sort.
-spec map_deps(atom(), [atom()], [sinan:app()]) -> list().
map_deps(App, Deps, AllApps) ->
         lists:foldr(fun (DApp, Acc) ->
                             case lists:any(fun(#app{name=AppName}) ->
                                                    AppName == DApp
                                            end, AllApps) of
                                 true ->
                                     [{DApp, App} | Acc];
                                 false ->
                                     Acc
                             end
                     end, [], Deps).

%% @doc Build the apps in the list.
build_apps(Config, State, Apps) ->
    AppList = sin_state:get_value(release_apps, State),
    AllDeps = sin_state:get_value(release_deps, State),
    ProjectDir = sin_state:get_value(project_dir, State),
    BuildDir = sin_state:get_value(build_dir, State),
    AppBDir = filename:join([BuildDir, "apps"]),
    SigDir = filename:join([BuildDir, "sigs"]),

    Includes =
        lists:map(fun(#app{path=Path}) ->
                          true = code:add_patha(filename:join(Path, "ebin")),
                          filename:join(Path, "includes")
                  end, AllDeps),

    build_apps(Config, State, #env{project_dir=ProjectDir,
                                   build_dir=BuildDir,
                                   apps_build_dir=AppBDir,
                                   sig_dir=SigDir,
                                   app_list=AppList,
                                   includes=Includes,
                                   deps=AllDeps},
               Apps).

%% @doc build the apps as they come up in the list.
build_apps(Config, State0, BuildSupInfo, AppList) ->
        lists:foldl(fun(App0, {State1, Apps0}) ->
                            {State2, App1} = build_app(Config, State1, BuildSupInfo, App0),
                            {State2, [App1 | Apps0]}
                    end, {State0, []}, AppList).

%% @doc Build an individual otp application.
build_app(Config0, State0, Env, App=#app{name=AppName,
                                         path=AppBuildDir,
                                         sources=Modules0}) ->
    Config1 = Config0:specialize([{app, AppName}]),
    AppDir = sin_state:get_value({apps, AppName, basedir}, State0),

    Target = filename:join([AppBuildDir, "ebin"]),

    Includes = Env#env.includes,

    code:add_patha(Target),

    {State3, NewModules} =
        lists:foldl(fun(Module, {State1, Modules1}) ->
                            {State2, Mods} =
                                build_source_if_required(Config1, State1,
                                                         Module,
                                                         Includes, AppDir,
                                                         Target, Modules0),
                            {State2, [Mods | Modules1]}
                    end, {State0, []}, Modules0),
    {State3, App#app{modules=lists:flatten(NewModules)}}.


%% @doc go through each source file building with the correct build module.
build_source_if_required(Config0, State0,
                         Module=#module{name=Name,
                                        type=Type,
                                        change_sig=NewSig},
              Includes, AppDir,  Target, AllModules) ->
    case Type of
        T when T == yrl; T == erl ->
            case sin_sig:get_sig_info({?MODULE, Name}, State0) of
                {ok, Sig} ->
                    case Sig == NewSig of
                        true ->
                            {ok, Modules} = sin_sig:get_sig_info({?MODULE, modules, Name}, State0),
                            {State0, Modules};
                        false ->
                            do_build_source(Config0, State0, Module, Includes, AppDir,  Target, AllModules)
                    end;
                _ ->
                    do_build_source(Config0, State0, Module, Includes, AppDir,  Target, AllModules)
            end;
        _ ->
            {State0, [Module]}
    end.

do_build_source(Config0, State0, Module = #module{name=Name, module_deps=DepModules},
                Includes, AppDir,  Target, AllModules) ->
    State1 = build_dep_modules(Config0, State0, DepModules,
                               Includes, AppDir,  Target, AllModules),
    {State2, Modules} =
        build_source(Config0, State1, Module,
                     Includes, AppDir, Target),
    {sin_sig:save_sig_info({?MODULE, modules, Name}, Modules, State2),
     Modules}.

build_dep_modules(_Config0, State0, [], _Includes, _AppDir,  _Target, _AllModules) ->
    State0;
build_dep_modules(Config0, State0, DepNames, Includes, AppDir,  Target, AllModules) ->
    sets:fold(fun(DepName, State1) ->
                      case ec_lists:find(fun(#module{name=ModName})
                                               when ModName == DepName ->
                                                 true;
                                              (_) ->
                                                 false
                                         end, AllModules) of
                          {ok, Module} ->
                                {State2, _} =
                                  build_source_if_required(Config0, State1,
                                                           Module,
                                                           Includes, AppDir,
                                                           Target, AllModules),
                                State2;
                          error ->
                              State1
                      end
              end, State0, DepNames).

build_source(Config0, State0,
             Module=#module{name=Name,
                            type=Type,
                            change_sig=NewSig},
             Includes, AppDir, Target) ->
    Config1 = Config0:specialize([{module, Module}]),
    Options = Config1:match(compile_args, [])
        ++ [{outdir, Target},
            strict_record_tests,
            return_errors, return_warnings,
            {i, filename:join([AppDir, "include"])},
            %% Search directory with .hrl files
            Includes],
    event_compile_args(Config1, Options),
    BuildModule = get_build_module(State0, Type),
    {State2, Mods} = BuildModule:build_file(Config1, State0,
                                            Module, Options, Target),
    {sin_sig:save_sig_info({?MODULE, Name}, NewSig, State2), Mods}.

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

get_build_module(_, erl) ->
    sin_compile_erl;
get_build_module(_, yrl) ->
    sin_compile_yrl;
get_build_module(State, Ext) ->
    ?SIN_RAISE(State, {unsupported_file_type, Ext}).

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


