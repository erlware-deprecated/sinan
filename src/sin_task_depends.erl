%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Checks the dependencies in the system. Pulls down latest dependencies if
%%% required.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_depends).

-behaviour(sin_task).

-include_lib("sinan/include/sinan.hrl").
-include("internal.hrl").

%% API
-export([description/0,
         do_task/2,
         format_exception/1]).

-define(TASK, depends).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================

%% @doc provide a description of the system for the caller
-spec description() -> sin_task:task_description().
description() ->

    Desc = "This task analyzes all of the dependencies in the project and
        provides that" " information to the build state for use by other
        tasks. It is not a command intended to be called directly by the
        user. Though you can if that floats your boat.",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          example = "depends",
          short_desc = "dependency resolution for the project",
          deps = ?DEPS,
          desc = Desc,
          opts = []}.

%% @doc gather all the dependencies for the system
-spec do_task(sin_config:config(), sin_state:state()) ->
                     sin_state:state().
do_task(Config, State0) ->
    ProjectApps = sin_state:get_value(project_applist, State0),
    Changed1 =
        lists:foldl(fun(AppName, Changed0) ->
                            AppF = sin_state:get_value({apps, AppName, dotapp},
                                                       State0),
                        Changed0 orelse sin_sig:changed(deps, AppF, State0)
                    end, false, ProjectApps),
    {State1, {RuntimeDeps0, CompiletimeDeps}} =
        case Changed1 of
            true ->
                case sin_sig:get_sig_info(?MODULE, State0) of
                    {ok, Deps} ->
                        {State0, Deps};
                    _ ->
                        solve_deps(Config, State0, ProjectApps)
                end;
            _ ->
                solve_deps(Config, State0, ProjectApps)
        end,

    {State4, ReleaseApps, RuntimeDeps1} =
        lists:foldl(fun(App0=#app{name=AppName},
                        {State2, ReleaseApps0, RuntimeApps0}) ->
                            case lists:member(AppName, ProjectApps) of
                                true ->
                                    {State3, Mods} =
                                        process_source_files(State2, AppName,
                                                             RuntimeDeps0 ++
                                                                 CompiletimeDeps),
                                        App1 = App0#app{sources=Mods,
                                                        project=true},
                                    {State3, [App1 | ReleaseApps0], [App1 | RuntimeApps0]};
                                false ->
                                    {State2, ReleaseApps0, [App0 | RuntimeApps0]}
                            end
                    end, {State1, [], []}, RuntimeDeps0),

    MergedDeps = RuntimeDeps0 ++ lists:filter(fun(#app{name=App, vsn=Vsn}) ->
                                                      not lists:member({App, Vsn}, RuntimeDeps0)
                                              end, CompiletimeDeps),

    sin_state:store(release_runtime_deps, RuntimeDeps1,
                    sin_state:store(release_compile_deps,
                                    CompiletimeDeps,
                                    sin_state:store(release_deps, MergedDeps,
                                                    sin_state:store(release_apps,
                                                                    ReleaseApps, State4)))).

-spec solve_deps(sin_config:config(), sin_state:state(), [atom()]) ->
                        {sin_state:state(), term()}.
solve_deps(Config, State0, ProjectApps) ->
    DefaultConstraints = Config:match(dep_constraints, []),

    {Apps, ActualSpecs} = remove_excluded(State0,
                                          ProjectApps,
                                          DefaultConstraints),

    ResolverState0 = sin_dep_resolver:new(sin_fs_resolver, Config, State0),
    SolverState0 = sin_dep_solver:new(ResolverState0),

    {SolverState1, RuntimeDeps0} =
        get_runtime_deps(State0, SolverState0, Apps, ActualSpecs),
    {SolverState2, CompiletimeDeps0} =
        get_compiletime_deps(State0, Config, SolverState1, DefaultConstraints),

    ResolverState1 = sin_dep_solver:extract_resolver_state(SolverState2),
    RuntimeDeps1 =
        lists:map(fun({App, Vsn}) ->
                          {_, Path} =
                              sin_dep_resolver:resolve(ResolverState1, App, Vsn),

                          #app{name=App, vsn=Vsn, path=Path,
                               type=runtime, project=lists:member(App, ProjectApps)}
                  end, RuntimeDeps0),

    CompiletimeDeps1 =
        lists:map(fun({App, Vsn}) ->
                          {_, Path} =
                              sin_dep_resolver:resolve(ResolverState1, App, Vsn),
                          #app{name=App, vsn=Vsn, path=Path,
                               type=compiletime, project=false}

                  end, CompiletimeDeps0),

    {State3, RuntimeDeps2} =
        lists:foldl(fun(App=#app{project=true, path=Path}, {State1, Acc}) ->
                            {State2, Modules} =
                                process_source_files(State1, Path, RuntimeDeps1),
                            {State2, [App#app{modules=Modules} | Acc]};
                       (App, {State1, Acc}) ->
                            {State1, [App | Acc]}
                    end, {State0, []}, RuntimeDeps1),


    State4 =
        sin_sig:save_sig_info(?MODULE,
                              {RuntimeDeps2, CompiletimeDeps1}, State3),

    {State4, {RuntimeDeps1, CompiletimeDeps1}}.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================
-spec process_source_files(sin_state:state(), atom(), [sinan:app()]) ->
                                  {sin_state:state(), [sinan:mod()]}.
process_source_files(State0, AppName, Deps) ->
    Includes = lists:map(fun(#app{path=Path}) ->
                                 filename:join(Path, "includes")
                         end, Deps),
    AppDir = sin_state:get_value({apps, AppName, basedir}, State0),
    SrcDir = filename:join(AppDir, "src"),
    TestDir = filename:join(AppDir, "test"),
    {State1, SrcModules} = process_source_files_in_path(State0, SrcDir, Includes),
    {State2, TestModules} = process_source_files_in_path(State1, TestDir, Includes),
    {State2, SrcModules ++ TestModules}.

-spec process_source_files_in_path(sin_state:state(), string(), [string()]) ->
                                          {sin_state:state(), sinan:mod()}.
process_source_files_in_path(State0, Dir, Includes) ->
    filelib:fold_files(Dir, "^((.+\.erl)|(.+\.hrl)|(.+\.erl))$", true,
                       fun(Path, {State1, Acc}) ->
                               {State2, Rec} =
                                   sin_file_info:process_file(State1, Path, Includes),
                               {State2, [Rec | Acc]}
                       end, {State0, []}).


-spec get_compiletime_deps(sin_state:state(), sin_config:config(),
                           sin_dep_solver:state(),
                           [sin_dep_solver:spec()]) ->
                                  {sin_dep_solver:state(), [sin_dep_solver:spec()]}.
get_compiletime_deps(State0, Config, SolverState0, DefaultSpecs) ->
    CompiletimeApps0 = [eunit, proper] ++
        Config:match(compile_deps, []),

    {CompiletimeApps1, CompileSpecs} = remove_excluded(State0,
                                                       CompiletimeApps0,
                                                       DefaultSpecs),
    CompiletimeDeps0 =
        case sin_dep_solver:all_deps(SolverState0,
                                     CompiletimeApps1,
                                     CompileSpecs) of
            Error1 = {failed, {possible_culprit, SpecList1}} ->
            ewl_talk:say("Unable to resolve compile time dependencies, probably do "
                         "to the following constraints:"),
            lists:foreach(fun({AppName, LimitSet, Sources}) ->
                                  ewl_talk:say(" constraint on ~p with constraints ~p "
                                               "originating from these application(s) ~p",
                                               [AppName, sets:to_list(LimitSet),
                                                sets:to_list(Sources)]);
                             (AppName) when is_atom(AppName) ->
                                  ewl_talk:say(" application ~p in the project ",
                                               [AppName])

                          end, SpecList1),
            ?SIN_RAISE(State0, Error1);
        {_, Deps1} ->
            lists:filter(fun({excluded, _}) ->
                                 false;
                            (_) ->
                                 true
                         end, Deps1)
        end,
    {SolverState0, CompiletimeDeps0}.


get_runtime_deps(State0, SolverState0, Apps, Specs) ->
    case sin_dep_solver:all_deps(SolverState0, Apps,
                                 Specs) of
        Error1 = {failed, {possible_culprit, SpecList1}} ->
            ewl_talk:say("Unable to resolve runtime dependencies, probably do "
                         "to the following constraints:"),
            lists:foreach(fun({AppName, LimitSet, Sources}) ->
                                  ewl_talk:say(" constraint on ~p with constraints ~p "
                                               "originating from these application(s) ~p",
                                               [AppName, sets:to_list(LimitSet),
                                                sets:to_list(Sources)]);
                             (AppName) when is_atom(AppName) ->
                                  ewl_talk:say(" application ~p in the project ",
                                               [AppName])
                          end, SpecList1),
            ?SIN_RAISE(State0, Error1);
        {SolverState1, Deps0} ->
            {SolverState1,
             lists:filter(fun({excluded, _}) ->
                                  false;
                             (_) ->
                                  true
                          end, Deps0)}
    end.

-spec remove_excluded(sin_state:state(),
                      [sin_dep_solver:app()],
                      [sin_dep_solver:spec()]) ->
                             {[sin_dep_solver:app()], [sin_dep_solver:spec()]}.
remove_excluded(State0, Apps0, Constraints) ->
    Apps1 = lists:filter(fun(App) ->
                                 not lists:member({exclude, App}, Constraints)
                         end, Apps0),
    DefaultSpecs =
        lists:map(fun(AppName) ->
                         case sin_state:get_value({apps, AppName, vsn},
                                                  State0) of
                             undefined ->
                                 AppName;
                             Vsn ->
                                 {AppName, Vsn}
                         end
                  end, Apps1),
    {Apps1, DefaultSpecs ++ Constraints}.

