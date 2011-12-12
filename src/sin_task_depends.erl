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
    {State1, {ReleaseApps, RuntimeDeps0, CompiletimeDeps}} =
        case Changed1 of
            true ->
                case sin_sig:get_sig_info(?MODULE, State0) of
                    {ok, {RelApps, RunDeps, CompDeps}} ->
                        {State0, {RelApps, RunDeps, CompDeps}};
                    _ ->
                        solve_deps(Config, State0, ProjectApps)
                end;
            _ ->
                solve_deps(Config, State0, ProjectApps)
        end,

    lists:foreach(fun(#app{path=Path}) ->
                          Ebin = filename:join(Path, "ebin"),
                          ec_file:mkdir_path(Ebin),
                          true = code:add_patha(Ebin)
                  end, CompiletimeDeps),


    MergedDeps = RuntimeDeps0 ++ CompiletimeDeps,

    ec_talk:say("~ncompile time dependencies:~n"),
    lists:foreach(fun format_app/1, CompiletimeDeps),

    ec_talk:say("~nruntime dependencies:~n"),
    lists:foreach(fun format_app/1, RuntimeDeps0),

    ec_talk:say("~nproject applications:~n"),
    lists:foreach(fun format_app/1, ReleaseApps),


    sin_state:store([{release_runtime_deps, RuntimeDeps0},
                     {release_compile_deps, CompiletimeDeps},
                     {release_deps, MergedDeps},
                     {project_apps, ReleaseApps},
                     {release_apps, MergedDeps ++ ReleaseApps}],
                    State1).

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
    {ReleaseApps1, RuntimeDeps2} =
        lists:foldl(fun({App, Vsn}, {ReleaseApps0, RuntimeDeps1}) ->
                            {_, Path} =
                                sin_dep_resolver:resolve(ResolverState1,
                                                         App, Vsn),
                            case lists:member(App, ProjectApps) of
                                true ->
                                    {[#app{name=App, vsn=Vsn, path=Path,
                                           type=runtime,
                                           project=true} | ReleaseApps0],
                                     RuntimeDeps1};
                                false ->
                                    {ReleaseApps0,
                                     [#app{name=App, vsn=Vsn, path=Path,
                                           type=runtime,
                                           project=false} | RuntimeDeps1]}
                            end
                    end, {[], []}, RuntimeDeps0),

    CompiletimeDeps1 =
        lists:foldl(fun({App, Vsn}, Acc) ->
                            case in_runtime(App, RuntimeDeps2) of
                                true ->
                                    Acc;
                                false ->
                                    {_, Path} =
                                        sin_dep_resolver:resolve(ResolverState1,
                                                                 App, Vsn),
                                    [#app{name=App, vsn=Vsn, path=Path,
                                          type=compiletime, project=false} |
                                     Acc]
                            end
                    end, [], CompiletimeDeps0),


    State1 =
        sin_sig:save_sig_info(?MODULE,
                              {ReleaseApps1, RuntimeDeps2, CompiletimeDeps1},
                              State0),

    {State1, {ReleaseApps1, RuntimeDeps2, CompiletimeDeps1}}.

in_runtime(App0, RuntimeDeps) ->
    lists:any(fun(#app{name=AppName})
                    when App0 == AppName ->
                      true;
                 (_) ->
                      false
              end, RuntimeDeps).

-spec format_app(sinan:app()) -> ok.
format_app(#app{name=Name0, vsn=Vsn0, path=Path}) ->
    Name1 = string:left(erlang:atom_to_list(Name0), 25),
    Vsn1 = string:left(Vsn0, 10),
    ec_talk:say("    ~s ~s : ~s", [Name1, Vsn1, Path]).


%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
                              string().
format_exception(?SIN_EXEP_UNPARSE(_, {failed, {possible_culprit, SpecList1}})) ->
    ["Unable to resolve compile time dependencies, probably do "
     "to the following constraints:~n",
     lists:map(fun({AppName, LimitSet, Sources}) ->
                       io_lib:format(" constraint on ~p with constraints ~p "
                                     "originating from these application(s) ~p",
                                     [AppName, sets:to_list(LimitSet),
                                      sets:to_list(Sources)]);
                  (AppName) when is_atom(AppName) ->
                       [" application", erlang:atom_to_list(AppName), " in the project "]
               end, SpecList1)];
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================
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
            Error1 = {failed, {possible_culprit, _SpecList1}} ->
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
        Error1 = {failed, {possible_culprit, _SpecList1}} ->
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
