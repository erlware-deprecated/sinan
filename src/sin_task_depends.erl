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

    Desc = "
depends Task
============

This task analyzes all of the dependencies in the project and
provides that information to the build state for use by other
tasks.

Dependencies are taken from three areas.

1. Dependencies specified in the `applications` and `included_applications`
element in each OTP app metadata file.
2. Dependency constraints specified in the projects `sinan.config`
3. Dependency constraints in a `dep_constraints` element in the OTP App metadata.

`dep_constraints` is a Sinan only addition to the metadata that OTP ignores.


Constraining Dependency Versions
--------------------------------

To constrain the versions of set of dependencies that you rely on you
must add a `dep_constraints` tuple to either the [[OTPApplication]]
dotapp file or the `sinan.config`. The `dep_constraints` tuple
contains a list of dependency constraints for the [[OTPApplication]]
in the case of the dotapp or for the entire project in the case of the
`sinan.config`. An individual dependency constraint may look as follows.

    {<app-name>, <version>, <type>}

or

    {<app-name>, <version 1>, <version 2>, <type>}

or

    {<app-name>, <version>}

In the above case type may be one of the following

* gte: greater than or equal
* lte: less than or equal
* lt: less than
* gt: greater than
* eql: equal
* between: between two versions

`between` is the specifier that takes two versions. So if we depended
on my_app between versions 0.1 and 5.0 we would have the entry:

     {my_foo, \"0.1\", \"5.0\", between}

that would provide the constraints we need.

The form `{<app-name>, <version>}` is exactly equivalent to
`{<app-name>, <version>, eql}` and is provided as a convenience.

Lets look at a complete example used in sinan itself. This is from
sinan's `sinan.config`.


    {dep_constraints,
     [{cucumberl, \"0.0.4\", gte},
      {erlware_commons, \"0.6.0\", gte},
      {getopt, \"0.0.1\", gte}]}.

This is specifying the versions for the entire sinan project.
",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          example = "depends",
          short_desc = "Dependency resolution for the project",
          deps = ?DEPS,
          desc = Desc,
          opts = []}.

%% @doc gather all the dependencies for the system
-spec do_task(sin_config:config(), sin_state:state()) ->
                     sin_state:state().
do_task(Config, State0) ->
    ProjectApps = sin_state:get_value(project_applist, State0),
    {State1, {ReleaseApps, RuntimeDeps0, CompiletimeDeps}} =
        solve_deps(Config, State0, ProjectApps),

    lists:foreach(fun(#app{path=Path}) ->
                          Ebin = filename:join(Path, "ebin"),
                          ec_file:mkdir_path(Ebin),
                          true = code:add_patha(Ebin)
                  end, CompiletimeDeps),

    MergedDeps = RuntimeDeps0 ++ CompiletimeDeps,

    FA = fun (App) ->
                 format_app(Config, App)
         end,
    sin_log:verbose(Config, "~ncompile time dependencies:~n"),
    lists:foreach(FA, CompiletimeDeps),

    sin_log:verbose(Config, "~nruntime dependencies:~n"),
    lists:foreach(FA, RuntimeDeps0),

    sin_log:verbose(Config, "~nproject applications:~n"),
    lists:foreach(FA, ReleaseApps),

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

    {Apps, ActualSpecs} = remove_excluded(ProjectApps,
                                          DefaultConstraints),

    ResolverState0 = sin_dep_resolver:new(sin_fs_resolver, Config, State0),
    SolverState0 = sin_dep_solver:new(ResolverState0),

    {SolverState1, RuntimeDeps0} =
        get_runtime_deps(Config, State0, SolverState0, Apps, ActualSpecs),
    {SolverState2, CompiletimeDeps0} =
        get_compiletime_deps(Config, State0, SolverState1, DefaultConstraints),

    ResolverState1 = sin_dep_solver:extract_resolver_state(SolverState2),
    {ReleaseApps1, RuntimeDeps2} =
        seperate_resolve_deps(ResolverState1, RuntimeDeps0, ProjectApps),

    CompiletimeDeps1 =
        lists:foldl(fun({AppName, Vsn}, Acc) ->
                            case in_runtime(AppName, RuntimeDeps2) of
                                true ->
                                    Acc;
                                false ->
                                    {_, Path} =
                                        sin_dep_resolver:resolve(ResolverState1,
                                                                 AppName, Vsn),
                                    [#app{name=AppName, vsn=Vsn, path=Path,
                                          type=compiletime, project=false} |
                                     Acc]
                            end
                    end, [], CompiletimeDeps0),


    State1 =
        sin_sig:save_sig_info(?MODULE,
                              {ReleaseApps1, RuntimeDeps2, CompiletimeDeps1},
                              State0),

    {State1, {ReleaseApps1, RuntimeDeps2, CompiletimeDeps1}}.

seperate_resolve_deps(ResolverState1, RuntimeDeps0, ProjectApps) ->
    lists:foldl(fun({AppName, Vsn}, {ReleaseApps0, RuntimeDeps1}) ->
                        {_, Path} =
                            sin_dep_resolver:resolve(ResolverState1,
                                                     AppName, Vsn),
                            case get_project_app(AppName, ProjectApps)  of
                                {ok, PApp, _}  ->
                                    {[PApp#app{path=Path,
                                               type=runtime,
                                               project=true} | ReleaseApps0],
                                     RuntimeDeps1};
                                not_found ->
                                    {ReleaseApps0,
                                     [#app{name=AppName, vsn=Vsn, path=Path,
                                           type=runtime,
                                           project=false} | RuntimeDeps1]}
                            end
                    end, {[], []}, RuntimeDeps0).

get_project_app(AppName, ProjectApps) ->
    ec_lists:search(fun(PApp=#app{name=PAppName}) ->
                           case PAppName == AppName of
                               true ->
                                   {ok,PApp};
                               _ ->
                                   not_found
                           end
                   end,
                   ProjectApps).

in_runtime(App0, RuntimeDeps) ->
    lists:any(fun(#app{name=AppName})
                    when App0 == AppName ->
                      true;
                 (_) ->
                      false
              end, RuntimeDeps).

-spec format_app(sin_config:config(), sinan:app()) -> ok.
format_app(Config, #app{name=Name0, vsn=Vsn0, path=Path}) ->
    Name1 = string:left(erlang:atom_to_list(Name0), 25),
    Vsn1 = string:left(Vsn0, 10),
    sin_log:verbose(Config, "    ~s ~s : ~s", [Name1, Vsn1, Path]).


%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
                              string().
format_exception(?SIN_EXEP_UNPARSE(_, {failed, {possible_culprit, SpecList1}})) ->
    ["Unable to resolve compile time dependencies, probably due "
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
-spec get_compiletime_deps(sin_config:config(),
                           sin_state:state(),
                           sin_dep_solver:state(),
                           [sin_dep_solver:spec()]) ->
                                  {sin_dep_solver:state(),
                                   [sin_dep_solver:spec()]}.
get_compiletime_deps(Config, State0, SolverState0, DefaultSpecs) ->
    CompiletimeApps0 = Config:match(compile_deps, []),

    {CompiletimeApps1, CompileSpecs} = remove_excluded(CompiletimeApps0,
                                                       DefaultSpecs),
    CompiletimeDeps0 =
        case sin_dep_solver:all_deps(Config, SolverState0,
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


get_runtime_deps(Config, State0, SolverState0, Apps, Specs) ->
    case sin_dep_solver:all_deps(Config, SolverState0, Apps,
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

remove_excluded(Apps0, Constraints) ->
    Apps1 = lists:filter(fun(#app{name=AppName}) ->
                                 not lists:member({exclude, AppName},
                                                  Constraints)
                         end, Apps0),
    DefaultSpecs =
        [{AppName, Vsn} || #app{name=AppName, vsn=Vsn} <- Apps1],

    {[AppName || #app{name=AppName} <- Apps1], DefaultSpecs ++ Constraints}.
