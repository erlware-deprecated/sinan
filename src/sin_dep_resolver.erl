%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  A signature definition for resolver providers
%%% @end
%%%-------------------------------------------------------------------
-module(sin_dep_resolver).

-export([new/3,
         app_dependencies/3,
         app_versions/2,
         resolve/3,
         behaviour_info/1]).

-record(dep_resolver, {impl, state}).

%%============================================================================
%% Types
%%============================================================================
-opaque impl() :: record(dep_resolver).

%%============================================================================
%% API
%%============================================================================

%% @doc create a new dep resolver with the build config
-spec new(module(), sin_config:matcher(), sin_state:state()) -> impl().
new(Impl, Config, State) ->
    #dep_resolver{impl=Impl, state=Impl:new(Config, State)}.

%% @doc return the dependency specs for a particular version of
%% an app
-spec app_dependencies(impl(), sin_dep_solver:app(),
                           sin_dep_solver:version()) ->
                                  {impl(), [sin_dep_solver:spec()]}.
app_dependencies(Resolver = #dep_resolver{impl=Impl, state=State0}, App, Ver) ->
    {State1, Deps} = Impl:app_dependencies(State0, App, Ver),
    {Resolver#dep_resolver{state=State1}, Deps}.

%% @doc get all available versions of an app
-spec app_versions(impl(), sin_dep_solver:app()) ->
                          {impl(), [sin_dep_solver:version()]}.
app_versions(Resolver = #dep_resolver{impl=Impl, state=State0}, App) ->
    {State1, Versions} = Impl:app_versions(State0, App),
    {Resolver#dep_resolver{state=State1}, Versions}.

%% @doc resolve the actual app and version and return a path and
%% updated state to the resolved app. This will only be called once per dependency.
-spec resolve(impl(), sin_dep_solver:app(), sin_dep_solver:version()) ->
                     {impl(), string()}.
resolve(Resolver = #dep_resolver{impl=Impl, state=State0}, App, Vsn) ->
    {State1, Path} = Impl:resolve(State0, App, Vsn),
    {Resolver#dep_resolver{state=State1}, Path}.

%% @doc define the behaviour for tasks.
behaviour_info(callbacks) ->
    [{new, 2},
     {app_dependencies, 3},
     {app_versions, 2}];
behaviour_info(_) ->
    undefined.
