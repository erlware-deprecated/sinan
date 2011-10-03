-module(sin_dep_solver).

-export([new/1, deps/3, all_deps/3, extract_resolver_state/1]).

-export_type([spec/0, app_path/0,
              state/0, app/0, version/0]).

-include_lib("sinan/include/sinan.hrl").

-record(state, {r :: sin_dep_resolver:impl(),
                deps :: set(),
                dc :: dict(),
                vc :: dict()}).

%%============================================================================
%% Types
%%============================================================================

-opaque state() :: record(state).
-type app() :: atom().
-type directive() :: gte | lte | lt | gt | eql | between.
-type version() :: string().
-type spec() ::  spec_part()
               | {excluded, app()}.
-type spec_part() :: app()
                   | {app(), version()}
                   | {app(), version(), directive()}
                   | {app(), version(), version(), directive()}.
-type app_path() :: [{app(), version()}].
-type mixed_spec() :: {app, app()} | spec().
-type internal_spec() :: {app, [spec()]}.

%%============================================================================
%% API
%%============================================================================
-spec new(sin_dep_resolver:impl()) -> state().
new(R0) ->
    #state{r=R0, dc=dict:new(), vc=dict:new(), deps=sets:new()}.

-spec deps(state(), app(), version()) ->
                  {state(), [{app(), version()}]}.
deps(State0, App, Ver) ->
    Limits = add_limit(State0, '__top_level__', new_limits(), {App, Ver}),
    deps(State0#state{deps=sets:add_element(App, sets:new())},
         [], App, Limits, []).

-spec all_deps(state(), [app()], [{app(), spec()}]) ->
                      {state(), [{app(), version()}]}.
all_deps(State0, Apps, Limits0) ->
    Limits2 = lists:foldl(fun(Dep, Limits1) ->
                                  add_limit(State0, '__top_level__', Limits1, Dep)
                          end, new_limits(), Limits0),
    D2 = lists:foldl(fun(App, D1) ->
                             sets:add_element(App, D1)
                     end, sets:new(), Apps),
    case all_deps(State0#state{deps=D2}, [], Apps, Limits2, []) of
        {_, fail} ->
            shrink_deps(State0#state{deps=D2}, Apps, Limits2);
        Res ->
            Res
    end.

-spec extract_resolver_state(state()) -> sin_dep_resolver:impl().
extract_resolver_state(#state{r=R0}) ->
    R0.

%%============================================================================
%% Internal Functions
%% ============================================================================

%% @doc tries to shrink the limits provided by the user. Provides a
%% powerset of those limits. Sorts them to broadest first, then runs
%% the resolver on each set of limits until it finds one that
%% passes. Then the possible culprits are the inverse of those limits.
-spec shrink_deps(state(), [app()], [spec()]) ->
                         {failed, {possible_culprit, [ app() | spec()]}}.
shrink_deps(State0, Apps0, Limits0) ->
    AdjustedLimits = lists:map(fun(App) -> {app, App} end, Apps0) ++ Limits0,
    BroadestFirst = broadest_first(remove_empty(powerset(AdjustedLimits))),
    FirstPassingSet =
        case ec_lists:find(fun([]) ->
                                   false;
                              (LimitSet) ->
                                   {Apps1, Limits1} = disambiguate_set(LimitSet),
                                   case all_deps(State0, [], Apps1, Limits1, []) of
                                       {_, fail} ->
                                           false;
                                       _ ->
                                           true
                                   end
                           end, BroadestFirst) of
            {ok, List} ->
                List;
            error ->
                []
        end,
    {failed, {possible_culprit, invert(FirstPassingSet, AdjustedLimits)}}.

-spec remove_empty([mixed_spec()]) -> [mixed_spec()].
remove_empty(Limits) ->
    %% Every limit needs at least one {app, _} considering that this a
    %% powers set we need to roll through the possibilities and remove
    %% the ones that only contain limits without apps
    lists:filter(fun(LimitSet) ->
                         lists:any(fun({app, _}) ->
                                           true;
                                      (_) ->
                                           false
                                   end, LimitSet)
                 end, Limits).

-spec invert([mixed_spec()], [mixed_spec()]) ->
                    [app() | spec()].
invert(FirstPassingSet, PossibleLimits) ->
    lists:map(fun({app, App}) ->
                      App;
                 (El) ->
                      El
              end,
              lists:filter(fun(Element) ->
                                   not lists:member(Element, FirstPassingSet)
                           end, PossibleLimits)).

-spec disambiguate_set([mixed_spec]) -> {[app()], [spec()]}.
disambiguate_set(LimitSet) ->
    lists:foldl(fun({app, App}, {Apps, Limits}) ->
                        {[App | Apps], Limits};
                   (Limit, {Apps, Limits}) ->
                        {Apps, [Limit | Limits]}
                end, {[], []}, LimitSet).


-spec broadest_first([mixed_spec()]) ->
                            [mixed_spec()].
broadest_first(Limits) ->
    lists:sort(fun(L1, L2) ->
                       erlang:length(L1) > erlang:length(L2)
               end, Limits).

-spec all_deps(state(), app_path(), [app()], [internal_spec()],
               [{app_path(), [app()]}]) ->
                      {state(), [{app(), version()}]}.
all_deps(State0, _, [], Limits, []) ->
        lists:foldl(fun({App0, Lims, _Sources}, {State1=#state{deps=D0}, AppVsns}) ->
                            {State2, AppVsn} =
                                lists_some(fun (State3, App1)
                                                 when is_atom(App1) ->
                                                   {State3, fail};
                                               (State3, {_App1, Ver} = PV)
                                                 when is_list(Ver) ->
                                                   {State3, PV};
                                               (State3, {exclude, App1})
                                                 when is_atom(App1) ->
                                                   {State3, {excluded, App1}};
                                               (State3, {_App2, _Ver, _}) ->
                                                   {State3, fail};
                                               (State3, {_App2, _Ver1, _Ver2, _}) ->
                                                   {State3, fail}
                                       end, {State1, sets:to_list(Lims)}),
                            case AppVsn of
                                fail ->
                                    case sets:is_element(App0, D0) of
                                        true ->
                                           {State2, fail};
                                        false ->
                                            {State2, AppVsns}
                                    end;
                                _ ->
                                    {State2, [AppVsn | AppVsns]}
                            end
                end, {State0, []}, Limits);
all_deps(State0, _OldAppPath, [], Limits, [{AppPath, Apps} | OtherApps]) ->
    all_deps(State0, AppPath, Apps, Limits, OtherApps);
all_deps(State0, AppPath, [App | Apps], Limits, OtherApps) ->
    deps(State0, AppPath, App, Limits, [{AppPath, Apps} | OtherApps]).

-spec deps(state(), [app()], app(), [spec()], [app()]) ->
                  {state(), [{app(), version()}]}.
deps(State0, AppPath, App, Limits, OtherApps) ->
    F = fun (State1=#state{deps=D0}, Ver) ->
                check_circular(App, Ver, AppPath),
                {State2, Deps} = get_deps(State1, App, Ver, Limits),

                ULimits = extend_limits(State0, App, Limits, [{App, Ver} | Deps]),
                DepApps = lists:map(fun dep_app/1, Deps),
                D2 = lists:foldl(fun(Dep, D1) ->
                                         sets:add_element(Dep, D1)
                                 end, D0, DepApps),
                all_deps(State2#state{deps=D2},
                         [{App, Ver} | AppPath], DepApps, ULimits, OtherApps)
        end,
    lists_some(F, limited_app_versions(State0, App, Limits), fail).

-spec get_deps(state(), app(), version(), [spec()]) -> {state(), [spec()]}.
get_deps(State0 = #state{r=R0, dc=D0}, App, Ver, AppsLimits) ->
    case dict:find({App, Ver}, D0) of
        {ok, Deps} ->
            {State0, Deps};
        error ->
            {R2, Deps} =
                sin_dep_resolver:app_dependencies(R0, App, Ver),
            FilteredDeps = remove_excluded(Deps, AppsLimits),
            {State0#state{r=R2, dc=dict:store({App, Ver}, FilteredDeps, D0)},
             FilteredDeps}
    end.
-spec remove_excluded([spec()], [spec()]) -> [spec()].
remove_excluded(Deps, AppsLimits) ->
    lists:filter(fun (Dep) ->
                         DepApp = dep_app(Dep),
                         not lists:any(fun(Limit) ->
                                               is_excluded(DepApp, Limit)
                                       end, get_limits(AppsLimits, DepApp))
                 end, Deps).

%% @doc given a spec() return the app name
-spec dep_app(spec()) -> app().
dep_app({exclude, App}) when is_atom(App) -> App;
dep_app({App, _Ver}) -> App;
dep_app({App, _Ver, _}) -> App;
dep_app({App, _Ver1, _Ver2, _}) -> App;
dep_app(App) when is_atom(App) -> App.

%% @doc limits is an associated list keeping track of all the limits that have
%% been placed on a app
-spec new_limits() -> [spec()].
new_limits() -> [].

-spec add_limit(sin_state:state(), app(), [spec()], spec()) -> [spec()].
add_limit(State, Source, AppsLimits, NewLimit) ->
    case is_valid_limit(NewLimit) of
        false ->
            ewl_talk:say("Invalid constraint ~p found originating at ~p",
                         [NewLimit, Source]),
            ?SIN_RAISE(State, {invalid_constraint, NewLimit, Source});
        true ->
            ok
    end,
    App = dep_app(NewLimit),
    {Limits, Sources1} = case lists:keysearch(App, 1, AppsLimits) of
                 false -> {sets:new(), sets:new()};
                 {value, {App, Ls, Sources0}} -> {Ls, Sources0}
             end,
    [{App, sets:add_element(NewLimit, Limits),
      sets:add_element(Source, Sources1)} |
     lists:keydelete(App, 1, AppsLimits)].

-spec get_limits([internal_spec()], app()) -> [spec()].
get_limits(AppsLimits, App) ->
    case lists:keysearch(App, 1, AppsLimits) of
        false -> [];
        {value, {App, Limits, _sources}} -> sets:to_list(Limits)
    end.

-spec extend_limits(sin_state:state(), app(), [internal_spec()], [spec()]) -> [internal_spec()].
extend_limits(State, Source, AppsLimits, Deps) ->
    lists:foldl(fun (Dep, PLs) -> add_limit(State, Source, PLs, Dep) end,
                AppsLimits, Deps).

-spec limited_app_versions(state(), app(), [internal_spec()]) ->
                                  {state(), [version()]}.
limited_app_versions(State0, App, AppsLimits) ->
    {State1, RawVersions} = get_versions(State0, App),
    Versions = lists:reverse(lists:sort(RawVersions)),
    {State1,
     lists:filter(fun (Ver) ->
                          lists:all(fun (L) ->
                                            is_version_within_limit(Ver, L)
                                    end,
                                    get_limits(AppsLimits, App))
                      end, Versions)}.
-spec get_versions(state(), app()) -> {state(), [version()]}.
get_versions(State0 = #state{r=R0, vc=V0}, App) ->
    case dict:find(App, V0) of
        {ok, Versions} ->
            {State0, Versions};
        error ->
            {R1, Versions} = sin_dep_resolver:app_versions(R0, App),
            {State0#state{r=R1, vc=dict:store(App, Versions, V0)}, Versions}
    end.

-spec is_excluded(atom(), spec()) -> boolean().
is_excluded(App, {exclude, App}) -> true;
is_excluded(_App, _) -> false.

-spec is_valid_limit(spec()) -> boolean().
is_valid_limit(App) when is_atom(App) -> true;
is_valid_limit({exclude, App}) when is_atom(App) -> true;
is_valid_limit({_App, Ver}) when is_list(Ver) -> true;
is_valid_limit({_App, _LVer, gte}) -> true;
is_valid_limit({_App, _LVer, lte}) -> true;
is_valid_limit({_App, _LVer, gt}) -> true;
is_valid_limit({_App, _LVer, lt}) -> true;
is_valid_limit({_App, _LVer1, _LVer2, between}) -> true;
is_valid_limit(_InvalidLimit) -> false.

-spec is_version_within_limit(version(), spec()) -> boolean().
is_version_within_limit(_Ver, App) when is_atom(App) -> true;
is_version_within_limit(_Ver, {exclude, App}) when is_atom(App) -> false;
is_version_within_limit(Ver, {_App, Ver}) when is_list(Ver) -> true;
is_version_within_limit(Ver, {_App, LVer, gte}) when Ver >= LVer -> true;
is_version_within_limit(Ver, {_App, LVer, lte}) when Ver =< LVer -> true;
is_version_within_limit(Ver, {_App, LVer, gt}) when Ver > LVer -> true;
is_version_within_limit(Ver, {_App, LVer, lt}) when Ver < LVer -> true;
is_version_within_limit(Ver, {_App, LVer1, LVer2, between})
  when Ver >= LVer1 andalso Ver =< LVer2 -> true;
is_version_within_limit(_Ver, _App) -> false.

%% @doc return the first Res = F(el) for El in List that is not False
-spec lists_some(fun(), {state(), [spec()]}) ->
                        {state(), [{app(), version()}] | fail}.
lists_some(F, L) ->
    lists_some(F, L, fail).

-spec lists_some(fun(), {state(), [spec()]}, boolean()) ->
                        {state(), [{app(), version()}]}.
lists_some(_, {State0, []}, False) ->
    {State0, False};
lists_some(F, {State0, [H | T]}, False) ->
    case F(State0, H) of
        {State1, False} ->
            lists_some(F, {State1, T}, False);
        Res ->
            Res
    end.

-spec check_circular(app(), version(), app_path()) -> ok.
check_circular(App, Ver, AppPath) ->
    case lists:member({App, Ver}, AppPath) of
        true ->
            throw({circular_dependency,
                   {dependency_path,
                    lists:reverse([{App, Ver} | AppPath])}});
        false ->
            ok
    end.

-spec powerset(list()) -> list().
powerset(Lst) ->
    N = length(Lst),
    Max = trunc(math:pow(2,N)),
    [[lists:nth(Pos+1,Lst) || Pos <- lists:seq(0,N-1), I band (1 bsl Pos) =/= 0]
      || I <- lists:seq(0,Max-1)].

%%============================================================================
%% Tests
%%============================================================================
-ifndef(NO_TESTS).
-include_lib("eunit/include/eunit.hrl").

passing_test() ->

    App1Deps = [{app2, "0.1.0", gte},
                {app3, "0.1.1", "0.1.5", between}],

    App2Deps = [{app4, "5.0.0", gte}],
    App3Deps = [{app5, "2.0.0", gte}],
    App4Deps = [app5],
    Config = sin_config:new_from_terms([{{dependencies, app1, "0.1.0"}, App1Deps},
                                        {{dependencies, app1, "0.2"}, App1Deps},
                                        {{dependencies, app1, "3.0"}, App1Deps},
                                        {{dependencies, app2, "0.0.0.1"}, App2Deps},
                                        {{dependencies, app2, "0.1"}, App2Deps},
                                        {{dependencies, app2, "1.0"}, App2Deps},
                                        {{dependencies, app2, "3.0"}, App2Deps},
                                        {{dependencies, app3, "0.1.0"}, App3Deps},
                                        {{dependencies, app3, "0.1.3"}, App3Deps},
                                        {{dependencies, app3, "2.0.0"}, App3Deps},
                                        {{dependencies, app3, "3.0.0"}, App3Deps},
                                        {{dependencies, app3, "4.0.0"}, App3Deps},
                                        {{dependencies, app4, "0.1.0"}, App4Deps},
                                        {{dependencies, app4, "0.3.0"}, App4Deps},
                                        {{dependencies, app4, "5.0.0"}, App4Deps},
                                        {{dependencies, app4, "6.0.0"}, App4Deps},
                                        {{dependencies, app5, "0.1.0"}, []},
                                        {{dependencies, app5, "0.3.0"}, []},
                                        {{dependencies, app5, "2.0.0"}, []},
                                        {{dependencies, app5, "6.0.0"}, []},
                                        {{versions, app1}, ["0.1.1",
                                                            "0.2",
                                                            "3.0"]},
                                        {{versions, app2}, ["0.0.1",
                                                            "0.1",
                                                            "1.0",
                                                            "3.0"]},
                                        {{versions, app3}, ["0.1.0",
                                                            "0.1.3",
                                                            "2.0.0",
                                                            "3.0.0",
                                                            "4.0.0"]},
                                        {{versions, app4}, ["0.1.0",
                                                            "0.3.0",
                                                            "5.0.0",
                                                            "6.0.0"]},
                                        {{versions, app5}, ["0.1.0",
                                                            "0.3.0",
                                                            "2.0.0",
                                                            "6.0.0"]}], []),

    Res = sin_dep_resolver:new(sin_test_resolver,
                               sin_config:create_matcher([], Config),
                               sin_state:new()),
    State = new(Res),
    ?assertMatch({_, [{app1,"3.0"},
                      {app2,"3.0"},
                      {app4,"6.0.0"},
                      {app3,"0.1.3"},
                      {app5,"6.0.0"}]}, deps(State, app1, "3.0")),

    ?assertMatch({_, [{app1,"3.0"},
                      {app3,"0.1.3"},
                      {app2,"3.0"},
                      {app4,"6.0.0"},
                      {app5,"6.0.0"}]}, all_deps(State, [app1, app2, app5], [])).


conflicting_passing_test() ->
    App1Deps = [{app2, "0.1.0", gte},
                {app5, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between},
                {app3, "0.1.1", "0.1.5", between}],

    App2Deps = [{app4, "3.0.0", gte}],
    App3Deps = [{app5, "2.0.0", gte}],

    Config = sin_config:new_from_terms([{{dependencies, app1, "0.1.0"}, App1Deps},
                                        {{dependencies, app1, "0.1.0"}, App1Deps},
                                        {{dependencies, app1, "0.2"}, App1Deps},
                                        {{dependencies, app1, "3.0"}, App1Deps},
                                        {{dependencies, app2, "0.0.0.1"}, App2Deps},
                                        {{dependencies, app2, "0.1"}, App2Deps},
                                        {{dependencies, app2, "1.0"}, App2Deps},
                                        {{dependencies, app2, "3.0"}, App2Deps},
                                        {{dependencies, app3, "0.1.0"}, App3Deps},
                                        {{dependencies, app3, "0.1.3"}, App3Deps},
                                        {{dependencies, app3, "2.0.0"}, App3Deps},
                                        {{dependencies, app3, "3.0.0"}, App3Deps},
                                        {{dependencies, app3, "4.0.0"}, App3Deps},
                                        {{dependencies, app4, "0.1.0"}, [{app5, "0.1.0"}]},
                                        {{dependencies, app4, "0.3.0"}, [{app5, "0.3.0"}]},
                                        {{dependencies, app4, "5.0.0"}, [{app5, "2.0.0"}]},
                                        {{dependencies, app4, "6.0.0"}, [{app5, "6.0.0"}]},
                                        {{dependencies, app5, "0.1.0"}, []},
                                        {{dependencies, app5, "0.3.0"}, []},
                                        {{dependencies, app5, "2.0.0"}, []},
                                        {{dependencies, app5, "6.0.0"}, []},
                                        {{versions, app1}, ["0.1.1",
                                                            "0.2",
                                                            "3.0"]},
                                        {{versions, app2}, ["0.0.1",
                                                            "0.1",
                                                            "1.0",
                                                            "3.0"]},
                                        {{versions, app3}, ["0.1.0",
                                                            "0.1.3",
                                                            "2.0.0",
                                                            "3.0.0",
                                                            "4.0.0"]},
                                        {{versions, app4}, ["0.1.0",
                                                            "0.3.0",
                                                            "5.0.0",
                                                            "6.0.0"]},
                                        {{versions, app5}, ["0.1.0",
                                                            "0.3.0",
                                                            "2.0.0",
                                                            "6.0.0"]}], []),

    Res = sin_dep_resolver:new(sin_test_resolver,
                               sin_config:create_matcher([], Config),
                               sin_state:new()),
    State = new(Res),
    ?assertMatch({_, [{app1,"3.0"},
                      {app2,"3.0"},
                      {app4,"5.0.0"},
                      {app3,"0.1.3"},
                      {app5,"2.0.0"}]}, deps(State, app1, "3.0")),

    ?assertMatch({_, [{app1,"3.0"},
                      {app3,"0.1.3"},
                      {app2,"3.0"},
                      {app4,"5.0.0"},
                      {app5,"2.0.0"}]}, all_deps(State, [app1, app2, app5], [])).



circular_dependencies_test() ->
    Config = sin_config:new_from_terms([{{dependencies, app1, "0.1.0"},
                                         [app2]},
                                        {{dependencies, app2, "0.0.1"},
                                         [app1]},
                                        {{versions, app1}, ["0.1.0"]},
                                        {{versions, app2}, ["0.0.1"]}], []),


    Res = sin_dep_resolver:new(sin_test_resolver,
                               sin_config:create_matcher([], Config),
                               sin_state:new()),
    State = new(Res),
    ?assertThrow({circular_dependency,
                  {dependency_path, [{app1,"0.1.0"},
                                     {app2,"0.0.1"},
                                     {app1,"0.1.0"}]}},
                  deps(State, app1, "0.1.0")).

conflicting_failing_test() ->
    App1Deps = [app2,
                {app5, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between}],

    App2Deps = [{app4, "5.0.0", gte}],
    App3Deps = [{app5, "6.0.0"}],

    Config = sin_config:new_from_terms([{{dependencies, app1, "0.1.1"}, App1Deps},
                                        {{dependencies, app1, "0.2"}, App1Deps},
                                        {{dependencies, app1, "3.0"}, App1Deps},
                                        {{dependencies, app2, "0.0.1"}, App2Deps},
                                        {{dependencies, app2, "0.1"}, App2Deps},
                                        {{dependencies, app2, "1.0"}, App2Deps},
                                        {{dependencies, app2, "3.0"}, App2Deps},
                                        {{dependencies, app3, "0.1.0"}, App3Deps},
                                        {{dependencies, app3, "0.1.3"}, App3Deps},
                                        {{dependencies, app3, "2.0.0"}, App3Deps},
                                        {{dependencies, app3, "3.0.0"}, App3Deps},
                                        {{dependencies, app3, "4.0.0"}, App3Deps},
                                        {{dependencies, app4, "0.1.0"}, [{app5, "0.1.0"}]},
                                        {{dependencies, app4, "0.3.0"}, [{app5, "0.3.0"}]},
                                        {{dependencies, app4, "5.0.0"}, [{app5, "2.0.0"}]},
                                        {{dependencies, app4, "6.0.0"}, [{app5, "6.0.0"}]},
                                        {{dependencies, app5, "0.1.0"}, []},
                                        {{dependencies, app5, "0.3.0"}, []},
                                        {{dependencies, app5, "2.0.0"}, []},
                                        {{dependencies, app5, "6.0.0"}, []},
                                        {{versions, app1}, ["0.1.1",
                                                            "0.2",
                                                            "3.0"]},
                                        {{versions, app2}, ["0.0.1",
                                                            "0.1",
                                                            "1.0",
                                                            "3.0"]},
                                        {{versions, app3}, ["0.1.0",
                                                            "0.1.3",
                                                            "2.0.0",
                                                            "3.0.0",
                                                            "4.0.0"]},
                                        {{versions, app4}, ["0.1.0",
                                                            "0.3.0",
                                                            "5.0.0",
                                                            "6.0.0"]},
                                        {{versions, app5}, ["0.1.0",
                                                            "0.3.0",
                                                            "2.0.0",
                                                            "6.0.0"]}], []),

    Res = sin_dep_resolver:new(sin_test_resolver,
                               sin_config:create_matcher([], Config), sin_state:new()),
    State = new(Res),

    ?assertMatch({failed,{possible_culprit,[app1]}},
                 all_deps(State, [app1, app3], [])).


conflicting_exclude_test() ->
    App1Deps = [app2,
                {app3, "2.0.0"},
                {app4, "0.3.0", "5.0.0", between}],

    App2Deps = [{app4, "5.0.0", gte}],
    App3Deps = [{app5, "6.0.0"}],

    Config = sin_config:new_from_terms([{{dependencies, app1, "0.1.1"}, App1Deps},
                                        {{dependencies, app1, "0.2"}, App1Deps},
                                        {{dependencies, app1, "3.0"}, App1Deps},
                                        {{dependencies, app2, "0.0.1"}, App2Deps},
                                        {{dependencies, app2, "0.1"}, App2Deps},
                                        {{dependencies, app2, "1.0"}, App2Deps},
                                        {{dependencies, app2, "3.0"}, App2Deps},
                                        {{dependencies, app3, "0.1.0"}, App3Deps},
                                        {{dependencies, app3, "0.1.3"}, App3Deps},
                                        {{dependencies, app3, "2.0.0"}, App3Deps},
                                        {{dependencies, app3, "3.0.0"}, App3Deps},
                                        {{dependencies, app3, "4.0.0"}, App3Deps},
                                        {{dependencies, app4, "0.1.0"}, [{app5, "0.1.0"}]},
                                        {{dependencies, app4, "0.3.0"}, [{app5, "0.3.0"}]},
                                        {{dependencies, app4, "5.0.0"}, [{app5, "2.0.0"}]},
                                        {{dependencies, app4, "6.0.0"}, [{app5, "6.0.0"}]},
                                        {{dependencies, app5, "0.1.0"}, []},
                                        {{dependencies, app5, "0.3.0"}, []},
                                        {{dependencies, app5, "2.0.0"}, []},
                                        {{dependencies, app5, "6.0.0"}, []},
                                        {{versions, app1}, ["0.1.1",
                                                            "0.2",
                                                            "3.0"]},
                                        {{versions, app2}, ["0.0.1",
                                                            "0.1",
                                                            "1.0",
                                                            "3.0"]},
                                        {{versions, app3}, ["0.1.0",
                                                            "0.1.3",
                                                            "2.0.0",
                                                            "3.0.0",
                                                            "4.0.0"]},
                                        {{versions, app4}, ["0.1.0",
                                                            "0.3.0",
                                                            "5.0.0",
                                                            "6.0.0"]},
                                        {{versions, app5}, ["0.1.0",
                                                            "0.3.0",
                                                            "2.0.0",
                                                            "6.0.0"]}], []),

    Res = sin_dep_resolver:new(sin_test_resolver,
                               sin_config:create_matcher([], Config), sin_state:new()),
    State = new(Res),

    ?assertMatch({_, [{excluded,app3},
                      {app1,"3.0"},
                      {app2,"3.0"},
                      {app4,"5.0.0"},
                      {app5,"2.0.0"}]},
                 all_deps(State, [app1], [{exclude, app3}])).


-endif.
