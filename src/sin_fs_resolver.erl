%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Erlware, LLC
%%% @doc
%%%  A resolver for resolving applications in the file system
%%% @end
%%% Created : 17 Sep 2011 by Eric Merritt <>
%%%-------------------------------------------------------------------
-module(sin_fs_resolver).

-behaviour(sin_dep_resolver).

-export([new/2, app_dependencies/3, app_versions/2, resolve/3]).

-include("internal.hrl").

%%============================================================================
%% Api
%%============================================================================
-spec new(sin_config:config(), sin_state:state()) -> sin_dep_resolver:impl().
new(Config, State) ->
    BuildDir = sin_state:get_value(build_dir, State),
    AppBDir = filename:join([BuildDir, "apps"]),
    ErlLib = case os:getenv("ERL_LIBS") of
                 false ->
                     [];
                 Libs ->
                     Libs
             end,
    {[AppBDir, code:lib_dir() |
         get_erl_lib_path(ErlLib) ++
          Config:match(dep_dirs, [])], State,
     sin_state:get_value(project_applist, State)}.

-spec app_dependencies(sin_dep_resolver:state(),
                       sin_dep_solver:app(),
                       sin_dep_solver:version()) ->
                              {sin_dep_resolver:state(),
                               [sin_dep_solver:spec()]}.
app_dependencies(RState={PathList, State, ProjectApps}, App, Ver) ->
    case lists:member(App, ProjectApps) of
        true ->
            {Deps, VersionedDeps} = get_app_constraints(State,
                                            sin_state:get_value({apps, App, dotapp}, State)),
            {RState, Deps ++ VersionedDeps};
        false ->
            case look_for_dependency_path(State, PathList, App, Ver) of
                {ok, Path} ->
                    {RState, get_dependency_information(State, Path, App)};
                not_found ->
                    ?SIN_RAISE(State, {unable_to_find_dependencies, App, Ver})
            end
    end.

-spec app_versions(sin_dep_resolver:impl(), sin_dep_solver:app()) ->
                          {sin_dep_resolver:impl(),
                           [sin_dep_solver:versions()]}.
app_versions(RState={PathList, State, ProjectApps}, App) ->
    case lists:member(App, ProjectApps) of
        true ->
            {RState, [get_app_vsn(State, sin_state:get_value({apps, App, dotapp}, State))]};
        false ->
            VsnList = get_available_versions(State, PathList, App),
            %% Make sure the elements are unique
            {RState, sets:to_list(sets:from_list(VsnList))}
    end.

%% @doc resolve the on disc location of the dependency
-spec resolve(sin_dep_resolver:impl(), sin_dep_solver:app(), sin_dep_solver:version()) ->
                     {sin_dep_resolver:impl(), string()}.
resolve(RState={PathList, State, ProjectApps}, App, Version) ->
    case lists:member(App, ProjectApps) of
        true ->
            BuildDir = sin_state:get_value(build_dir, State),
            Path =
                filename:join([BuildDir, "apps",
                               erlang:atom_to_list(App) ++ "-" ++Version]),
            {RState, Path};
        false ->
            case look_for_dependency_path(State, PathList, App, Version) of
                {ok, Path} ->
                    {RState, Path};
                not_found ->
                    ?SIN_RAISE(State, {unable_to_find_dependencies, App, Version})
            end
    end.
%%============================================================================
%% Internal Functions
%%============================================================================
-spec get_erl_lib_path(string()) -> [string()].
get_erl_lib_path(Paths) ->
    Elements = re:split(Paths, get_path_sep()),
    lists:map(fun(El) ->
                      erlang:binary_to_list(El)
              end, Elements).

-spec get_path_sep() -> char().
get_path_sep() ->
    Test = filename:join("o", "o"),
    {Sep, got} = lists:foldl(fun($o, {_, no}) ->
                                     {ignored, yes};
                                (Sep, {_, yes}) ->
                                     {Sep, got};
                                (_, Acc) ->
                                     Acc
                             end, {[], no}, Test),
    [Sep].


-spec look_for_dependency_path(sin_state:impl(), [string()],
                               sin_dep_solver:app(), sin_dep_solver:version()) ->
                                      {ok, string()} | not_found.
look_for_dependency_path(State, PathList, App, Ver) ->
    case ec_lists:search(fun(Path) ->
                                 Name = erlang:atom_to_list(App) ++ "-" ++ Ver,
                                 FullPath = filename:join(Path, Name),
                                 case sin_utils:file_exists(State, FullPath) of
                                     true ->
                                         {ok, FullPath};
                                     false ->
                                         not_found
                                 end
                         end, PathList) of
        {ok, FullPath, _} ->
            {ok, FullPath};
        _ ->
            not_found
    end.

%% @doc We do this by plugging looking in three places. The deps.config, if
%% it exists, then in the versioned_dependencies element in the app
%% metadata, and finally in the simple list of dependencies in
%% applications element of the app metadata.
-spec get_dependency_information(sin_state:state(), [string()], sin_dep_solver:app()) ->
                                        [sin_dep_solver:spec()].
get_dependency_information(State, FullPath, AppName) ->
    {Deps, VersionedDeps} = get_app_constraints(State, FullPath, AppName),
    Deps ++ VersionedDeps ++ get_dep_config_deps(State, FullPath).

-spec get_dep_config_deps(sin_state:state(), string()) ->
                                 [sin_dep_solver:spec()].
get_dep_config_deps(State, FullPath) ->
    DepConfig = filename:join(FullPath, "deps.config"),

    case file:consult(DepConfig) of
        {ok, Terms} ->
            Terms;
        {error, enoent} ->
            [];
        Error ->
            ?SIN_RAISE(State, {error_opening_file, FullPath, Error})
    end.

-spec get_app_constraints(sin_state:state(), string(), sin_dep_solver:app()) ->
                                 [sin_dep_solver:spec()].
get_app_constraints(State, FullPath, AppName) ->
    AppConfig = filename:join([FullPath, "ebin",
                               erlang:atom_to_list(AppName) ++ ".app"]),
    get_app_constraints(State, AppConfig).

-spec get_app_constraints(sin_state:state(), string()) ->
                                 [sin_dep_solver:spec()].
get_app_constraints(State, AppConfig) ->
    case file:consult(AppConfig) of
        {ok, [{application, _AppName, Terms}]} ->
            DirectDeps =
                case lists:keyfind(applications, 1, Terms) of
                    {applications, Deps0} ->
                        Deps0;
                    _ ->
                        []
                end,
            Included =
                case lists:keyfind(included_applications, 1, Terms) of
                    {included_applications, Deps1} ->
                        Deps1;
                    _ ->
                        []
                end,
            VersionedDeps =
                case lists:keyfind(dep_constraints, 1, Terms) of
                    {dep_constraints, Deps2} ->
                        Deps2;
                    _ ->
                        []
                end,
            {DirectDeps ++ Included, VersionedDeps};
        {error, enoent} ->
            ?SIN_RAISE(State, {no_app_config, AppConfig});
        {error, SomeOtherError} ->
            ?SIN_RAISE(State, {error_opening_file, AppConfig, SomeOtherError})
    end.

-spec get_available_versions(sin_state:state(), [string()], sin_dep_solver:app()) ->
                                    [sin_dep_solver:version()].
get_available_versions(State, PathList, App) ->
    lists:foldl(fun(Path, Acc0) ->
                        case file:list_dir(Path) of
                            {ok, Dirs} ->
                                lists:foldl(fun(Dir, Acc1) ->
                                                    case re:split(Dir, "^" ++
                                                                      erlang:atom_to_list(App) ++
                                                                      "-(\\S+)$") of
                                                        [<<>>, Version, <<>>] ->
                                                            [erlang:binary_to_list(Version) | Acc1];
                                                        _ ->
                                                            Acc1
                                                    end
                                            end, Acc0, Dirs);
                            {error, enoent} ->
                                Acc0;
                            Error ->
                                ?SIN_RAISE(State,
                                           {unable_to_access_directory, Error, Path})
                        end
                end, [], PathList).

-spec get_app_vsn(sin_state:state(), [string()]) ->
                         string().
get_app_vsn(State, AppConfigPath) ->
    case file:consult(AppConfigPath) of
        {ok, [{application, _AppName, Terms}]} ->
                case lists:keyfind(vsn, 1, Terms) of
                    {vsn, Vsn} ->
                        Vsn;
                    _ ->
                        ?SIN_RAISE(State, {app_config_contains_no_version, AppConfigPath})
                end;
        {error, enoent} ->
            ?SIN_RAISE(State, {no_app_config, AppConfigPath});
        {error, SomeOtherError} ->
            ?SIN_RAISE(State, {error_opening_file, AppConfigPath, SomeOtherError})
    end.