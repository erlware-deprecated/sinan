%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @doc
%%%  Resolves individual items for the dependency engine.
%%% @copyright 2007-2011 Erlware
%%% @end
%%%-------------------------------------------------------------------
-module(sin_resolver).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%% API
-export([package_versions/2,
         package_dependencies/3,
	 find_package_location/3,
	 format_exception/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Get the list of versions available for the specified package.
-spec package_versions(string(), atom()) -> [Vsn::string()].
package_versions(LibDir, Package) when is_atom(Package) ->
    lists:sort(fun ewr_util:is_version_greater/2,
               get_package_versions(Package,
                                    LibDir)).


%% @doc Get the list of dependencies for the specified package and the specified
%% version.
-spec package_dependencies(LibDir::string(), Package::atom(),
			   Version::string()) -> Deps::term().
package_dependencies(LibDir, Package, Version) ->
    NPackage = atom_to_list(Package),
    NDeps = get_package_dependencies(NPackage,
                                     Version,
                                     LibDir),
    NDeps.


%% @doc Get the dependencies for a package and version.
-spec find_package_location(atom(), string(), string()) -> string().
find_package_location(LibDir, Package, Version) when is_atom(Package) ->
    find_package_location(LibDir, atom_to_list(Package), Version);
find_package_location(LibDir, Package, Version) ->
    filename:join([LibDir, Package ++ "-" ++ Version]).

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).
%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Get the version from an app-version
-spec get_version(string()) -> Vsn::string().
get_version([$- | Rest]) ->
    Rest;
get_version([_ | Rest]) ->
    get_version(Rest);
get_version([]) ->
    ?SIN_RAISE(unable_to_parse,  ["Unable to find package version"]).

%% @doc Get all the versions for a package, search across all relavent
%% major/minor versions.
-spec get_package_versions(atom(), string()) -> [Version::string()].
get_package_versions(Package, LibDir) ->

    AppVersions = lists:filter(fun(X) ->
				       filelib:is_dir(X) end,
			       filelib:wildcard(filename:join(LibDir,
							      Package) ++
						"-*")),
    lists:map(fun(X) ->
		      get_version(filename:basename(X))
	      end,
	      AppVersions).

%% @doc Get the dependencies for a package and version.
-spec get_package_dependencies(atom(), string(), string()) ->
    {[atom()], [atom()]}.
get_package_dependencies(Package, Version, LibDir) ->
    DotAppName = lists:flatten([Package, ".app"]),
    AppName = lists:flatten([Package, "-", Version]),
    Location = filename:join([LibDir,AppName,
			      "ebin", DotAppName]),
    case file:consult(Location) of
	{ok, [Term]} ->
	    handle_parse_output(Term);
	{error, _} ->
	    ?SIN_RAISE({invalid_app_file, Location},
		       "Invalid application: ~s", [Location])
    end.

%% @doc get the version the deps and the versioned deps from an *.app term.
-spec handle_parse_output(AppInfo::term()) ->
    {Vsn::string(), VersionedDeps::list(), Deps::list()}.
handle_parse_output({application, _, Ops}) ->
    {get_deps(Ops), get_ideps(Ops)};
handle_parse_output(_) ->
   ?SIN_RAISE(invalid_app_data, "Invalid dependency info").

%% @doc Get the list of non-versioned dependencies from the oplist. This is
%% specifed in the applications entry.
-spec get_deps([{atom(), term()}]) -> [atom()].
get_deps([{applications, List} | _T]) ->
    List;
get_deps([_ | T]) ->
    get_deps(T);
get_deps([]) ->
    [].

%% @doc Get the list of included applications.
-spec get_ideps(OpList::[{atom(), term()}]) -> term().
get_ideps([{included_applications, List} | _T]) ->
    List;
get_ideps([_ | T]) ->
    get_ideps(T);
get_ideps([]) ->
    [].

%%====================================================================
%% tests
%%====================================================================
get_version_test() ->
    ?assertMatch("1.0", get_version("sinan-1.0")),
    ?assertMatch("1.3.2.2", get_version("bah-1.3.2.2")).

