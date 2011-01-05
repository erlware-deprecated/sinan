%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Erlware
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
%%%-------------------------------------------------------------------
%%% @doc
%%%  Resolves individual items for the dependency engine.
%%% @copyright 2007-2010 Erlware
%%% @end
%%%-------------------------------------------------------------------
-module(sin_resolver).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([package_versions/2,
         package_dependencies/3,
	 find_package_location/3]).



%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Get the list of versions available for the specified package.
%%
%% @spec (LibDir, Package) -> VersionList
%% @end
%%--------------------------------------------------------------------
package_versions(LibDir, Package) when is_atom(Package) ->
    lists:sort(fun ewr_util:is_version_greater/2,
               get_package_versions(Package,
                                    LibDir)).


%%--------------------------------------------------------------------
%% @doc
%%  Get the list of dependencies for the specified package and the
%%  specified version.
%%
%% @spec (LibDir, Package, Version) -> Deps | Error
%% @end
%%--------------------------------------------------------------------
package_dependencies(LibDir, Package, Version) ->
    NPackage = atom_to_list(Package),
    NDeps = get_package_dependencies(NPackage,
                                     Version,
                                     LibDir),
    NDeps.


%%--------------------------------------------------------------------
%% @doc
%%  Get the dependencies for a package and version.
%%
%% @spec (Package, LibDir, Version) -> Location
%% @end
%%--------------------------------------------------------------------
find_package_location(LibDir, Package, Version) when is_atom(Package) ->
    find_package_location(LibDir, atom_to_list(Package), Version);
find_package_location(LibDir, Package, Version) ->
    filename:join([LibDir, Package ++ "-" ++ Version]).

%%====================================================================
%%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Get the version from an app-version
%%
%% @spec (Name) -> Version
%% @end
%%--------------------------------------------------------------------
get_version([$- | Rest]) ->
    Rest;
get_version([_ | Rest]) ->
    get_version(Rest);
get_version([]) ->
    throw({error, "Unable to find package version"}).

%%--------------------------------------------------------------------
%% @doc
%%  Get all the versions for a package, search across all
%%  relavent major/minor versions.
%%
%% @spec (Package, LibDir) -> Versions
%% @end
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @doc
%%  Get the dependencies for a package and version.
%%
%% @spec (Package, Version, LibDir) -> Deps
%% @end
%%--------------------------------------------------------------------
get_package_dependencies(Package, Version, LibDir) ->
    DotAppName = lists:flatten([Package, ".app"]),
    AppName = lists:flatten([Package, "-", Version]),
    case file:consult(filename:join([LibDir,AppName,
				     "ebin", DotAppName])) of
	{ok, [Term]} ->
	    handle_parse_output(Term);
	{error, _} ->
	    throw({error, "Invalid application"})
    end.

%%--------------------------------------------------------------------
%% @doc
%%  get the version the deps and the versioned deps from an *.app
%%  term.
%%
%% @spec handle_parse_output(AppTerm) -> {Vsn, VersionedDeps, Deps} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
handle_parse_output({application, _, Ops}) ->
    {get_deps(Ops), get_ideps(Ops)};
handle_parse_output(_) ->
   throw({error, "Invalid dependency info"}).


%%--------------------------------------------------------------------
%% @doc
%%  Get the list of non-versioned dependencies from the oplist. This
%%  is specifed in the applications entry.
%% @spec get_deps(OpList) -> Dependencies
%% @end
%% @private
%%--------------------------------------------------------------------
get_deps([{applications, List} | _T]) ->
    List;
get_deps([_ | T]) ->
    get_deps(T);
get_deps([]) ->
    [].


%%--------------------------------------------------------------------
%% @doc
%%  Get the list of included applications.
%% @spec get_ideps(OpList) -> IncludedDependencies
%% @end
%% @private
%%--------------------------------------------------------------------
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

