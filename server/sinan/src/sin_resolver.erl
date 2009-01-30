%%%-------------------------------------------------------------------
%%% Copyright (c) 2007 Eric Merritt, Martin Logan
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
%%% @copyright Erlware 2007
%%% @end
%%%-------------------------------------------------------------------
-module(sin_resolver).

-include("eunit.hrl").

%% API
-export([package_versions/3,
         package_dependencies/4,
	 find_package_location/4,
	 gather_version_info/2]).



%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Get the list of versions available for the specified package.
%%
%% @spec (Prefix, ErtsVersion, Package) -> VersionList
%% @end
%%--------------------------------------------------------------------
package_versions(Prefix, ErtsVersion, Package) when is_atom(Package) ->
    VList = gather_version_info(Prefix, ErtsVersion),
    get_package_versions(Package,
			 Prefix,
			 VList,
			 []);
package_versions(_, _, _) ->
    throw({error, "Package name must be an atom"}).

%%--------------------------------------------------------------------
%% @doc
%%  Get the list of dependencies for the specified package and the
%%  specified version.
%%
%% @spec (Prefix, ErtsVersion, Package, Version) -> Deps | Error
%% @end
%%--------------------------------------------------------------------
package_dependencies(Prefix, ErtsVersion, Package, Version) ->
    VList = gather_version_info(Prefix, ErtsVersion),
    NPackage = atom_to_list(Package),
    NDeps = get_package_dependencies(NPackage,
				     Version,
				     Prefix,
				     VList),
    NDeps.


%%--------------------------------------------------------------------
%% @doc
%%  Get the dependencies for a package and version.
%%
%% @spec (Package, Version, Prefix, Versions) -> Location
%% @end
%%--------------------------------------------------------------------
find_package_location(Prefix, ErtsVersion, Package, Version) ->
    VList = gather_version_info(Prefix, ErtsVersion),
    find_package_location_across_versions(Package, Version, Prefix, VList).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Get the list of subdirectories for the current directory
%%
%% @spec (Dir) -> ListOfDirs
%% @end
%%--------------------------------------------------------------------
list_subdirectories(Dir) ->
    lists:filter(fun(X) -> filelib:is_dir(X) end,
                 filelib:wildcard(Dir ++ "/*")).

%%--------------------------------------------------------------------
%% @doc
%%  Get the dependencies for a package and version.
%%
%% @spec (Package, Version, Prefix, Versions) -> Location
%% @end
%%--------------------------------------------------------------------
find_package_location_across_versions(Package, Version,
				      Prefix, [ErtsVersion | ErtsVersions]) ->
    PackageName = lists:flatten([Package, "-", Version]),
    FileName = filename:join([Prefix, "packages", ErtsVersion,
			      "lib", PackageName]),
    case filelib:is_dir(FileName) of
	true ->
	    FileName;
	false ->
	    find_package_location_across_versions(Package, Version,
						  Prefix, ErtsVersions)
    end;
find_package_location_across_versions(_, _, _, []) ->
    throw({error, "Unable to find location for package"}).

%%--------------------------------------------------------------------
%% @doc
%%  Get all versions that share the same major.minor version
%%  with the current erts. Thats erts version, not app version
%%
%% @spec gather_version_info(Prefix, ErtsVersion) -> ErtsVersions
%% @end
%%--------------------------------------------------------------------
gather_version_info(Prefix, ErtsVersion) ->
    ErtsVersions = lists:reverse(
		     lists:sort(
		       lists:map(fun(File) ->
					 filename:basename(File)
				 end,
				 list_subdirectories(
				   filename:join([Prefix, "packages"]))))),
    NewErtsVersion = get_major_minor(ErtsVersion, 0, []),
    lists:filter(fun(X) ->
			 starts_with(NewErtsVersion, X)
		 end,
		 ErtsVersions).

%%--------------------------------------------------------------------
%% @doc
%% Check that list1 is the same as the first part of list2
%%
%% @spec (List1, List2) -> true | false
%% @end
%%--------------------------------------------------------------------
starts_with([C1 | Rest1], [C1 | Rest2]) ->
    starts_with(Rest1, Rest2);
starts_with([], _) ->
    true;
starts_with(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% Get the major.minor version from version string.
%%
%% @spec (Name, Count, Acc) -> MajorMinor
%% @end
%%--------------------------------------------------------------------
get_major_minor([$. | _], 1, Acc) ->
    lists:reverse(Acc);
get_major_minor([$. | Rest], 0, Acc) ->
    get_major_minor(Rest, 1, [$. | Acc]);
get_major_minor([Head | Rest], Count, Acc) ->
    get_major_minor(Rest, Count, [Head | Acc]);
get_major_minor([], _, Acc) ->
    lists:reverse(Acc).

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
%%  Acc is a set. It this just makes sure only one entry is in the set
%%
%% @spec (Name, In, Acc) -> NewAcc
%% @end
%%--------------------------------------------------------------------
add_to_acc([Name | Rest1] , [Name | _], Acc) ->
    add_to_acc(Rest1, Acc, Acc);
add_to_acc(All, [_ | Rest], Acc) ->
    add_to_acc(All, Rest, Acc);
add_to_acc([Name | Rest], [], Acc) ->
    NewAcc = [Name | Acc],
    add_to_acc(Rest, NewAcc, NewAcc);
add_to_acc([], [], Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc
%%  Get all the versions for a package, search across all
%%  relavent major/minor versions.
%%
%% @spec (Package, Prefix, Versions, Acc) -> Versions
%% @end
%%--------------------------------------------------------------------
get_package_versions(Package, Prefix, [ErtsVersion | ErtsVersions], Acc) ->
    FileName = filename:join([Prefix, "packages", ErtsVersion,
			      "lib"]),
    AppVersions = lists:filter(fun(X) ->
				       starts_with(atom_to_list(Package)
						   ++ "-", filename:basename(X))
			       end,
			       list_subdirectories(FileName)),
    Versions = lists:map(fun(X) ->
				 get_version(X)
			    end,
			 AppVersions),
    get_package_versions(Package, Prefix, ErtsVersions,
			 add_to_acc(Versions, Acc, Acc));
get_package_versions(_, _, [], Acc) ->
    lists:sort(fun(A, B) -> ewr_util:is_version_greater(A, B) end,
	       Acc).

%%--------------------------------------------------------------------
%% @doc
%%  Get the dependencies for a package and version.
%%
%% @spec (Package, Version, Prefix, Versions) -> Deps
%% @end
%%--------------------------------------------------------------------
get_package_dependencies(Package, Version, Prefix,
			 [ErtsVersion | ErtsVersions]) ->
    DotAppName = lists:flatten([Package, ".app"]),
    AppName = lists:flatten([Package, "-", Version]),
    case file:consult(filename:join([Prefix, "packages",
				     ErtsVersion, "lib", AppName,
				     "ebin", DotAppName])) of
	{ok, [Term]} ->
	    handle_parse_output(Term);
	{error, _} ->
	    get_package_dependencies(Package, Version, Prefix,
				     [ErtsVersion | ErtsVersions])
    end;
get_package_dependencies(_, _, _, []) ->
    throw({error, "Unable to find dependencies for package"}).




%%--------------------------------------------------------------------
%% @doc
%%  get the version the deps and the versioned deps from an *.app
%%  term.
%%
%% @spec handle_parse_output(AppTerm) -> {Vsn, VersionedDeps, Deps} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
handle_parse_output({application, _, Ops}) ->
    lists:umerge(lists:sort(get_deps(Ops)),
		 lists:sort(get_ideps(Ops)));
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
handle_parse_output_test() ->
    Data = {application, testapp,
            [{vsn, "0.1.0"},
             {description, "test test test"},
             {versioned_dependencies,
              [{app1, "0.1.0"}, {app2, "0.33.1", gte},
               {app3, "33.11.3"}]},
             {applications, [app1, app2, app3, app4, app5]}]},
    ?assertMatch([{app3, [33, 11, 3]}, {app2, [0, 33, 1],gte},
                  {app1, [0, 1, 0]}, app5, app4],
                 handle_parse_output(Data)).

starts_with_test() ->
    ?assertMatch(true, starts_with("onetwo", "onetwothree")),
    ?assertMatch(false, starts_with("onetwo", "threetwoone")).

get_major_minor_test() ->
    ?assertMatch("5.6", get_major_minor("5.6.3", 0, [])),
    ?assertMatch("5.5", get_major_minor("5.5.5", 0, [])).

get_version_test() ->
    ?assertMatch("1.0", get_version("sinan-1.0")),
    ?assertMatch("1.3.2.2", get_version("bah-1.3.2.2")).

add_to_acc_test() ->
    Start = ["one", "two", "one", "three", "four", "two"],
    ?assertMatch(["one", "two", "three", "four"], add_to_acc(Start, [], [])).

gather_version_info_test() ->
    Prefix = "/usr/local/erlware",
    Version = "5.6.3",
    ?assertMatch([Version], gather_version_info(Prefix, Version)).

get_package_versions_test() ->
    ?assertMatch(["10.0.1"], get_package_versions(sinan,
						  "/usr/local/erlware",
						  ["5.6.3"], [])).
