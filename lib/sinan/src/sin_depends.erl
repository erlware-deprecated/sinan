%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
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
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Checks the dependencies in the system. Pulls down latest dependencies if 
%%% required.
%%% @end
%%% @copyright 2006
%%%---------------------------------------------------------------------------
-module(sin_depends).

%% API
-export([depends/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  Run the depends task.
%% 
%% @spec depends() -> ok.
%% @end
%%--------------------------------------------------------------------
depends(BuildRef, _) ->
    ProjectApps = gather_project_apps(BuildRef),
    fconf:store(BuildRef, "project.apps", ProjectApps),
    Repos = fconf:get_value(BuildRef, "repositories"),
    case catch ewr_depends:
               check_project_dependencies(Repos, 
                                          ProjectApps,
                                          get_supplimental(BuildRef)) of 
               {dependency_resolution_error, Reason} ->
                 ewl_talk:say(Reason),
                 throw(dependency_issue);
               {error, Reason} ->
                 ewl_talk:say(Reason),
                 throw(dependency_issue);
               {'EXIT', Reason} ->
                 ewl_talk:say(io_lib:format("'EXIT': ~p~n", [Reason])),
                 throw(dependency_issue);
               AllDeps ->
                 fconf:store(BuildRef, "project.deps", AllDeps),
                 sin_repo_fetcher:fetch(BuildRef, ProjectApps, AllDeps),
                 save_deps(BuildRef, AllDeps),
                 update_sigs(BuildRef)
            
         end.



%%====================================================================
%% Internal functions
%%==================================================================== 
%%--------------------------------------------------------------------
%% @doc 
%%  Add the latest version of eunit to the dependency list.
%% @end
%% @private
%%--------------------------------------------------------------------
get_supplimental(BuildRef) ->
    case fconf:get_value(BuildRef, "eunit") of
        "disable" ->
            [];
        _ ->
            [{eunit, []}]
    end.



%%--------------------------------------------------------------------
%% @spec save_deps(Deps) -> ok
%% 
%% @doc 
%%  Saves the list of dependencies for later use.
%% @end
%% @private
%%--------------------------------------------------------------------
save_deps(BuildRef, Deps) ->
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    filelib:ensure_dir(filename:join([BuildDir, "info", "tmp"])),
    Depsf = filename:join([BuildDir, "info", "deps"]),
    case file:open(Depsf, write) of
        {error, _} ->
            ewl_talk:say("Couldn't open ~s for writing. Unable to "
                         "write dependency information",
                         [Depsf]),
            throw(unable_to_write_dep_info);
        {ok, IoDev} ->
            io:format(IoDev, "~p.", [Deps]),
            file:close(IoDev)
    end,
    save_repo_apps(BuildRef, BuildDir).

%%--------------------------------------------------------------------
%% @spec save_repo_apps(BuildDir) -> ok.
%% 
%% @doc 
%%  Saves the list of repo apps to info.
%% @end
%%--------------------------------------------------------------------
save_repo_apps(BuildRef, BuildDir) ->
    Apps = fconf:get_value(BuildRef, "project.repoapps"),
    Repsf = filename:join([BuildDir, "info", "repoapps"]),
    case file:open(Repsf, write) of
        {error, _} ->
            ewl_talk:say("Couldn't open ~s for writing. Unable to "
                         "write dependency information",
                         [Repsf]),
            throw(unable_to_write_dep_info);
        {ok, IoDev} ->
            io:format(IoDev, "~p.", [Apps]),
            file:close(IoDev)
    end.

%%-------------------------------------------------------------------
%% @spec gather_project_apps() -> ListOfAppVsn
%% @doc
%%   Roll through the list of project apps and gather the app
%%   name and version number.
%% @end
%% @private
%%-------------------------------------------------------------------
gather_project_apps(BuildRef) ->
    gather_project_apps(BuildRef, 
                        fconf:get_value(BuildRef, "project.applist"), []).

gather_project_apps(BuildRef, [AppName | T], Acc) ->
    Vsn = fconf:get_value(BuildRef, {path, ["apps", AppName, "vsn"]}),
    Name = fconf:get_value(BuildRef, {path, ["apps", AppName, "name"]}),
    OpenDeps = fconf:get_value(BuildRef, {path, 
                                       ["apps", AppName, 
                                        "applications"]}),
    IncludedDeps = fconf:get_value(BuildRef, {path,
                                           ["apps", AppName,
                                            "included_applications"]}),

    VersionedDeps = fconf:get_value(BuildRef, {path, 
                                          ["apps", AppName,
                                           "versioned_dependencies"]}),
    NDeps = {merge(OpenDeps, IncludedDeps), VersionedDeps},

    fconf:store(BuildRef, {path, ["apps", AppName, "deps"]}, NDeps),

    gather_project_apps(BuildRef, T, [{Name, Vsn, NDeps} | Acc]);
gather_project_apps(_BuildRef, [], Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @doc 
%%   Merge the two types of deps removing duplicates.
%% @spec merge(OpenDeps, IncludedDeps) -> MergedList.
%% @end
%%--------------------------------------------------------------------
merge(OpenDeps, undefined) ->
    OpenDeps;
merge(OpenDeps, IncludedDeps) ->
    lists:umerge(lists:sort(OpenDeps), lists:sort(IncludedDeps)).

%%--------------------------------------------------------------------
%% @spec update_sigs() -> ok
%% 
%% @doc 
%%  Update the sigs for all of the 'verifiable' apps.
%% @end
%% @private
%%--------------------------------------------------------------------
update_sigs(BuildRef) ->
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    update_app_sigs(BuildRef, BuildDir, 
                    fconf:get_value(BuildRef, "project.applist")).

%%--------------------------------------------------------------------
%% @spec update_app_sigs(BuildDir, AppList) -> ok
%% 
%% @doc 
%%  Update the signatures for each of the *.app files in the AppList.
%% @end
%% @private
%%--------------------------------------------------------------------
update_app_sigs(BuildRef, BuildDir, [H | T]) ->
    App = fconf:get_value(BuildRef, {path, ["apps", H, "dotapp"]}),
    sin_sig:update("dep", BuildDir, App),
    update_app_sigs(BuildRef, BuildDir, T);
update_app_sigs(_BuildRef, _BuildDir, []) ->
    ok.
