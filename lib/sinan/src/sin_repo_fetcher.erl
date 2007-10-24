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
%%%   Gets the list of fetchables from the environment and loads them
%%%   into the local repository.
%%% @end
%%% @copyright 2006
%%%---------------------------------------------------------------------------
-module(sin_repo_fetcher).

%% API
-export([fetch/3]).

%%====================================================================
%% API
%%====================================================================

%%-------------------------------------------------------------------
%% @spec fetch() -> ok.
%% @doc
%%  Given the metadata about the project goes through and pulls 
%%  down the required libraries from the specified repo.
%% @end
%%-------------------------------------------------------------------
fetch(BuildRef, ProjectApps, ProjectDeps) ->
    Repo = ensure_repo(BuildRef),
    fetch_deps(BuildRef, Repo, ProjectApps, ProjectDeps).

%%====================================================================
%% Internal functions
%%====================================================================

%%-------------------------------------------------------------------
%% @spec ensure_repo(Env) -> ok.
%% @doc
%%  Make sure a repo location is specified.
%% @end
%% @private
%%-------------------------------------------------------------------
ensure_repo(BuildRef) ->
    case fconf:get_value(BuildRef, "project.repository") of
        undefined ->
            ewl_talk:say(["I need a local repository specified in the",
                          "build config!"]),
            throw(no_local_repo_specified);
        Repo ->
            filelib:ensure_dir(filename:join([Repo, "tmp"])),
            Repo
    end.

%%-------------------------------------------------------------------
%% @spec fetch_deps(Repo, ProjectApp, ProjectDeps)
%% @doc
%%  Fetch down and individual required app from the http repository.
%% @end
%% @private
%%-------------------------------------------------------------------
fetch_deps(BuildRef, LocalRepo, ProjectApps, ProjectDeps) ->
    Repos = fconf:get_value(BuildRef, "repositories"),
    fetch_dep(BuildRef, Repos, LocalRepo, ProjectApps, ProjectDeps, []).



%%--------------------------------------------------------------------
%% @spec fetch_dep(RemoteRepos, LocalRepo, ProjectApps, ProjectDeps, Acc) ->
%%   RepoDeps.
%% @doc 
%%  Fetch the individual deps from the system, returning a list of apps
%%  that are non-project but required.
%% @end
%% @private
%%--------------------------------------------------------------------
fetch_dep(BuildRef, RemoteRepos, LocalRepo, 
          ProjectApps, [App = {AppName, Vsn} | T], Acc) ->
    case is_project_app(AppName, ProjectApps) of
        true ->
            fetch_dep(BuildRef, RemoteRepos, LocalRepo, ProjectApps, T, Acc);
        false ->
            ewl_talk:say("Pulling ~w-~s from repository if non-local", 
                         [AppName, Vsn]),
            case ewr_fetch:fetch_binary_package(RemoteRepos, AppName, 
                                                Vsn, LocalRepo) of
                {error, {_Type, Reason}} ->
                    ewl_talk:say("Unable to fetch ~w version (~w) from repo "
                                 " due to ~s",
                                 [AppName, Vsn, Reason]),
                    throw(unable_to_fetch_from_repo);
                {error, Reason} ->
                    ewl_talk:say("Unable to fetch ~w version (~w) from repo "
                                 " due to ~s",
                                 [AppName, Vsn, Reason]),
                    throw(unable_to_fetch_from_repo);
                _ ->
                    fetch_dep(BuildRef, RemoteRepos,
                              LocalRepo, ProjectApps, T, [App | Acc])
            end
    end;
fetch_dep(BuildRef, _RemoteRepos, _LocalRepo, _ProjectApps, [], Acc) ->
    fconf:store(BuildRef, "project.repoapps", Acc),
    ok.

%%--------------------------------------------------------------------
%% @spec is_project_app(AppName, AppList) -> true | false.
%% 
%% @doc 
%%  check to see if AppName is a project app.
%% @end
%% @private
%%--------------------------------------------------------------------
is_project_app(AppName, [{AppName, _, _} | _T]) ->
    true;
is_project_app(App, [_H | T]) ->
    is_project_app(App, T);
is_project_app(_App, []) ->
    false.
