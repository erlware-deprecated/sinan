%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Erlware
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
%%%  The module handles building the
%%% @end
%%% @copyright (C) 2007, Erlware
%%% @copyright 2006 by Eric Merritt <cyberlync@gmail.com>
%%%--------------------------------------------------------------------------
-module(sin_discover).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, discover/1]).

-define(TASK, discover).
-define(DEPS, [setup]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start() -> ok.
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Discovers information about the project and "
    "updates the project information",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = []},
    eta_task:register_task(TaskDesc).


%%--------------------------------------------------------------------
%% @spec do_task(BuildRef, Args) -> ok
%%
%% @doc
%%  dO the task defined in this module.
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    discover(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Run the discover task.
%% @spec discover() -> ok.
%% @end
%%--------------------------------------------------------------------
discover(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK, "Discovering project layout and structure ..."),
    ProjectDir = fconf:get_value(BuildRef, "project.dir"),
    BuildDir = fconf:get_value(BuildRef, "build_dir", "_build"),
    AppDirs = look_for_app_dirs(BuildRef, BuildDir, ProjectDir),
    build_app_info(BuildRef, AppDirs, []),
    get_repo_location(BuildRef),
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @spec get_repo_location() -> ok.
%% @doc
%%   Find the location of the repository.
%% @end
%% @private
%%-------------------------------------------------------------------
get_repo_location(BuildRef) ->
    Var = os:getenv("SINAN_REPO"),
    get_repo_location(BuildRef, Var).

get_repo_location(BuildRef, false) ->
    Repo = filename:join([os:getenv("HOME"), ".sinan", "repo"]),
    filelib:ensure_dir(filename:join([Repo, "tmp"])),
    fconf:store(BuildRef, "project.repository", Repo);
get_repo_location(BuildRef, Loc) ->
    filelib:ensure_dir(filename:join([Loc, "tmp"])),
    fconf:store(BuildRef, "project.repository", Loc).

%%-------------------------------------------------------------------
%% @spec build_app_info(List::list(), Acc::list()) -> AppNames
%% @doc
%%   Given a list of app dirs retrieves the application names.
%% @end
%%-------------------------------------------------------------------
build_app_info(BuildRef, [H|T], Acc) ->
    AppName = filename:basename(H),
    AppFile = filename:join([H, "ebin", string:concat(AppName, ".app")]),
    case file:consult(AppFile) of
        {ok, [{application, Name, Details}]} ->
            Dict = process_details([{"name", Name},
                                    {"dotapp", AppFile},
                                    {"basedir", H} | Details], dict:new()),
            fconf:store(BuildRef, {path, ["apps",
                                       AppName]}, Dict),
            build_app_info(BuildRef, T, [AppName | Acc]);
        {error, {_, Module, Desc}} ->
            Error = Module:format_error(Desc),
            eta_event:task_fault(BuildRef, ?TASK,
                                 {"Invalid app file for ~s: ~s",
                                  [AppName, lists:flatten(Error)]}),
            throw(invalid_app_file);
        {error, _} ->
            eta_event:task_fault(BuildRef, ?TASK, {"No app file for ~s. Aborting build.",
                                                   [AppName]}),
            throw(no_app_file)
    end;
build_app_info(BuildRef, [], Acc) ->
    fconf:store(BuildRef, "project.applist", Acc).


%%--------------------------------------------------------------------
%% @spec process_details(List, Acc) -> NewDetails.
%%
%% @doc
%%  Convert the details list to something that fits into the config
%%  nicely.
%% @end
%% @private
%%--------------------------------------------------------------------
process_details([{Key, Value} | T], Dict) when is_atom(Key) ->
    process_details(T, dict:store(atom_to_list(Key), Value, Dict));
process_details([{Key, Value} | T], Dict) when is_list(Key)->
    process_details(T, dict:store(Key, Value, Dict));
process_details([], Dict) ->
    Dict.

%%-------------------------------------------------------------------
%% @spec look_for_app_dirs(Pwd, Acc) -> AppDirs
%% @doc
%%  Roles through subdirectories of the build directory looking
%%  for directories that have a src and an ebin subdir. When it
%%  finds one it stops recursing and adds the directory to the list
%%  to return.
%% @end
%% @private
%%-------------------------------------------------------------------
look_for_app_dirs(BuildRef, BuildDir, ProjectDir) ->
    Ignorables = fconf:get_value(BuildRef, "ignore_dirs", []),
    case look_for_app_dirs(BuildRef, BuildDir, ProjectDir, "",
                           Ignorables, []) of
        [] ->
            eta_event:task_fault(BuildRef, ?TASK,
                                 "Unable to find any application directories."
                                 " aborting now"),
            throw({error, no_app_directories});
        Else ->
            Else
    end.

look_for_app_dirs(_, BuildDir, _Parent, BuildDir, _Ignore, Acc) ->
    Acc;
look_for_app_dirs(BuildRef, BuildDir, Parent, Sub, Ignorables, Acc) ->
    case sin_utils:is_dir_ignorable(Sub, Ignorables) of
        true ->
            Acc;
        false ->
            process_app_dir(BuildRef, BuildDir, Parent, Sub, Ignorables, Acc)
    end.


%%--------------------------------------------------------------------
%% @spec process_app_dir(BuildDir, Parent, Sub, Acc) -> ListOfDirs.
%%
%% @doc
%%  Process the app dir to see if it is an application directory.
%% @end
%% @private
%%--------------------------------------------------------------------
process_app_dir(BuildRef, BuildDir, Parent, Sub, Ignorables, Acc) ->
    Pwd = filename:join([Parent, Sub]),
    case filelib:is_dir(Pwd) of
        true ->
            {ok, Dirs} = file:list_dir(Pwd),
            Res = lists:foldl(fun(F, AccIn) ->
                                      File = filename:join([Pwd, F]),
                                      process_dirs(File, F, AccIn)
                              end, none, Dirs),
            case {Res, Dirs} of
                {both, _} ->
                    [Pwd | Acc];
                {_, []} ->
                    Acc;
                {_, _} ->
                    lists:foldl(fun(Elem, NAcc) ->
                                        look_for_app_dirs(BuildRef,
                                                          BuildDir,
                                                          Pwd, Elem,
                                                          Ignorables,
                                                          NAcc)
                                end, Acc, Dirs)
            end;
        false ->
            Acc
    end.

%%-------------------------------------------------------------------
%% @spec process_dirs(File, State) -> {both, _} | State
%% @doc
%%  Given a directory checks of the name is src or ebin, compares
%%  against its state and returns an indicator if the parent is a
%%  app dir.
%% @end
%% @private
%%-------------------------------------------------------------------
process_dirs(File, F, ebin) ->
    case {filelib:is_dir(File), F} of
        {true, "src"} ->
            both;
        _ ->
            ebin
    end;
process_dirs(File, F, src) ->
    case {filelib:is_dir(File), F} of
        {true, "ebin"} ->
            both;
        _ ->
            src
    end;
process_dirs(File, F, Type)  ->
    case {filelib:is_dir(File), F} of
        {true, "ebin"} ->
            ebin;
        {true, "src"} ->
            src;
        _ ->
            Type
    end.

%%====================================================================
%% tests
%%====================================================================

