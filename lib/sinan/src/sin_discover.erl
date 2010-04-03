%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%%-------------------------------------------------------------------
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
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  The module handles building the
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%%--------------------------------------------------------------------------
-module(sin_discover).

-include("etask.hrl").

%% API
-export([discover/2]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%%  Run the discover task.
%% @spec discover(ProjectDir, Config) -> ok
%% @end
%%--------------------------------------------------------------------
discover(ProjectDir, Config) ->
    {ok, BuildDir} = dict:find("build_dir", Config),
    AppDirs = look_for_app_dirs(Config, BuildDir, ProjectDir),
    NewConfig = build_app_info(Config, AppDirs, []),
    get_repo_location(NewConfig).


%%====================================================================
%%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @doc
%%   Find the location of the repository.
%% @spec (Config) -> ok
%% @end
%% @private
%%-------------------------------------------------------------------
get_repo_location(Config) ->
    Var = os:getenv("SINAN_REPO"),
    get_repo_location(Config, Var).

get_repo_location(Config, false) ->
    Repo = filename:join([os:getenv("HOME"), ".sinan", "repo"]),
    filelib:ensure_dir(filename:join([Repo, "tmp"])),
    dict:store("project.repository", Repo, Config);
get_repo_location(Config, Loc) ->
    filelib:ensure_dir(filename:join([Loc, "tmp"])),
    dict:store("project.repository", Loc, Config).

%%-------------------------------------------------------------------
%% @doc
%%   Given a list of app dirs retrieves the application names.
%%
%% @spec (Config::dict(), List::list(), Acc::list()) -> AppNames
%% @end
%%-------------------------------------------------------------------
build_app_info(Config, [H|T], Acc) ->
    AppName = filename:basename(H),
    AppFile = filename:join([H, "ebin", string:concat(AppName, ".app")]),
    case file:consult(AppFile) of
        {ok, [{application, Name, Details}]} ->
            Config2 = process_details("apps." ++ AppName ++ ".",
                                      [{"name", Name},
                                       {"dotapp", AppFile},
                                       {"basedir", H} | Details], Config),
            build_app_info(Config2,  T, [AppName | Acc]);
        {error, {_, Module, Desc}} ->
            Error = Module:format_error(Desc),
            throw({error, {invalid_app_file, Error},
                   io_lib:format("*.app file is invalid for ~s at ~s",
                    [AppName, AppFile])});
        {error, Error} ->
            throw({error, {no_app_file, Error},
                   io_lib:format("No *.app file found for ~s at ~s",
                                 [AppName, AppFile])})
    end;
build_app_info(Config, [], Acc) ->
    dict:store("project.applist", Acc, Config).

%%--------------------------------------------------------------------
%% @doc
%%  Convert the details list to something that fits into the config
%%  nicely.
%%
%% @spec (BaseKey::string(), List::list(), Config::dict()) -> NewConfig
%% @end
%% @private
%%--------------------------------------------------------------------
process_details(BaseName, [{Key, Value} | T], Config) when is_atom(Key) ->
    process_details(BaseName, T, dict:store(BaseName ++ atom_to_list(Key),
                                            Value, Config));
process_details(BaseName, [{Key, Value} | T], Config) when is_list(Key)->
    process_details(BaseName, T, dict:store(BaseName ++ Key, Value, Config));
process_details(_, [], Config) ->
    Config.

%%-------------------------------------------------------------------
%% @doc
%%  Roles through subdirectories of the build directory looking
%%  for directories that have a src and an ebin subdir. When it
%%  finds one it stops recursing and adds the directory to the list
%%  to return.
%%
%% @spec (Config, BuildDir, ProjectDir) -> AppDirs
%% @end
%% @private
%%-------------------------------------------------------------------
look_for_app_dirs(Config, BuildDir, ProjectDir) ->
    Ignorables = [BuildDir] ++ case dict:find("ignore_dirs", Config) of
				   {ok, Value} -> Value;
				   _ -> []
			       end,
    case look_for_app_dirs(Config, BuildDir, ProjectDir, "",
                           Ignorables, []) of
        [] ->
            throw({error, no_app_directories,
                   "Unable to find any application directories."
                   " aborting now"});
        Else ->
            Else
    end.

look_for_app_dirs(_, BuildDir, _Parent, BuildDir, _Ignore, Acc) ->
    Acc;
look_for_app_dirs(Config, BuildDir, Parent, Sub, Ignorables, Acc) ->
    case sin_utils:is_dir_ignorable(Sub, Ignorables) of
        true ->
            Acc;
        false ->
            process_app_dir(Config, BuildDir, Parent, Sub, Ignorables, Acc)
    end.


%%--------------------------------------------------------------------
%% @doc
%%  Process the app dir to see if it is an application directory.
%%
%% @spec (Config, BuildDir, Parent, Sub, Ignorables, Acc) -> ListOfDirs
%% @end
%% @private
%%--------------------------------------------------------------------
process_app_dir(Config, BuildDir, Parent, Sub, Ignorables, Acc) ->
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
                                        look_for_app_dirs(Config,
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
%% @doc
%%  Given a directory checks of the name is src or ebin, compares
%%  against its state and returns an indicator if the parent is a
%%  app dir.
%% @spec (File, State, Type) -> {both, Other} | State
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

