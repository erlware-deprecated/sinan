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

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

-compile(export_all).

%% API
-export([discover/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc
%%  Run the discover task.
%% @end
-spec discover(ProjectDir::string(), Config::sin_build_config:build_config()) ->
    UpdatedConfig::sin_build_config:config().
discover(ProjectDir, Config) ->
    {ok, BuildDir} = dict:find("build_dir", Config),
    AppDirs = look_for_app_dirs(Config, BuildDir, ProjectDir),
    build_app_info(Config, AppDirs, []).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc
%%   Given a list of app dirs retrieves the application names.
%%
%% @end
-spec build_app_info(Config::sin_build_config:build_config(), List::list(), Acc::list()) ->
    Config::sin_build_config:build_config().
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

%% @doc
%%  Convert the details list to something that fits into the config
%%  nicely.
%% @end
-spec process_details(BaseKey::string(), List::list(), Config::dict()) ->
    NewConfig::sin_build_config:build_config().
process_details(BaseName, [{Key, Value} | T], Config) when is_atom(Key) ->
    process_details(BaseName, T, dict:store(BaseName ++ atom_to_list(Key),
                                            Value, Config));
process_details(BaseName, [{Key, Value} | T], Config) when is_list(Key)->
    process_details(BaseName, T, dict:store(BaseName ++ Key, Value, Config));
process_details(_, [], Config) ->
    Config.

%% @doc
%%  Roles through subdirectories of the build directory looking
%%  for directories that have a src and an ebin subdir. When it
%%  finds one it stops recursing and adds the directory to the list
%%  to return.
%% @end
-spec look_for_app_dirs(Config::sin_build_config:build_config(),
			BuildDir::string(), ProjectDir::string()) -> AppDirs::[string()].
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


%% @doc
%%  Process the app dir to see if it is an application directory.
%%
%% @end
-spec process_app_dir(Config::sin_build_config:build_config(),
		      BuildDir::string(), Parent::string(), Sub::string(),
		      Ignorables::[string()], Acc::list()) -> ListOfDirs::[string()].
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

%% @doc
%%  Given a directory checks of the name is src or ebin, compares
%%  against its state and returns an indicator if the parent is a
%%  app dir.
%% @end
-spec process_dirs(File::string(), FinateState::string(), src | ebin) -> src | ebin.
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
copy_from_app_test_() ->
    {setup,
     fun build_config/0,
     fun(_) -> ok end,
     fun test_project_p1/1}.



test_project_p1(Config) ->
    Results = discover(filename:join(["test_data",
				      "sin_discover",
				      "p1"]),
		       Config),
    {timeout, 2,
     [?_assertMatch(
	 ["p1"], sin_build_config:get_value(Results, "project.applist")),
      ?_assertMatch(
	 p1, sin_build_config:get_value(Results, "apps.p1.name")),
      ?_assertMatch(
	 "test_data/sin_discover/p1/ebin/p1.app",
	 sin_build_config:get_value(Results, "apps.p1.dotapp")),
      ?_assertMatch(
	 "test_data/sin_discover/p1",
	 sin_build_config:get_value(Results,
				    "apps.p1.basedir"))]}.

test_project_p2(Config) ->
    Results = discover(filename:join(["test_data",
				      "sin_discover",
				      "p2"]),
		       Config),
    {timeout, 2,
     [?_assertMatch(
	 ["p1"], sin_build_config:get_value(Results, "project.applist")),
      ?_assertMatch(
	 p1, sin_build_config:get_value(Results, "apps.p1.name")),
      ?_assertMatch(
	 "test_data/sin_discover/p1/ebin/p1.app",
	 sin_build_config:get_value(Results, "apps.p1.dotapp")),
      ?_assertMatch(
	 "test_data/sin_discover/p1",
	 sin_build_config:get_value(Results,
				    "apps.p1.basedir"))]}.


test_project_p3(Config) ->
    Results = discover(filename:join(["test_data",
				      "sin_discover",
				      "p2"]),
		       Config),
    {timeout, 2,
     [?_assertMatch(
	 ["p1"], sin_build_config:get_value(Results, "project.applist")),
      ?_assertMatch(
	 p1, sin_build_config:get_value(Results, "apps.p1.name")),
      ?_assertMatch(
	 "test_data/sin_discover/p1/ebin/p1.app",
	 sin_build_config:get_value(Results, "apps.p1.dotapp")),
      ?_assertMatch(
	 "test_data/sin_discover/p1",
	 sin_build_config:get_value(Results,
				    "apps.p1.basedir"))]}.

test_project_p4(Config) ->
    Results = discover(filename:join(["test_data",
				      "sin_discover",
				      "p2"]),
		       Config),
    {timeout, 2,
     [?_assertMatch(
	 ["p1"], sin_build_config:get_value(Results, "project.applist")),
      ?_assertMatch(
	 p1, sin_build_config:get_value(Results, "apps.p1.name")),
      ?_assertMatch(
	 "test_data/sin_discover/p1/ebin/p1.app",
	 sin_build_config:get_value(Results, "apps.p1.dotapp")),
      ?_assertMatch(
	 "test_data/sin_discover/p1",
	 sin_build_config:get_value(Results,
				    "apps.p1.basedir"))]}.




build_config() ->
    sin_build_config:store(
      sin_build_config:store(
	sin_build_config:store(
	  sin_build_config:store(
	    sin_build_config:new(),
	    "build_dir", "_build"),
	  "ignore_dirs", ["_", "."]),
	"project_name", "foo"),
      "project_version", "v1").
