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
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2009-2010 Erlware
%%% @doc
%%%  Provides a means of correctly creating eta pre/post task hooks
%%%  and executing those hooks that exist.
%%% @end
%%% Created : 30 May 2009 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_hooks).

-include("etask.hrl").

-define(NEWLINE, 10).
-define(CARRIAGE_RETURN, 13).

%% API
-export([get_hooks_function/1]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Creats a function that can be used to run build hooks in the system.
%% @spec (ProjectRoot::string()) -> function()
%% @end
%%--------------------------------------------------------------------
get_hooks_function(ProjectRoot) ->
    HooksDir = filename:join([ProjectRoot, "_hooks"]),
    case sin_utils:file_exists(HooksDir) of
       false ->
	    none;
       true ->
	    gen_build_hooks_function(HooksDir)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Generate a function that can be run pre and post task
%% @spec (HooksDir::string()) -> function()
%% @end
%%--------------------------------------------------------------------
gen_build_hooks_function(HooksDir) ->
    fun(Type, Task, BuildRef) ->
	    do_hook(Type, Task, BuildRef, HooksDir)
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Setup to run the hook and run it if it exists.
%% @spec (Type::atom(), Task::atom(), BuildRef::string(),
%%        HooksDir::string()) -> ok
%% @end
%%--------------------------------------------------------------------
do_hook(Type, Task, BuildRef, HooksDir) when is_atom(Task) ->
    HookName = atom_to_list(Type) ++ "_" ++ atom_to_list(Task),
    HookPath = filename:join(HooksDir, HookName),
    case sin_utils:file_exists(HookPath) of
       true ->
	    run_hook(HookPath, BuildRef, list_to_atom(HookName));
       _ ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Setup the execution environment and run the hook.
%% @spec (HookPath::list(), BuildRef::list(), HookName::atom()) -> ok
%% @end
%%--------------------------------------------------------------------
run_hook(HookPath, BuildRef, HookName) ->
    Env =  generate_env(BuildRef),
    command(HookPath, Env, BuildRef, HookName).


%%--------------------------------------------------------------------
%% @doc
%%  Generates all of the required environmental variables from the
%% build config.
%% @spec (BuildRef::list()) -> Env::list()
%% @end
%%--------------------------------------------------------------------
generate_env(BuildRef) ->
    ConvFun = fun(Arg) when is_list(Arg) ->
		      Arg;
		 (Arg) when is_atom(Arg) ->
		      atom_to_list(Arg)
	      end,
    lists:flatten([{"PREFIX", ConvFun(sin_utils:get_application_env(prefix))},
		   {"ERTS_VERSION",
		    ConvFun(sin_utils:get_application_env(erts_version))},
		   {"BUILD_FLAVOR",
		    sin_build_config:get_value(BuildRef, "build.flavor")},
		   {"BUILD_DIR", sin_build_config:get_value(BuildRef, "build.dir")},
		   {"BUILD_REF", BuildRef},
		   {"PROJECT_DIR",
		    sin_build_config:get_value(BuildRef, "project.dir")},
		   {"PROJECT_VSN",
		    sin_build_config:get_value(BuildRef, "project.vsn")},
		   {"PROJECT_NAME",
		    sin_build_config:get_value(BuildRef, "project.name")},
		   generate_apps_env(BuildRef)]).

%%--------------------------------------------------------------------
%% @doc
%%  Generate the app specific environment variables, for all
%%  individual apps related to the project
%% @spec (BuildRef::list()) -> Env::list()
%% @end
%%--------------------------------------------------------------------
generate_apps_env(BuildRef) ->
    ProjectApps = sin_build_config:get_value(BuildRef, "project.apps"),
    ProjectDeps = sin_build_config:get_value(BuildRef, "project.deps"),
    [generate_app_list_env("PROJECT_APPS", ProjectApps),
     generate_app_list_env("PROJECT_DEPS", ProjectDeps)].


%%--------------------------------------------------------------------
%% @doc
%%  Generate the env varables for a specific app list
%% build config.
%% @spec (Name::list(), AppList::list()) -> Env::list()
%% @end
%%--------------------------------------------------------------------
generate_app_list_env(Name, AppList) ->
    [{Name, lists:flatten(lists:foldl(
			    fun({AppName, _, _, _}, Acc) when is_atom(AppName) ->
				    [atom_to_list(AppName), ",",
				     Acc];
			       ({AppName, _, _, _}, Acc) when is_list(AppName) ->
				    [AppName, ",", Acc]
			    end,
			    "",
			    AppList))},
     lists:foldl(fun generate_app_env/2, [], AppList)].


%%--------------------------------------------------------------------
%% @doc
%%  Generate the env varables for a specific application
%% build config.
%% @spec (App::tuple()) -> Env::list()
%% @end
%%--------------------------------------------------------------------
generate_app_env({AppName, AppVSN, Deps, Location}, Acc) when is_atom(AppName) ->
    generate_app_env(atom_to_list(AppName), AppVSN, Deps, Location, Acc);
generate_app_env({AppName, AppVSN, Deps, Location}, Acc) when is_list(AppName) ->
    generate_app_env(AppName, AppVSN, Deps, Location, Acc).

%%--------------------------------------------------------------------
%% @doc
%%  Generate the env varables for a specific application
%% build config.
%% @spec (AppName::list(), AppVSN::list(), Deps::list(), Location::list())
%% -> Env::list()
%% @end
%%--------------------------------------------------------------------
generate_app_env(AppName, AppVSN, Deps, Location, Acc) ->
    UpperName = string:to_upper(AppName),
    [{UpperName ++ "_VSN", AppVSN},
     {UpperName ++ "_LOCATION", Location},
     {UpperName ++ "_DEPS", lists:flatten(lists:foldl(fun(Dep, LAcc) ->
							      [atom_to_list(Dep),
							       ",",
							       LAcc]
						      end,
						      "",
						      Deps))},
     Acc].
%%--------------------------------------------------------------------
%% @doc
%%  Given a command an an environment run that command with the environment
%% @spec (Command::list(), Env::list(), BuildRef::list(), HookName::atom()) -> list()
%% @end
%%--------------------------------------------------------------------
command(Cmd, Env, BuildRef, HookName) ->
    Opt =  [{env, Env}, stream, exit_status, use_stdio,
	    stderr_to_stdout, in, eof,
	    {cd, sin_build_config:get_value(BuildRef, "project.dir")}],
    P = open_port({spawn, Cmd}, Opt),
    get_data(P, BuildRef, HookName, []).

%%--------------------------------------------------------------------
%% @doc
%%  Event results only at newline boundries.
%% @spec (BuildRef::list(), HookName::atom(), Line::list(), Acc::list()) -> list()
%% @end
%%--------------------------------------------------------------------
event_newline(BuildRef, HookName, [?NEWLINE | T], Acc) ->
    eta_event:task_event(BuildRef, HookName, info, {"~s", [lists:reverse(Acc)]}),
    event_newline(BuildRef, HookName, T, []);
event_newline(BuildRef, HookName, [?CARRIAGE_RETURN | T], Acc) ->
    eta_event:task_event(BuildRef, HookName, info, {"~s", [lists:reverse(Acc)]}),
    event_newline(BuildRef, HookName, T, []);
event_newline(BuildRef, HookName, [H | T], Acc) ->
    event_newline(BuildRef, HookName, T, [H | Acc]);
event_newline(_BuildRef, _HookName, [], Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @doc
%%  Recieve the data from the port and exit when complete.
%% @spec (P::port(), BuildRef::list(), HookName::atom(), Acc::list()) -> list()
%% @end
%%--------------------------------------------------------------------
get_data(P, BuildRef, HookName, Acc) ->
    receive
	{P, {data, D}} ->
	    NewAcc = event_newline(BuildRef, HookName, Acc ++ D, []),
	    get_data(P, BuildRef, HookName, NewAcc);
	{P, eof} ->
	    eta_event:task_event(BuildRef, HookName, info, {"~s", [Acc]}),
	    port_close(P),
	    receive
		{P, {exit_status, 0}} ->
		    ok;
		{P, {exit_status, N}} ->
		    ?ETA_RAISE_DA(bad_exit_status,
				  "Hook ~s exited with status ~p",
				  [HookName, N])
	    end
    end.



