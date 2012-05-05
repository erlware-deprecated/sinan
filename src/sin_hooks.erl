%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2009-2011 Eric Merritt
%%% @doc
%%%  Provides a means of correctly creating eta pre/post task hooks
%%%  and executing those hooks that exist.
%%% @end
%%%-------------------------------------------------------------------
-module(sin_hooks).

-include_lib("sinan/include/sinan.hrl").

-define(NEWLINE, 10).
-define(CARRIAGE_RETURN, 13).

%% API
-export([get_hooks_function/2,
         format_exception/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creats a function that can be used to run build hooks in the system.
-spec get_hooks_function(sin_state:state(),
                         ProjectRoot::string()) -> function().
get_hooks_function(State, ProjectRoot) ->
    HooksDir = filename:join([ProjectRoot, "_hooks"]),
    case sin_utils:file_exists(State, HooksDir) of
       false ->
            no_hooks;
       true ->
            gen_build_hooks_function(HooksDir)
    end.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Generate a function that can be run pre and post task
-spec gen_build_hooks_function(HooksDir::string()) -> function().
gen_build_hooks_function(HooksDir) ->
    fun(Type, Task, Config, State) ->
            do_hook(Config, State, Type, Task, HooksDir)
    end.

%% @doc Setup to run the hook and run it if it exists.
-spec do_hook(sin_config:config(), sin_state:state(),
              Type::atom(), Task::atom(),
              HooksDir::string()) -> ok.
do_hook(Config, State, Type, Task, HooksDir) when is_atom(Task) ->
    HookName = atom_to_list(Type) ++ "_" ++ atom_to_list(Task),
    HookPath = filename:join(HooksDir, HookName),
    case sin_utils:file_exists(State, HookPath) of
       true ->
            sin_log:verbose(Config, "hook: ~s", [HookName]),
            run_hook(Config, State, HookPath, list_to_atom(HookName));
       _ ->
            State
    end.

%% @doc Setup the execution environment and run the hook.
-spec run_hook(sin_config:config(), sin_state:state(),
               HookPath::list(), HookName::atom()) -> ok.
run_hook(Config, State, HookPath, HookName) ->
    command(Config, State, HookPath, create_env(State), HookName).

%% @doc create a minimal env for the hook from the state.
-spec create_env(sin_state:state()) -> Env::[{string(), string()}].
create_env(State) ->
    Env =
        [{"SIN_RELEASE",
          erlang:atom_to_list(sin_state:get_value(release, State))},
         {"SIN_RELEASE_VSN", sin_state:get_value(release_vsn, State)},
         {"SIN_BUILD_ROOT", sin_state:get_value(build_root, State)},
         {"SIN_BUILD_DIR", sin_state:get_value(build_dir, State)},
         {"SIN_APPS_DIR", sin_state:get_value(apps_dir, State)},
         {"SIN_RELEASE_DIR", sin_state:get_value(release_dir, State)},
         {"SIN_HOME_DIR", sin_state:get_value(home_dir, State)},
         {"SIN_PROJECT_DIR", sin_state:get_value(project_dir, State)}] ++
        [[{"SIN_" ++ erlang:atom_to_list(Name) ++
               "_VSN", Vsn},
          {"SIN_" ++ erlang:atom_to_list(Name) ++
               "_DIR", AppDir}] ||
            #app{name=Name, vsn=Vsn, path=AppDir}
                <- sin_state:get_value(project_apps, [], State)],
    lists:flatten(Env).

%% @doc Given a command an an environment run that command with the environment
-spec command(sin_config:config(), sin_state:state(), Command::list(), Env::list(),
              HookName::atom()) -> list().
command(Config, State, Cmd, Env, HookName) ->
    Opt =  [{env, Env}, stream, exit_status, use_stdio,
            stderr_to_stdout, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_data(Config, State, P, HookName, []).

%% @doc Event results only at newline boundaries.
-spec event_newline(sin_config:config(), BuildState::list(), HookName::atom(),
                    Line::list(), Acc::list()) -> ok.
event_newline(Config, BuildState, HookName, [?NEWLINE | T], Acc) ->
    sin_log:verbose(Config, lists:reverse(Acc)),
    event_newline(Config, BuildState, HookName, T, []);
event_newline(Config, BuildState, HookName, [?CARRIAGE_RETURN | T], Acc) ->
    sin_log:verbose(Config, lists:reverse(Acc)),
    event_newline(Config, BuildState, HookName, T, []);
event_newline(Config, BuildState, HookName, [H | T], Acc) ->
    event_newline(Config, BuildState, HookName, T, [H | Acc]);
event_newline(_Config, _BuildState, _HookName, [], Acc) ->
    lists:reverse(Acc).

%% @doc Recieve the data from the port and exit when complete.
-spec get_data(sin_config:config(), sin_state:state(), P::port(),
               HookName::atom(), Acc::list()) -> sin_state:state().
get_data(Config, State, P, HookName, Acc) ->
    receive
        {P, {data, D}} ->
            NewAcc = event_newline(Config, State, HookName, Acc ++ D, []),
            get_data(Config, State, P, HookName, NewAcc);
        {P, eof} ->
            sin_log:verbose(Config, Acc),
            port_close(P),
            receive
                {P, {exit_status, 0}} ->
                    State;
                {P, {exit_status, N}} ->
                    ?SIN_RAISE(State, bad_exit_status,
                               "Hook ~s exited with status ~p",
                               [HookName, N])
            end
    end.
