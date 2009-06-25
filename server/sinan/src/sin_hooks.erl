%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @copyright (C) 2009, Eric Merritt
%%% @doc
%%%  Provides a means of correctly creating eta pre/post task hooks
%%%  and executing those hooks that exist.
%%% @end
%%% Created : 30 May 2009 by Eric Merritt <cyberlync@gmail.com>
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
    fun(Type, Task, RunId) ->
	    do_hook(Type, Task, RunId, HooksDir)
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Setup to run the hook and run it if it exists.
%% @spec (Type::atom(), Task::atom(), RunId::string(),
%%        HooksDir::string()) -> ok
%% @end
%%--------------------------------------------------------------------
do_hook(Type, Task, RunId, HooksDir) when is_atom(Task) ->
    HookName = atom_to_list(Type) ++ "_" ++ atom_to_list(Task),
    HookPath = filename:join(HooksDir, HookName),
    case sin_utils:file_exists(HookPath) of
       true ->
	    run_hook(HookPath, RunId, list_to_atom(HookName));
       _ ->
	    ok
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Setup the execution environment and run the hook.
%% @spec (HookPath::list(), RunId::list(), HookName::atom()) -> ok
%% @end
%%--------------------------------------------------------------------
run_hook(HookPath, RunId, HookName) ->
    Env = sin_build_config:get_pairs(RunId),
    command(HookPath, stringify(Env, []), RunId, HookName).

%%--------------------------------------------------------------------
%% @doc
%%  Take a list of key value pairs and convert them to string
%%  based key value pairs.
%% @spec (PairList::list(), Acc::list()) -> list()
%% @end
%%--------------------------------------------------------------------
stringify([{Key, Value} | Rest], Acc) ->
    stringify(Rest, [{Key, lists:flatten(sin_utils:term_to_list(Value))} | Acc]);
stringify([], Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @doc
%%  Given a command an an environment run that command with the environment
%% @spec (Command::list(), Env::list(), RunId::list(), HookName::atom()) -> list()
%% @end
%%--------------------------------------------------------------------
command(Cmd, Env, RunId, HookName) ->
    Opt =  [{env, Env}, stream, exit_status, use_stdio,
	    stderr_to_stdout, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_data(P, RunId, HookName, []).

%%--------------------------------------------------------------------
%% @doc
%%  Event results only at newline boundries.
%% @spec (RunId::list(), HookName::atom(), Line::list(), Acc::list()) -> list()
%% @end
%%--------------------------------------------------------------------
event_newline(RunId, HookName, [?NEWLINE | T], Acc) ->
    eta_event:task_event(RunId, HookName, info, {"~s", [lists:reverse(Acc)]}),
    event_newline(RunId, HookName, T, []);
event_newline(RunId, HookName, [?CARRIAGE_RETURN | T], Acc) ->
    eta_event:task_event(RunId, HookName, info, {"~s", [lists:reverse(Acc)]}),
    event_newline(RunId, HookName, T, []);
event_newline(RunId, HookName, [H | T], Acc) ->
    event_newline(RunId, HookName, T, [H | Acc]);
event_newline(_RunId, _HookName, [], Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @doc
%%  Recieve the data from the port and exit when complete.
%% @spec (P::port(), RunId::list(), HookName::atom(), Acc::list()) -> list()
%% @end
%%--------------------------------------------------------------------
get_data(P, RunId, HookName, Acc) ->
    receive
	{P, {data, D}} ->
	    NewAcc = event_newline(RunId, HookName, Acc ++ D, []),
	    get_data(P, RunId, HookName, NewAcc);
	{P, eof} ->
	    eta_event:task_event(RunId, HookName, info, {"~s", [Acc]}),
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



