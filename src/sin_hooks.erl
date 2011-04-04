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

-include("internal.hrl").

-define(NEWLINE, 10).
-define(CARRIAGE_RETURN, 13).

%% API
-export([get_hooks_function/1,
	 format_exception/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creats a function that can be used to run build hooks in the system.
-spec get_hooks_function(ProjectRoot::string()) -> function().
get_hooks_function(ProjectRoot) ->
    HooksDir = filename:join([ProjectRoot, "_hooks"]),
    case sin_utils:file_exists(HooksDir) of
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
    fun(Type, Task, BuildConfig) ->
	    do_hook(Type, Task, BuildConfig, HooksDir)
    end.

%% @doc Setup to run the hook and run it if it exists.
-spec do_hook(Type::atom(), Task::atom(), BuildConfig::string(),
	      HooksDir::string()) -> ok.
do_hook(Type, Task, BuildConfig, HooksDir) when is_atom(Task) ->
    HookName = atom_to_list(Type) ++ "_" ++ atom_to_list(Task),
    HookPath = filename:join(HooksDir, HookName),
    case sin_utils:file_exists(HookPath) of
       true ->
	    run_hook(HookPath, BuildConfig, list_to_atom(HookName));
       _ ->
	    ok
    end.

%% @doc Setup the execution environment and run the hook.
-spec run_hook(HookPath::list(), BuildConfig::list(), HookName::atom()) -> ok.
run_hook(HookPath, BuildConfig, HookName) ->
    Env = sin_config:get_pairs(BuildConfig),
    command(HookPath, stringify(Env, []), BuildConfig, HookName).

%% @doc Take a list of key value pairs and convert them to string based key
%% value pairs.
-spec stringify(PairList::list(), Acc::list()) -> list().
stringify([{Key, Value} | Rest], Acc) ->
    stringify(Rest, [{Key, lists:flatten(sin_utils:term_to_list(Value))} | Acc]);
stringify([], Acc) ->
    Acc.

%% @doc Given a command an an environment run that command with the environment
-spec command(Command::list(), Env::list(),
	      BuildConfig::list(), HookName::atom()) -> list().
command(Cmd, Env, BuildConfig, HookName) ->
    Opt =  [{env, Env}, stream, exit_status, use_stdio,
	    stderr_to_stdout, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_data(P, BuildConfig, HookName, []).

%% @doc Event results only at newline boundries.
-spec event_newline(BuildConfig::list(), HookName::atom(),
		    Line::list(), Acc::list()) -> list().
event_newline(BuildConfig, HookName, [?NEWLINE | T], Acc) ->
    ewl_talk:say(lists:reverse(Acc)),
    event_newline(BuildConfig, HookName, T, []);
event_newline(BuildConfig, HookName, [?CARRIAGE_RETURN | T], Acc) ->
    ewl_talk:say(lists:reverse(Acc)),
    event_newline(BuildConfig, HookName, T, []);
event_newline(BuildConfig, HookName, [H | T], Acc) ->
    event_newline(BuildConfig, HookName, T, [H | Acc]);
event_newline(_BuildConfig, _HookName, [], Acc) ->
    lists:reverse(Acc).

%% @doc Recieve the data from the port and exit when complete.
-spec get_data(P::port(), BuildConfig::list(),
	       HookName::atom(), Acc::list()) -> list().
get_data(P, BuildConfig, HookName, Acc) ->
    receive
	{P, {data, D}} ->
	    NewAcc = event_newline(BuildConfig, HookName, Acc ++ D, []),
	    get_data(P, BuildConfig, HookName, NewAcc);
	{P, eof} ->
	    ewl_talk:say(Acc),
	    port_close(P),
	    receive
		{P, {exit_status, 0}} ->
		    ok;
		{P, {exit_status, N}} ->
		    ?SIN_RAISE(bad_exit_status,
			       "Hook ~s exited with status ~p",
			       [HookName, N])
	    end
    end.



