%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Return the sinan server version.
%%% @end
%%% @copyright (C) 2008-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_version).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1]).

-define(TASK, version).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================
%% @doc provides a description for this task
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Simple prints out the current version of sinan",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = true,
	  deps = ?DEPS,
	  example = "version",
	  short_desc = "Provides sinan version information",
	  desc = Desc,
	  opts = []}.

%% @doc Get the version of sinan that is currently running
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    Version = case get_version() of
		  unknown_version ->
		      "unknown";
		  SinVersion ->
		      SinVersion
	      end,
    ewl_talk:say("sinan version: ~s", [Version]),
    sin_config:store(BuildRef, "sinan.vsn", Version).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Gets the current version of the sinan release.
-spec get_version() -> Vsn::string() | unkown_version.
get_version() ->
    SinDir = filename:join([filename:dirname(code:priv_dir(sinan)), "ebin", "sinan.app"]),
    get_version(file:consult(SinDir)).

get_version({ok, [{application, sinan, Opts}]}) ->
    case lists:keysearch(vsn, 1, Opts) of
	{value, {vsn, Vsn}} ->
	    Vsn;
	_ ->
	    unknown_version
    end;
get_version(_) ->
    unknown_version.

