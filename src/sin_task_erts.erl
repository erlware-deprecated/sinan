%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Return the erts version sinan is running on.
%%% @end
%%% @copyright (C) 2008-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_erts).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1]).

-define(TASK, erts).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================
%% @doc provides a description for this task
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Provides erts version sinan is running as",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = true,
	  deps = ?DEPS,
	  desc = Desc,
	  opts = []}.

%% @doc Get the version of sinan that is currently running
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(_BuildRef) ->
    ewl_talk:say("erts-~s (~s)", [erlang:system_info(version),
				 erlang:system_info(otp_release)]).


