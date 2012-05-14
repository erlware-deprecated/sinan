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

-include_lib("sinan/include/sinan.hrl").

%% API
-export([description/0, do_task/2]).

-define(TASK, erts).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================
%% @doc provides a description for this task
-spec description() -> sin_task:task_description().
description() ->

    Desc = "
erts Task
=========

A very simple command that prints out the current version of the
Erlang Runtime System that Sinan is running on",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = true,
          deps = ?DEPS,
          example = "erts",
          short_desc = "Display the erts version sinan is running on",
          desc = Desc,
          opts = []}.

%% @doc Get the version of sinan that is currently running
-spec do_task(sin_config:config(), sin_state:state()) -> sin_state:state().
do_task(Config, State) ->
    sin_log:normal(Config, "erts-~s (~s)", [erlang:system_info(version),
                                            erlang:system_info(otp_release)]),
    State.
