%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Deletes everything in the Build directory
%%% @end
%%% @copyright (C) 2006-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_clean).

-behaviour(sin_task).

-include_lib("sinan/include/sinan.hrl").

%% API
-export([description/0, do_task/2]).

-define(TASK, clean).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================

%% @doc return a description of this task to the call
-spec description() -> sin_task:config().
description() ->

    Desc = "
clean Task
==========

This command removes *all* the build artifacts currently in the
system.

Basically this does the equivalent of:

    $> rm -rf _build
",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "clean",
          short_desc = "Removes all build artifacts in the system",
          desc = Desc,
          opts = []}.

%% @doc clean all sinan artifacts from the system
-spec do_task(sin_config:config(), sin_state:state()) -> sin_state:state().
do_task(Config, State) ->
    sin_log:verbose(Config, "cleaning build artifacts"),
    BuildDir = sin_state:get_value(build_root, State),
    sin_log:verbose(Config, "Removing directories and contents in ~s", [BuildDir]),
    sin_utils:delete_dir(BuildDir),
    State.

%%====================================================================
%%% Internal functions
%%====================================================================
