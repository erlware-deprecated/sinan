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

-include("internal.hrl").

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
    Desc = "Removes the build area and everything underneath",
    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          example = "clean",
          short_desc = Desc,
          desc = Desc,
          opts = []}.

%% @doc clean all sinan artifacts from the system
-spec do_task(sin_config:config(), sin_state:state()) -> sin_state:state().
do_task(_Config, State) ->
    ewl_talk:say("cleaning build artifacts"),
    BuildDir = sin_state:get_value(build_root, State),
    ewl_talk:say("Removing directories and contents in ~s", [BuildDir]),
    sin_utils:delete_dir(BuildDir),
    State.

%%====================================================================
%%% Internal functions
%%====================================================================
