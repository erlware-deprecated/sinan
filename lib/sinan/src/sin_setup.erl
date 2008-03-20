%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007 Erlware
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
%%%   Provides fconf setup
%%% @end
%%% @copyright (C) 2007, Erlware
%%% Created : 2007 by Eric Merritt <cyberlync@gmail.com>
%%%---------------------------------------------------------------------------
-module(sin_setup).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, setup/1]).

-define(TASK, setup).
-define(DEPS, []).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start() -> ok.
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Sets up the build config area and various other "
        "things required by the build",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = [{"sf", "build-flavor", true}]},
    eta_task:register_task(TaskDesc).



%%--------------------------------------------------------------------
%% @spec do_task(BuildRef, Args) -> ok
%%
%% @doc
%%  dO the task defined in this module.
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    setup(BuildRef).

%%-------------------------------------------------------------------
%% @spec run(Task, Args) -> ok.
%% @doc
%%   Run the specified task given the specified args. Make sure that
%%  all dependency tasks have also been run and all thier
%%  dependencies etc, etc.
%% @end
%% @private
%%-------------------------------------------------------------------
setup(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK),
    setup_build(BuildRef),
    Flavor = get_flavor(BuildRef, undefined),
    handle_build_config(Flavor, BuildRef),
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @doc
%%  Get the current build flavor or the default flavor if
%%  required.
%% @spec get_flavor(BuildRef, FlavorList) -> Flavor
%% @end
%%-------------------------------------------------------------------
get_flavor(_BuildRef, [Flavor]) ->
    Flavor;
get_flavor(BuildRef, []) ->
    fconf:get_value(BuildRef, "default_flavor", "development");
get_flavor(BuildRef, undefined) ->
    fconf:get_value(BuildRef, "default_flavor", "development").

%%-------------------------------------------------------------------
%% @spec handle_build_config(Flavor, BuildRef) -> ok.
%% @doc
%%   Look for the build config. Pass it off to the processor when
%%   its found.
%% @end
%% @private
%%-------------------------------------------------------------------
handle_build_config(Flavor, BuildRef) ->
    Start = fconf:get_value(BuildRef, "build.start_dir"),
    case find_build_config(Start) of
        no_build_config ->
            eta_event:task_fault(BuildRef, ?TASK, "No build config found!!"),
            throw(no_build_config);
        BuildConfig ->
            process_build_config(Flavor, BuildRef, BuildConfig)
    end.

%%-------------------------------------------------------------------
%% @spec process_build_config(Flavor, BuildRef, BuildConfig) -> ok.
%% @doc
%%   Read in the build config/parse and send to the config process.
%% @end
%% @private
%%-------------------------------------------------------------------
process_build_config(Flavor, BuildRef, BuildConfig) ->
    Dir = filename:dirname(BuildConfig),
    case fconf:parse_config(BuildRef, BuildConfig) of
        ok ->
            ok;
        {error, {Reason, Line, Char}} ->
            eta_event:task_fault(BuildRef, ?TASK,
                                 {"The config file seems to be miss formatted. "
                                  "Got '~s' on line ~w in column ~w",
                         [Reason, Line + 1, Char + 1]}),
            throw(config_parse_error)
    end,
    BuildRoot = fconf:get_value(BuildRef, "build_dir", "_build"),
    BuildDir = filename:join([Dir, BuildRoot, Flavor]),
    register_loggers(BuildDir),
    fconf:store(BuildRef, "build.root", BuildRoot),
    fconf:store(BuildRef, "build.flavor", Flavor),
    fconf:store(BuildRef, "build.dir", BuildDir),
    fconf:store(BuildRef, "build.config", BuildConfig),
    fconf:store(BuildRef, "project.dir", Dir).

%%--------------------------------------------------------------------
%% @doc
%%  Register the build specific logger for an 'application' build.
%%  otherwise don't do anything.
%%
%% @spec register_loggers(BuildDir) -> ok.
%% @end
%% @private
%%--------------------------------------------------------------------
register_loggers(BuildDir) ->
    case application:get_env(sinan, application) of
        {ok, true} ->
            LogFile = filename:join([BuildDir, "log", "build.log"]),
            filelib:ensure_dir(LogFile),
            %% kick off the standard sasl file logger so we can get
            %% nice build log file
            gen_event:add_handler(error_logger, sin_file_logger, [LogFile]);
        _ ->
            ok
    end.

%%-------------------------------------------------------------------
%% @spec find_build_config(Dir::string()) -> ok.
%% @doc
%%   find "_build_config" in the current directory. if not recurse
%%   with parent directory.
%% @end
%% @private
%%-------------------------------------------------------------------
find_build_config("/") ->
    no_build_config;
find_build_config(Start) ->
    ConfigFile = filename:join(Start, "_build.cfg"),
    case file:read_file_info(ConfigFile) of
        {ok, _FileInfo} ->
            ConfigFile;
        {error, _Reason} ->
            find_build_config(sin_utils:parent_dir(Start))
    end.


%%--------------------------------------------------------------------
%% @doc
%%  Setup the build.
%% @spec setup_build(BuildRef) -> ok.
%% @end
%%--------------------------------------------------------------------
setup_build(BuildRef) ->
    DefaultConfig =
        filename:join([code:priv_dir(sinan),
                       "default_build"]),
    fconf:parse_config(BuildRef,
                       DefaultConfig).
