%%%-------------------------------------------------------------------
%%% @author Eric Merritt <>
%%% @copyright (C) 2011, Eric Merritt
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2011 by Eric Merritt <>
%%%-------------------------------------------------------------------
-module(sin_compile_erl).

%% API
-export([build_file/5,
         get_target/3,
         format_exception/1]).

-include_lib("sinan/include/sinan.hrl").

%%%===================================================================
%%% API
%%%===================================================================
get_target(BuildDir, File, ".erl") ->
    sin_task_build:get_target(File, ".erl", BuildDir, ".beam").

%% @doc Do the actual compilation on the file.
-spec build_file(sin_config:matcher(), sin_state:state(),
                 sin_file_info:mod(), [term()], string()) ->
                        {module(), sin_config:config()}.
build_file(_Config, State, Module=#module{path=File}, Options, _Target) ->
    ewl_talk:say("Building ~s", [File]),
    case compile:file(File, Options) of
        {ok, _ModuleName} ->
            {State, [Module]};
        {ok, _ModuleName, []} ->
            {State, [Module]};
        {ok, _ModuleName, Warnings} ->
            {?WARN(State,
                  sin_task_build:gather_fail_info(Warnings, "warning")),
             [Module]};
        {error, Errors, Warnings} ->
            NewRef =
                ?WARN(State,
                  lists:flatten([sin_task_build:gather_fail_info(Errors,
                                                                 "error"),
                                 sin_task_build:gather_fail_info(Warnings,
                                                                 "warning")])),
            ?SIN_RAISE(NewRef, {build_error, error_building_erl_file, File});
        error ->
            ewl_talk:say("Unknown error occured during build"),
            ?SIN_RAISE(State, {build_error, error_building_erl_file, File})
    end.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).
%%%===================================================================
%%% Internal functions
%%%===================================================================
