%%%-------------------------------------------------------------------
%%% @author Eric Merritt <>
%%% @copyright (C) 2011, Eric Merritt
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2011 by Eric Merritt <>
%%%-------------------------------------------------------------------
-module(sin_compile_yrl).

%% API
-export([get_dependencies/2,
         build_file/4,
         get_target/3,
         format_exception/1]).

-include("internal.hrl").

%%%===================================================================
%%% API
%%%===================================================================
get_target(BuildDir, File, ".yrl") ->
    sin_task_build:get_target(File, ".yrl", BuildDir, ".beam").

get_dependencies(_File, _Includes) ->
    [].

build_file(BuildRef, File, Options, Target) ->
    ErlFile = filename:basename(File, ".yrl"),
    AppDir = filename:dirname(Target),
    ErlTarget = filename:join([AppDir,"src"]),
    ErlName = filename:join([ErlTarget,
                             lists:flatten([ErlFile, ".erl"])]),
    ewl_talk:say("Building ~s", [File]),
    case yecc:file(File, [{parserfile, ErlName} |
                          sin_task_build:strip_options(Options)]) of
        {ok, _ModuleName} ->
            sin_compile_erl:build_file(BuildRef, ErlName, Options, Target);
        {ok, _ModuleName, []} ->
            sin_compile_erl:build_file(BuildRef, ErlName, Options, Target);
        {ok, _ModuleName, Warnings} ->
            NewRef =
                ?WARN(BuildRef,
                      sin_task_build:gather_fail_info(Warnings, "warning")),
            ?SIN_RAISE(NewRef, {build_error, error_building_yecc, File});
        {error, Errors, Warnings} ->
            NewRef =
                ?WARN(BuildRef,
                      lists:flatten([sin_task_build:gather_fail_info(Errors,
                                                                     "error"),
                                     sin_task_build:gather_fail_info(Warnings,
                                                                     "warning")])),
            ?SIN_RAISE(NewRef, {build_error, error_building_yecc, File})
    end.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).
%%%===================================================================
%%% Internal functions
%%%===================================================================
