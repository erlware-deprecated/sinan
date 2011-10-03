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
-export([build_file/5,
         get_target/3,
         format_exception/1]).

-include_lib("sinan/include/sinan.hrl").

%%%===================================================================
%%% API
%%%===================================================================
get_target(BuildDir, File, ".yrl") ->
    sin_task_build:get_target(File, ".yrl", BuildDir, ".beam").

build_file(Config, State0, Module=#module{path=File}, Options, Target) ->
    ErlFile = filename:basename(File, ".yrl"),
    AppDir = filename:dirname(Target),
    ErlTarget = filename:join([AppDir,"src"]),
    ErlName = filename:join([ErlTarget,
                             lists:flatten([ErlFile, ".erl"])]),
    ec_talk:say("Building ~s", [File]),
    {State2, ErlModule1} =
        case yecc:file(File, [{parserfile, ErlName} |
                              sin_task_build:strip_options(Options)]) of
            {ok, _ModuleName} ->
                {State1, ErlModule0} = sin_file_info:process_file(State0, File, []),
                sin_compile_erl:build_file(Config, State1, ErlModule0, Options, Target);
            {ok, _ModuleName, []} ->
                {State1, ErlModule0} = sin_file_info:process_file(State0, File, []),
                sin_compile_erl:build_file(Config, State1, ErlModule0, Options, Target);
            {ok, _ModuleName, Warnings} ->
                NewRef =
                    ?WARN(State0,
                          sin_task_build:gather_fail_info(Warnings, "warning")),
                ?SIN_RAISE(NewRef, {build_error, error_building_yecc, File});
            {error, Errors, Warnings} ->
                NewRef =
                    ?WARN(State0,
                          lists:flatten([sin_task_build:gather_fail_info(Errors,
                                                                         "error"),
                                         sin_task_build:gather_fail_info(Warnings,
                                                                         "warning")])),
                ?SIN_RAISE(NewRef, {build_error, error_building_yecc, File})
        end,
    {State2, [ErlModule1, Module]}.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).
%%%===================================================================
%%% Internal functions
%%%===================================================================
