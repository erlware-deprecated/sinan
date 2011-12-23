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
    ec_talk:say("Building ~s", [File]),
    case compile:file(File, Options) of
        {ok, ModuleName} ->
            reload_module(ModuleName),
            {State, [Module]};
        {ok, ModuleName, []} ->
            reload_module(ModuleName),
            {State, [Module]};
        {ok, ModuleName, Warnings0} ->
            reload_module(ModuleName),
            Warnings1 = sin_task_build:gather_fail_info(Warnings0, "warning"),
            {?WARN(State,
                  {build_warnings, Warnings1}),
             [Module]};
        {error, Errors, Warnings} ->
            ErrorStrings =
                lists:flatten([sin_task_build:gather_fail_info(Errors,
                                                               "error"),
                               sin_task_build:gather_fail_info(Warnings,
                                                               "warning")]),
            ec_talk:say(lists:flatten(ErrorStrings)),
            ?SIN_RAISE(State, {build_error, error_building_erl_file, File});
        error ->
            ec_talk:say("Unknown error occured during build"),
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
-spec reload_module(atom()) -> {ok, atom()}.
reload_module(Module) ->
    code:purge(Module),
    code:load_file(Module).
