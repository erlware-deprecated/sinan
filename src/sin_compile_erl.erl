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
build_file(Config, State, Module=#module{path=File}, Options, _Target) ->
    sin_log:verbose(Config, "Building ~s", [File]),
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
            sin_log:normal(Config, lists:flatten(ErrorStrings)),
            ?SIN_RAISE(State, {build_error, error_building_erl_file, File});
        error ->
            sin_log:normal(Config, "Unknown error occured during build"),
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
%% We don't want to do this on ourselves
-spec reload_module(atom()) -> {ok, atom()}.
reload_module(Module)
  when not Module == ?MODULE ->
    code:purge(Module),
    code:load_file(Module),
    code:purge(Module),
    code:load_file(Module);
reload_module(_) ->
    ok.

