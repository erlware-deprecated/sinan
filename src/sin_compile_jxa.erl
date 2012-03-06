%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Erlware, LLC
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sin_compile_jxa).

%% API
-export([build_file/5,
         get_target/3,
         format_exception/1]).

-include_lib("sinan/include/sinan.hrl").

%%%===================================================================
%%% API
%%%===================================================================
get_target(BuildDir, File, ".jxa") ->
    sin_task_build:get_target(File, ".jxa", BuildDir, ".beam").

%% @doc Do the actual compilation on the file.
-spec build_file(sin_config:matcher(), sin_state:state(),
                 sin_file_info:mod(), [term()], string()) ->
                        {module(), sin_config:config()}.
build_file(Config, State, Module=#module{path=File}, Options, _Target) ->
    sin_log:verbose(Config, "Building ~s", [File]),
    case joxa.compiler:'do-compile'(File, Options) of
        Ctx when is_tuple(Ctx), element(1, Ctx) == context ->
            case joxa.compiler:'has-errors?'(Ctx) of
                false ->
                    {State, [Module]};
                true ->
                    ?SIN_RAISE(State, {build_error, error_building_jxa_file, File})
            end;
        error ->
            ?SIN_RAISE(State, {build_error, error_building_jxa_file, File})
    end.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).
%%%===================================================================
%%% Internal functions
%%%===================================================================
