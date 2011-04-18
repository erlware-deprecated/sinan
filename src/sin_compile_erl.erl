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
-export([get_dependencies/2,
	 build_file/4,
	 get_target/3,
	 format_exception/1]).

-include("internal.hrl").

%%%===================================================================
%%% API
%%%===================================================================
get_target(BuildDir, File, ".erl") ->
    sin_task_build:get_target(File, ".erl", BuildDir, ".beam").


get_dependencies(File, Includes) ->
    {ok, Forms} = epp:parse_file(File, Includes,[]),
    lists:foldl(fun({attribute, _, file, {Include, _}}, Acc) ->
			[{file, Include} | Acc];
		   ({attribute, _, compile,
		     {parse_transform, DepModule}}, Acc) ->
			[{module, DepModule} | Acc];
		   ({attribute, _, behaviour, DepModule}, Acc) ->
			[{module, DepModule} | Acc];
		   (_, Acc) ->
			Acc
		end, Forms).

%% @doc Do the actual compilation on the file.
-spec build_file(sin_config:config(), string(), [term()], string()) ->
    [module()].
build_file(_BuildRef, File, Options, _Target) ->
    ewl_talk:say("Building ~s", [File]),
    case compile:file(File, Options) of
	{ok, ModuleName} ->
	    ModuleName;
	{ok, ModuleName, []} ->
	    ModuleName;
	{ok, ModuleName, Warnings} ->
	    ewl_talk:say(sin_task_build:gather_fail_info(Warnings, "warning")),
	    ModuleName;
	{error, Errors, Warnings} ->
	    ewl_talk:say(
	      lists:flatten([sin_task_build:gather_fail_info(Errors, "error"),
			     sin_task_build:gather_fail_info(Warnings,
							     "warning")])),
	    ?SIN_RAISE({build_error, error_building_erl_file, File});
	error ->
	    ewl_talk:say("Unknown error occured during build"),
	    ?SIN_RAISE({build_error, error_building_erl_file, File})
    end.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).
%%%===================================================================
%%% Internal functions
%%%===================================================================
