%%%-------------------------------------------------------------------
%%% @author Eric Merritt <>
%%% @copyright (C) 2011, Eric Merritt
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2011 by Eric Merritt <>
%%%-------------------------------------------------------------------
-module(sin_compile_asn1).

%% API
-export([get_dependencies/2,
	 get_target/3,
	 build_file/4]).

-include("internal.hrl").

%%%===================================================================
%%% API
%%%===================================================================
get_target(BuildDir, File, Ext) ->
    sin_task_build:get_target(File, Ext, BuildDir, ".beam").

get_dependencies(_File, _Includes) ->
    [].

build_file(BuildRef, File, Options, Target) ->
    ErlFile = sin_utils:basename(File),
    AppDir = filename:dirname(Target),
    ErlTarget = filename:join([AppDir, "src"]),
    ErlName = filename:join([ErlTarget,
			     lists:flatten([ErlFile, ".erl"])]),
    ewl_talk:say("Building ~s", [File]),
    case asn1ct:compile(File, [{outdir, ErlTarget}, noobj] ++
			sin_task_build:strip_options(Options)) of
	ok ->
	    sin_compile_erl:build_file(BuildRef, ErlName,
				       Options, Target);
	{error, Errors} ->
	    ewl_talk:say(sin_task_build:gather_fail_info(Errors, "error")),
	    ?SIN_RAISE({build_error, error_building_asn1, File})
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
