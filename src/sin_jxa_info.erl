%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @copyright Erlware, LLC
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @end
%%%-------------------------------------------------------------------
-module(sin_jxa_info).

%% API
-export([process_file/3, format_exception/1]).

-include_lib("sinan/include/sinan.hrl").

%%====================================================================
%% API
%%====================================================================
-spec process_file(sin_state:state(), string(), [string()]) ->
                          sinan:mod().
process_file(State0, Path0, _Includes) ->
    case joxa.compiler:info(Path0, []) of
        {ModName, Deps} when is_list(Deps) ->
            Mod0 = sin_file_info:initialize(Path0, jxa),
            Mod1 = lists:foldl(fun(DepModule,
                                   Mod1=#module{module_deps=Modules}) ->
                                       Mod1#module{module_deps=sets:add_element(DepModule, Modules)}
                               end,
                               Mod0#module{name=ModName},
                               Deps),
            {State0,  Mod1, erlang:phash2(file:read_file(Path0))};
        Error ->
            ?SIN_RAISE(State0, {unable_to_parse_file, Path0, Error})
    end.

format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

