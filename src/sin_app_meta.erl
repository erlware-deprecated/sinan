%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%% Populate the application metadata.
%%% @end
%%% @copyright (C) 2011 Erlware, LLC.
%%%---------------------------------------------------------------------------
-module(sin_app_meta).


-include("internal.hrl").
-include_lib("sinan/include/sinan.hrl").

%% API
-export([populate/2,
         format_exception/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc do the system preparation
-spec populate(sin_config:matcher(), sin_state:state()) -> sin_state:state().
populate(_Config, State0) ->
    ProjectApps = sin_state:get_value(release_apps, State0),
    lists:foldl(fun(App, State1) ->
                        prepare_app(State1, App)
                  end, State0, ProjectApps).

-spec prepare_app(sin_state:state(), string()) ->
                         sin_config:config().
prepare_app(State0, #app{name=AppName, path=AppBuildDir}) ->

    BaseDetails = populate_modules(State0, AppName),

    DotApp = filename:join([AppBuildDir, "ebin",
                            erlang:atom_to_list(AppName) ++ ".app"]),

    ok = file:write_file(DotApp,
                    io_lib:format("~p.\n",
                                  [{application, AppName,
                                    BaseDetails}])),
    State0.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================
populate_modules(State, AppName) ->
    Details = sin_state:get_value({apps, AppName, base}, State),

    SourceModuleList = lists:map(fun({_, Module, _, _, _}) ->
                                         Module
                                 end,
                                 sin_state:get_value({apps, AppName, src_modules_detail},
                                                     State)),

    lists:reverse(
      lists:foldl(fun(Element = {vsn, _}, Acc) ->
                          [{modules, SourceModuleList}, Element | Acc];
                     (Element, Acc) ->
                          [Element | Acc]
                  end, [], lists:keydelete(modules, 1, Details))).
