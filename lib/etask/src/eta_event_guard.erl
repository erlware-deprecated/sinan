%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Erlware
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   Provides a guard for the printer handler of the
%%%  eta_event gen_event system.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created : 23 Nov 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(eta_event_guard).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (HandlerModule, Options) -> {ok,Pid} | ignore | {error,Error}
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(HandlerModule, Options) ->
    gen_server:start_link({local, gen_service_name(HandlerModule)},
                          ?MODULE, [HandlerModule, Options], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec (Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%%
%% @doc
%% Initiates the server
%% @end
%%--------------------------------------------------------------------
init([HandlerModule, Options]) ->
    %% Call the new event handler's startup procedure
    case catch HandlerModule:start(Options) of
        ok ->
            {ok, HandlerModule};
        already_started ->
            {stop, {already_started, HandlerModule}};
        Error ->
            {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%%
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec (Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%%
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec (Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%%
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info({gen_event_EXIT, HandlerModule, Reason}, HandlerModule) ->
    %% gen_event manager sends this message if a handler was added using
    %% gen_event:add_sup_handler/3 or gen_event:swap_sup_handler/3 functions
    io:format("~w: detected handler ~p shutdown:~n~p~n",
              [?MODULE, HandlerModule, Reason]),
    {stop, {handler_died, HandlerModule, Reason}, HandlerModule};
handle_info(Other, HandlerModule) ->
    %% This process should not receive other messages
    io:format("~w: received unknown message:~n~p~n", [?MODULE, Other]),
    {noreply, HandlerModule}.


%%--------------------------------------------------------------------
%% @spec (Reason, State) -> void()
%%
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%%
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Generate the name for this service.
%% @spec gen_service_name(ModuleName) -> NewModuleName
%% @private
%% @end
%%--------------------------------------------------------------------
gen_service_name(ModuleName) ->
    list_to_atom("event_guard_" ++ atom_to_list(ModuleName)).
