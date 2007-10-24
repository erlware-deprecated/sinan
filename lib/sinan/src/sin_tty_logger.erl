%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
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
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%   Custom error logger for the build system.
%%% @end
%%% @copyright (C) 2007, Eric Merritt
%%% Created : 14 Mar 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_tty_logger).

-behaviour(gen_event).


%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).


%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}.
%% 
%% @doc 
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec  handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler.
%% 
%% @doc 
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%% @end 
%%--------------------------------------------------------------------
handle_event({_, _, {_, Msg, Data}}, State) when is_list(Msg) ->
    io:format(Msg, Data),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}.
%% 
%% @doc 
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%% @end 
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler.
%% 
%% @doc 
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void().
%% 
%% @doc 
%% Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}. 
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
