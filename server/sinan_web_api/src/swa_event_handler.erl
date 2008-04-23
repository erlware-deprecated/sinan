%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007 Erlware
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
%%% @copyright (C) 2008, Eric Merritt
%%% @doc
%%%  Handle sinan events by converting them to json and sending them to
%%%  the client.
%%% @end
%%% Created : 14 Apr 2008 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(swa_event_handler).

-behaviour(gen_event).

%% Include files

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {req, buildid}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(CraryRequest) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([CraryRequest, BuildId]) ->
    crary:r(CraryRequest, 200,
            [{"content-type", "application/json"},
             {"transfer-encoding", "chunked"}]),
    {ok, #state{req=CraryRequest, buildid=BuildId}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event(Event, State = #state{req = Req, buildid = BuildId}) ->
    case Event of
        {run_event, BuildId, Type} when Type == stop orelse Type == fault ->
            send_event(Event, Req),
            exit(normal);
        {_, BuildId, _} ->
            send_event(Event, Req);
        {_, BuildId, _, _} ->
            send_event(Event, Req);
        {_, BuildId, _, _, _} ->
            send_event(Event, Req);
        _ ->
            ok
    end,
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{req = Req}) ->
      quit(Req).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Actually sends the event to the crary system.
%% @spec (Event, Req) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
send_event({Type, _, EventType}, Req) ->
    JDesc = ktj_encode:encode({obj, [{type, Type},
                                     {event_type, EventType}]}),
    crary_body:write(Req, JDesc);
send_event({Type, _, Task, EventType}, Req) when is_atom(EventType)->
    JDesc = {obj, [{task, Task},
                  {type, Type},
                  {event_type, EventType}]},
    crary_body:write(Req, ktj_encode:encode(JDesc));
send_event({Type, _, EventType, Desc}, Req) ->
    JDesc = {obj, [{type, Type},
                  {event_type, EventType},
                  {desc, Desc}]},
    crary_body:write(Req, ktj_encode:encode(JDesc));
send_event({Type, _, Task, EventType, Desc}, Req) ->
    JDesc = ktj_encode:encode({obj, [{task, Task},
                                     {type, Type},
                                     {event_type, EventType},
                                     {desc, Desc}]}),
    crary_body:write(Req, JDesc).

%%--------------------------------------------------------------------
%% @doc
%%  do whatever clean up is required to quit the system.
%% @spec (Req) -> ok
%% @end
%%--------------------------------------------------------------------
quit(Req) ->
    crary_sock:done_writing(Req).


