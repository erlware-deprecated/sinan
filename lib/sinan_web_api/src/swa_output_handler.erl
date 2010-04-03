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
%%% This handles output of the eventing system. Used to write to the
%%% socket.
%%% @end
%%% Created : 20 Apr 2008 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(swa_output_handler).

-behaviour(gen_server).

%% API
-export([start_link/2, send_event/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {build_ref, req, writer}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec (BuildRef, Req) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(BuildRef, Req) ->
    gen_server:start_link(?MODULE, [BuildRef, Req], []).

%%--------------------------------------------------------------------
%% @doc
%%  Send an event to the output_handler specified by pid
%% @spec (Pid::pid(), Event) -> ok
%% @end
%%--------------------------------------------------------------------
send_event(Pid, Event) ->
    gen_server:cast(Pid, {event, Event}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec (Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([BuildRef, Req]) ->
    Writer = crary_body:new_writer(Req),
    crary:r(Writer, 200,
            [{"content-type", "application/json"},
             {"transfer-encoding", "chunked"}]),
    sinan:add_build_event_handler(swa_event_handler, [BuildRef, self()]),
    {ok, #state{build_ref = BuildRef, req = Req, writer = Writer}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec (Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec (Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({event, Event}, State = #state{writer = Writer}) ->
    case Event of
        {run_event, _, Type} when Type == stop orelse Type == fault ->
            send_event_to_sock(Event, Writer),
            {stop, normal, State};
        {run_event, _, Type, _} when Type == stop orelse Type == fault ->
            send_event_to_sock(Event, Writer),
            {stop, normal, State};
        _ ->
            send_event_to_sock(Event, Writer),
            {noreply, State}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec (Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec (Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{writer = Writer}) ->
    quit(Writer),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
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
send_event_to_sock({Type, _, EventType}, Writer) ->
    JDesc = ktj_encode:encode({obj, [{type, Type},
                                     {event_type, EventType}]}),
    crary_body:write(Writer, JDesc);
send_event_to_sock({Type, _, Task, EventType}, Writer) when is_atom(EventType)->
    JDesc = {obj, [{task, Task},
                  {type, Type},
                  {event_type, EventType}]},
    crary_body:write(Writer, ktj_encode:encode(JDesc));
send_event_to_sock({Type, _, EventType, Desc}, Writer) ->
    JDesc = {obj, [{type, Type},
                  {event_type, EventType},
                  {desc, conv_desc(Desc)}]},
    crary_body:write(Writer, ktj_encode:encode(JDesc));
send_event_to_sock({Type, _, Task, EventType, Desc}, Writer) ->
    JDesc = ktj_encode:encode({obj, [{task, Task},
                                     {type, Type},
                                     {event_type, EventType},
                                     {desc, conv_desc(Desc)}]}),
    crary_body:write(Writer, JDesc).

conv_desc({Format, Args}) ->
    list_to_binary(io_lib:format(Format, Args));
conv_desc(Desc) when is_list(Desc) ->
    list_to_binary(Desc);
conv_desc(Desc) when is_binary(Desc) ->
    Desc.

%%--------------------------------------------------------------------
%% @doc
%%  do whatever clean up is required to quit the system.
%% @spec (Req) -> ok
%% @end
%%--------------------------------------------------------------------
quit(Writer) ->
    crary_body:done_writing(Writer),
    crary_sock:done_writing(Writer).

