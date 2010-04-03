%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2010 Erlware
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
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2008-2010 Erlware
%%% @doc
%%%   This provides a mechenism to convert io for a particular
%%%   set of (sub) processes into eta_events for a specific build ref.
%%% @end
%%% Created : 25 Mar 2008 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_group_leader).

-behaviour(gen_server).

%% API
-export([start_link/3, shutdown/1, capture_start/2, capture_stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {build_id, task, type}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(BuildId, Task, Type) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(BuildId, Task, Type) ->
    gen_server:start_link(?MODULE, [BuildId,
                                    Task,
                                    Type], []).


%%--------------------------------------------------------------------
%% @doc
%%  Shut down the group_leader.
%% @spec (Pid) -> ok
%% @end
%%--------------------------------------------------------------------
shutdown(Pid) ->
    gen_server:cast(Pid, shutdown).

%%--------------------------------------------------------------------
%% @doc
%% Starts capturing all output from io:format, and similar. Capturing
%% output doesn't stop output from happening. It just makes it possible
%% to retrieve the output using capture_get/0.
%% Starting and stopping capture doesn't affect already captured output.
%% All output is stored as messages in the message queue until retrieved
%%
%% @spec (BuildRef, Task) -> {OldGroupLeader, NewGroupLeader}
%% @end
%%--------------------------------------------------------------------
capture_start(BuildRef, Task) ->
    OldGL = group_leader(),
    NewGL = case sin_group_leader_sup:start_group_leader(BuildRef,
                                                         Task, io) of
        {ok, Child} ->
            Child;
        {ok, Child, _ } ->
            Child;
        _ ->
            eta_event:task_fault(BuildRef, Task,
                                 "Unable to start group leader.")
            end,
    group_leader(NewGL, self()),
    {OldGL, NewGL}.

%%--------------------------------------------------------------------
%% @doc
%% Stops io capture.
%%
%% @spec (Info) -> ok
%% @end
%%--------------------------------------------------------------------
capture_stop({OldGroupLeader, NewGroupLeader}) ->
    sin_group_leader:shutdown(NewGroupLeader),
    group_leader(OldGroupLeader, self()).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([BuildId, Task, Type]) ->
    {ok, #state{build_id=BuildId, task=Task, type=Type}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
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
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(shutdown, _) ->
    exit(normal).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({io_request, From, ReplyAs,
             {put_chars, io_lib, Func, [Format, Args]}},
            State) when is_list(Format) ->
    Msg = (catch io_lib:Func(Format,Args)),
    handle_msg(ReplyAs, Msg, From, Func, State),
    {noreply, State};
handle_info({io_request, From, ReplyAs,
             {put_chars, unicode, io_lib, Func, [Format, Args]}},
            State) when is_list(Format) ->
    Msg = (catch io_lib:Func(Format,Args)),
    handle_msg(ReplyAs, Msg, From, Func, State),
    {noreply, State};
handle_info({io_request, From, ReplyAs,
             {put_chars, io_lib, Func, [Format, Args]}}, State)
  when is_atom(Format) ->
    Msg = (catch io_lib:Func(Format,Args)),
    handle_msg(ReplyAs, Msg, From, Func, State),
    {noreply, State};
handle_info({io_request, From, ReplyAs,
             {put_chars, unicode, io_lib, Func, [Format, Args]}}, State)
  when is_atom(Format) ->
    Msg = (catch io_lib:Func(Format,Args)),
    handle_msg(ReplyAs, Msg, From, Func, State),
    {noreply, State};
handle_info({io_request, From, ReplyAs, {put_chars, Bytes}}, State) ->
    handle_msg(ReplyAs, Bytes, From, put_chars, State),
    {noreply, State};
handle_info({io_request, From, ReplyAs, {put_chars,unicode,Bytes}}, State) ->
    handle_msg(ReplyAs, Bytes, From, put_chars, State),
    {noreply, State};
handle_info(IoReq, State) when element(1, IoReq) == io_request ->
    %% something else, just pass it on
    group_leader() ! IoReq,
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.




%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

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
handle_msg(ReplyAs, Msg, From, Func,
           #state{build_id=BuildId, task=Task, type=Type}) ->
    case Msg of
	{'EXIT',_} ->
	    From ! {io_reply,ReplyAs,{error,Func}};
	_ ->
	    From ! {io_reply,ReplyAs,ok}
    end,
    eta_event:task_event(BuildId, Task, Type, Msg).
