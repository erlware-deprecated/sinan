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
%%% @author Eric Merritt
%%% @doc
%%%   Prints out information from the eta_event system.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created : 31 Jan 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%---------------------------------------------------------------------------
-module(sin_print_handler).

-behaviour(gen_event).

%% API
-export([start/1, stop/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager.
%% @spec (Options) -> {ok,Pid} | {error,Error}
%% @end
%%--------------------------------------------------------------------
start(Options) ->
    EventName = eta_event:event_name(),
    %% gen_event:add_handler/2 doesn't check for duplicates
    case lists:member(?MODULE, gen_event:which_handlers(EventName)) of
        true  ->
            already_started;
        false ->
            case gen_event:swap_sup_handler(EventName,
                                            {EventName, swap},
                                            {?MODULE, Options}) of
                ok ->
                    ok;
                {error, Reason} ->
                    throw({error, {?MODULE, start_link, Reason}})
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Stop this handler.
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    EventName = eta_event:event_name(),
    gen_event:swap_handler(EventName, {?MODULE, swap}, {EventName, []}).

%%--------------------------------------------------------------------
%% @doc
%% init/1 is called when a event is being installed to an event manager
%% using gen_event:add_[sup_]handler/3 function
%% @spec (Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init(_) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @spec  (Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%%
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%% @end
%%--------------------------------------------------------------------
handle_event({meta_event, Event, Desc}, State) ->
    ewl_talk:say("Got ~p from tasking system", [Event]),
    print(Desc),
    {ok, State};
handle_event({run_event, RunRef, start}, State) ->
    ewl_talk:say("Starting new run ~p", [RunRef]),
    {ok, State};
handle_event({run_event, RunRef, fault}, State) ->
    ewl_talk:say("Fault during run ~p", [RunRef]),
    {ok, State};
handle_event({run_event, RunRef, fault, Desc}, State) ->
    ewl_talk:say("Fault during run ~p", [RunRef]),
    print(Desc),
    {ok, State};
handle_event({run_event, RunRef, stop}, State) ->
    ewl_talk:say("Run complete ~p", [RunRef]),
    {ok, State};
handle_event({run_event, RunRef, Event}, State) ->
    ewl_talk:say("Event ~p signaled for run ~p", [Event, RunRef]),
    {ok, State};
handle_event({task_event, RunRef, Task, start}, State) ->
    ewl_talk:say("[RUN:~p] Task ~p started", [RunRef, Task]),
    {ok, State};
handle_event({task_event, RunRef, Task, start, Desc}, State) ->
    ewl_talk:say("[RUN:~p] Task ~p started:~s", [RunRef, Task, format(Desc)]),
    {ok, State};
handle_event({task_event, RunRef, Task, stop}, State) ->
    ewl_talk:say("[RUN:~p] Task ~p complete", [RunRef, Task]),
    {ok, State};
handle_event({task_event, RunRef, Task, stop, Desc}, State) ->
    ewl_talk:say("[RUN:~p] Task ~p complete:~s", [RunRef, Task, format(Desc)]),
    {ok, State};
handle_event({task_event, RunRef, Task, fault}, State) ->
    ewl_talk:say("[RUN:~p] Task ~p faulted", [RunRef, Task]),
    {ok, State};
handle_event({task_event, RunRef, Task, fault, Desc}, State) ->
    ewl_talk:say("[RUN:~p] Task ~p faulted:~s", [RunRef, Task, format(Desc)]),
    {ok, State};
handle_event({task_event, RunRef, Task, Event}, State) ->
    ewl_talk:say("[RUN:~p] Task ~p generated event ~p ",
                 [RunRef, Task, Event]),
    {ok, State};
handle_event({task_event, RunRef, Task, Event, Desc}, State) ->
    ewl_talk:say("[RUN:~p] ~p:~p generated event:~s", [RunRef, Task, Event,
                                               format(Desc)]),
    {ok, State};
handle_event(_, State)->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec (Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%%
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
%% @end
%%--------------------------------------------------------------------
handle_call(_Query, State) ->
    {ok, {error, bad_query}, State}.

%%--------------------------------------------------------------------
%% @spec (Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%%
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%% @end
%%--------------------------------------------------------------------
handle_info(_, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec (Reason, State) -> void()
%%
%% @doc
%% Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State)  ->
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


%%====================================================================
%%% Internal functions
%%====================================================================
print({Text, Args}) ->
    ewl_talk:say(Text, Args);
print(Text) ->
    ewl_talk:say(Text).

format({Text, Args}) ->
    io_lib:format(Text, Args);
format(Text) ->
    Text.
