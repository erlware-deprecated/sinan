%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%  Provides a nice interface to the eventing system. 
%%% @end
%%% @copyright (C) 2007, Eric Merritt
%%% Created : 18 Nov 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_event).

%% API
-export([start_link/0, event/2, add_handler/2]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec start_link() -> ok
%%
%%  Starts the gen_event with the sin_event name.
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link(?SERVER).

%%--------------------------------------------------------------------
%% @doc 
%% @spec event(RunRef, Event) -> ok
%% 
%%  Send an event to the event system.
%% @end
%%--------------------------------------------------------------------
event(RunRef, Event) ->
    gen_event:notify(?SERVER, {run_event, RunRef, Event}).

%%--------------------------------------------------------------------
%% @doc 
%% @spec add_handler(Handler, Args) -> ok
%% 
%% Add a handler for this event system. 
%% @end
%%--------------------------------------------------------------------
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).


%%====================================================================
%% Internal functions
%%====================================================================
