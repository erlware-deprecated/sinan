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
%%%  Provides a nice interface to the eventing system.
%%% @end
%%% @copyright (C) 2007, Eric Merritt
%%% Created : 18 Nov 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(eta_event).

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
    gen_event:start_link({local, ?SERVER}).

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
