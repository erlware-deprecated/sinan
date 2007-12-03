%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Erlware
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
%%%  This allows clients to add tasks around functions. Handlers may be
%%%  either pre run of the entire chain or post run of the entire chain.
%%%  Handlers may also be set to run either before or after a particular
%%%  task. 'Around' style tasks can be set up by setting handlers both
%%%  before and after the task.
%%%
%%%  Handlers may be functions or the name of a module that implements the
%%%  {@link task_event_handler} behaviour or a function that takes exactly two args
%%%  just like tasks. In either case the function will be called with two
%%%  arguments. The build identifier and any args for the task chain.
%%%
%%%
%%% @type handler() = atom() | function().
%%%   The handler may be a function or atom that is the name of a
%%%  module that implements the {@link task_event_handler} behaviour.
%%%
%%% @type chain() = atom().
%%%  The chain is simply a namespace for a group of related tasks.
%%%  Tasks aren't a explicitly part of any chain but a chain name
%%%  is specified when a task is run.
%%%
%%% @type handler_list = [handler()].
%%%  A list of handlers.
%%%
%%% @type task() = atom().
%%%  The name of the task to execute the handler on.
%%%
%%%
%%% @end
%%% @copyright (C) 2007, Erlware
%%% Created : 28 Nov 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(eta_meta_task).

-behaviour(gen_server).

-include("eunit.hrl").

%% API
-export([start_link/1,
         register_pre_chain_handler/2,
         remove_pre_chain_handler/2,
         get_pre_chain_handlers/1,
         clear_pre_chain_handlers/1,
         register_post_chain_handler/2,
         remove_post_chain_handler/2,
         get_post_chain_handlers/1,
         clear_post_chain_handlers/1,
         register_pre_task_handler/3,
         remove_pre_task_handler/3,
         get_pre_task_handlers/2,
         clear_pre_task_handlers/2,
         register_post_task_handler/3,
         remove_post_task_handler/3,
         get_post_task_handlers/2,
         clear_post_task_handlers/2,
         stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CHAIN_MAGIC_NAME, '$$chain$$').
-define(GET_MAGIC_NAME, '$$get$$').
-define(CLEAR_MAGIC_NAME, '$$clear$$').

-record(state, {tid}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(Tid) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Tid], []).

%%--------------------------------------------------------------------
%% @doc
%%  Regester a handler that should happen before a task chain is
%%  started.
%% @spec register_pre_chain_handler(Chain::chain(), Handler::handler()) -> ok
%% @end
%%--------------------------------------------------------------------
register_pre_chain_handler(Chain, Handler)
  when is_atom(Handler); is_function(Handler) ->
    cast_handler_action(register, pre, Chain, ?CHAIN_MAGIC_NAME, Handler).

%%--------------------------------------------------------------------
%% @doc
%%  Remove a pre chain handler. This only removes the exact handler
%%  specified and not the handlers themselves.
%% @spec remove_pre_chain_handler(Chain::chain(), Handler::handler()) -> ok
%% @end
%%--------------------------------------------------------------------
remove_pre_chain_handler(Chain, Handler)
  when is_atom(Handler); is_function(Handler) ->
    cast_handler_action(remove, pre, Chain, ?CHAIN_MAGIC_NAME, Handler).
%%--------------------------------------------------------------------
%% @doc
%%  Get the list of all the pre chain handlers for a chain.
%% @spec get_pre_chain_handlers(Chain::chain()) -> Handlers::handler_list()
%% @end
%%--------------------------------------------------------------------
get_pre_chain_handlers(Chain) ->
    call_handler_action(get, pre, Chain, ?CHAIN_MAGIC_NAME, ?GET_MAGIC_NAME).

%%--------------------------------------------------------------------
%% @doc
%%  clear all the pre chain handlers for the specific chain speficied
%%
%% @spec clear_pre_chain_handlers(Chain::chain()) -> ok
%% @end
%%--------------------------------------------------------------------
clear_pre_chain_handlers(Chain) ->
    cast_handler_action(clear, pre, Chain, ?CHAIN_MAGIC_NAME,
                        ?CLEAR_MAGIC_NAME).

%%--------------------------------------------------------------------
%% @doc
%%  Register a post chain handler. Register a handler to execute
%%  after the execution of a task chain.
%% @spec register_post_chain_handler(Chain::chain(), Handler::handler()) -> ok
%% @end
%%--------------------------------------------------------------------
register_post_chain_handler(Chain, Handler)
  when is_atom(Handler); is_function(Handler) ->
    cast_handler_action(register, post, Chain, ?CHAIN_MAGIC_NAME, Handler).

%%--------------------------------------------------------------------
%% @doc
%%  Remove a post chain handler from the system.
%% @spec remove_post_chain_handler(Chain::chain(), Handler::handler()) -> ok
%% @end
%%--------------------------------------------------------------------
remove_post_chain_handler(Chain, Handler)
  when is_atom(Handler); is_function(Handler) ->
    cast_handler_action(remove, post, Chain, ?CHAIN_MAGIC_NAME, Handler).

%%--------------------------------------------------------------------
%% @doc
%%  Get a list of all post chain handlers.
%%
%% @spec get_post_chain_handlers(Chain::chain()) -> Handlers::handler_list()
%% @end
%%--------------------------------------------------------------------
get_post_chain_handlers(Chain) ->
    call_handler_action(get, post, Chain, ?CHAIN_MAGIC_NAME, ?GET_MAGIC_NAME).

%%--------------------------------------------------------------------
%% @doc
%%  Removes all of post chain handlers.
%%
%% @spec clear_post_chain_handlers(Chain::chain()) -> ok
%% @end
%%--------------------------------------------------------------------
clear_post_chain_handlers(Chain) ->
    cast_handler_action(clear, post, Chain, ?CHAIN_MAGIC_NAME,
                        ?CLEAR_MAGIC_NAME).

%%--------------------------------------------------------------------
%% @doc
%%  Register a new handler on the task named 'Task' in the specified
%%  chain.
%% @spec register_pre_task_handler(Chain::chain(), Task::task(), Handler::handler()) -> ok
%% @end
%%--------------------------------------------------------------------
register_pre_task_handler(Chain, Task, Handler)
  when is_atom(Handler); is_function(Handler) ->
    cast_handler_action(register, pre, Chain, Task, Handler).

%%--------------------------------------------------------------------
%% @doc
%%   Remove a specific pre task handler from the system.
%% @spec remove_pre_task_handler(Chain::chain(), Task::task(), Handler::handler()) -> ok
%% @end
%%--------------------------------------------------------------------
remove_pre_task_handler(Chain, Task, Handler)
  when is_atom(Handler); is_function(Handler) ->
    cast_handler_action(remove, pre, Chain, Task, Handler).

%%--------------------------------------------------------------------
%% @doc
%%  Get all of the pre task handlers for the specified task.
%% @spec get_pre_task_handlers(Chain::chain(), Task::task()) -> Handlers::handler_list()
%% @end
%%--------------------------------------------------------------------
get_pre_task_handlers(Chain, Task) ->
    call_handler_action(get, pre, Chain, Task, ?GET_MAGIC_NAME).

%%--------------------------------------------------------------------
%% @doc
%%  Clear all pre handlers from the specified train
%% @spec clear_pre_task_handlers(Chain, Task) -> ok
%% @end
%%--------------------------------------------------------------------
clear_pre_task_handlers(Chain, Task) ->
    cast_handler_action(clear, pre, Chain, Task,
                        ?CLEAR_MAGIC_NAME).

%%--------------------------------------------------------------------
%% @doc
%%  Register a new post task handler for the specified chain
%% @spec register_post_task_handler(Chain::chain(), Task::task(), Handler::handler()) -> ok
%% @end
%%--------------------------------------------------------------------
register_post_task_handler(Chain, Task, Handler)
  when is_atom(Handler); is_function(Handler) ->
    cast_handler_action(register, post, Chain, Task, Handler).

%%--------------------------------------------------------------------
%% @doc
%%  remove a specific post task handler from the specified chain.
%% @spec remove_post_task_handler(Chain::chain(), Task::task(), Handler::handler()) -> ok
%% @end
%%--------------------------------------------------------------------
remove_post_task_handler(Chain, Task, Handler)
  when is_atom(Handler); is_function(Handler) ->
    cast_handler_action(remove, post, Chain, Task, Handler).

%%--------------------------------------------------------------------
%% @doc
%%  Get all of the post handlers for the specified chain.
%% @spec get_post_task_handlers(Chain, Task) -> Handlers:handler_list()
%% @end
%%--------------------------------------------------------------------
get_post_task_handlers(Chain, Task) ->
    call_handler_action(get, post, Chain, Task, ?GET_MAGIC_NAME).

%%--------------------------------------------------------------------
%% @doc
%%  Clear all of the tasks from the post task handler.
%% @spec clear_post_task_handlers(Chain, Task) -> ok
%% @end
%%--------------------------------------------------------------------
clear_post_task_handlers(Chain, Task) ->
    cast_handler_action(clear, post, Chain, Task,
                        ?CLEAR_MAGIC_NAME).
%%--------------------------------------------------------------------
%% @doc
%%  Stop the server. This should really only be used in very specific
%%  information.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%%
%% @doc
%% Initiates the server
%% @end
%%--------------------------------------------------------------------
init([Tid]) ->
    {ok, #state{tid=Tid}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
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
handle_call({{get, Type}, Chain, TaskName, ?GET_MAGIC_NAME}, _From, State = #state{tid = Tid}) ->
    Reply = get_handlers(Tid, Type, Chain, TaskName),
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%%
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({{register, Type}, Chain, TaskName, Handler}, State = #state{tid = Tid}) ->
    register_handler(Tid, Type, Chain, TaskName, Handler),
    {noreply, State};
handle_cast({{remove, Type}, Chain, TaskName, Handler}, State = #state{tid = Tid}) ->
    remove_handler(Tid, Type, Chain, TaskName, Handler),
    {noreply, State};
handle_cast({{clear, Type}, Chain, TaskName, ?CLEAR_MAGIC_NAME}, State = #state{tid = Tid}) ->
    clear_handlers(Tid, Type, Chain, TaskName),
    {noreply, State};
handle_cast(stop, _) ->
    exit(normal).

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%%
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
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
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
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
%%  Cast a  handler action to the server. Most of the chains are
%%  have the same key structure. Its only a matter of the values changing.
%%  this abstracts that fact.
%%
%% @spec cast_handler_action(Action, Type, Chain, TaskName, Handler) -> ok
%% @end
%%--------------------------------------------------------------------
cast_handler_action(Action, Type, Chain, TaskName, Handler) ->
    gen_server:cast(?SERVER, {{Action, Type}, Chain, TaskName, Handler}).

%%--------------------------------------------------------------------
%% @doc
%%  call the server and return whatever is returned.
%%
%% @spec call_handler_action(Action, Type, Chain, TaskName, Handler) -> ok
%% @end
%%--------------------------------------------------------------------
call_handler_action(Action, Type, Chain, TaskName, Handler) ->
    gen_server:call(?SERVER, {{Action, Type}, Chain, TaskName, Handler}).

%%--------------------------------------------------------------------
%% @doc
%%  Get the list of handlers for this type/task.
%%
%% @spec get_handlers(Tid, Type::atom(), Chain::atom(), TaskName::atom()) ->
%%  Handlers::list()
%% @end
%%--------------------------------------------------------------------
get_handlers(Tid, Type, Chain, TaskName) ->
    reformat_list(ets:lookup(Tid, {TaskName, Chain, Type}), []).

%%--------------------------------------------------------------------
%% @doc
%%  Register this handler with the system.
%%
%% @spec register_handler(Tid, Type::atom(), Chain::atom(), TaskName::atom(), Handler::term()) -> true
%% @end
%%--------------------------------------------------------------------
register_handler(Tid, Type, Chain, TaskName, Handler) ->
    ets:insert(Tid, {{TaskName, Chain, Type}, Handler}).

%%--------------------------------------------------------------------
%% @doc
%%  Remove a specific handler from the system.
%%
%% @spec remove_handler(Tid, Type, Chain, TaskName, Handler) -> true
%% @end
%%--------------------------------------------------------------------
remove_handler(Tid, Type, Chain, TaskName, Handler) ->
    ets:delete_object(Tid, {{TaskName, Chain, Type}, Handler}).

%%--------------------------------------------------------------------
%% @doc
%%  Clear all handlers for a specific task from the system.
%%
%% @spec clear_handlers(Tid, Type, Chain, TaskName) -> true
%% @end
%%--------------------------------------------------------------------
clear_handlers(Tid, Type, Chain, TaskName) ->
    ets:delete(Tid, {TaskName, Chain, Type}).

%%--------------------------------------------------------------------
%% @doc
%%
%% @spec reformat_list(Values::ValueList, Acc::Acc) ->
%%   List = [{term(), handler()}]
%%   Acc = [handler()]
%% @end
%%--------------------------------------------------------------------
reformat_list([{_, Value} | Tail], Acc) ->
    reformat_list(Tail, [Value | Acc]);
reformat_list([], Acc) ->
    lists:reverse(Acc).

%%====================================================================
%%% Tests
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Checks the ability to register individual tasks.
%%
%% @spec register_chain_test() -> bool()
%% @end
%%--------------------------------------------------------------------
register_chain_test() ->
    timer:sleep(1000),
    Tid = ets:new(test, [duplicate_bag, public]),
    start_link(Tid),
    register_pre_chain_handler(some_chain, some_handler1),
    register_pre_chain_handler(some_chain, some_handler2),
    register_pre_chain_handler(some_chain, some_handler3),
    register_pre_chain_handler(some_chain, some_handler4),
    Res = get_pre_chain_handlers(some_chain),
    stop(),
    ets:delete(Tid),
    [some_handler1, some_handler2, some_handler3, some_handler4] =
        Res.

remove_chain_test() ->
    timer:sleep(1000),
    Tid = ets:new(test, [duplicate_bag, public]),
    start_link(Tid),
    register_pre_chain_handler(some_chain, some_handler1),
    register_pre_chain_handler(some_chain, some_handler2),
    register_pre_chain_handler(some_chain, some_handler3),
    register_pre_chain_handler(some_chain, some_handler4),
    remove_pre_chain_handler(some_chain, some_handler2),
    Res = get_pre_chain_handlers(some_chain),
    stop(),
    ets:delete(Tid),
    [some_handler1, some_handler3, some_handler4] =
        Res.

clear_chain_test() ->
    timer:sleep(1000),
    Tid = ets:new(test, [duplicate_bag, public]),
    start_link(Tid),
    register_pre_chain_handler(some_chain, some_handler1),
    register_pre_chain_handler(some_chain, some_handler2),
    register_pre_chain_handler(some_chain, some_handler3),
    register_pre_chain_handler(some_chain, some_handler4),
    clear_pre_chain_handlers(some_chain),
    Res = get_pre_chain_handlers(some_chain),
    stop(),
    ets:delete(Tid),
    [] = Res.





