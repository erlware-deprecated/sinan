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
%%%  This module manages tasks, task descriptions and task relationships.
%%%
%%%  @type tid() = term(). The ets id for the ets storage table for this gen_server.
%%%  @type task_name() = atom(). The name of the task. The public global name of the system.
%%%  @type task_impl() = atom() | function(). The implementation of the task. Either
%%%    a module that implements the {@link eta_gen_task} behavior or a function that takes
%%%    two args.
%%%  @type deps() = [task_name()]. The list of dependent tasks for the task being described.
%%%  @type desc() = string(). A string description of the task, used in help among other things.
%%%  @type opts() = [{string(), string(), bool()}]. An option description, in the form of ewl_get_opts.
%%% @end
%%% @copyright (C) 2007-2010, Erlware
%%% Created : 18 Oct 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(eta_task).

-behaviour(gen_server).

-include("eunit.hrl").
-include("etask.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/1, register_task/6, register_task/5, register_task/1,
         unregister_task/1, gen_task_chain/1, get_task_def/1,
         shutdown/0, get_task_defs/0, get_task_opts/1, get_task_name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tid}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Tid::tid()) -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link(Tid) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Tid], []).

%%--------------------------------------------------------------------
%% @doc
%%  Tell this server to shutdown.
%% @spec shutdown() -> ok
%% @end
%%--------------------------------------------------------------------
shutdown() ->
    gen_server:cast(?SERVER, shutdown).

%%--------------------------------------------------------------------
%% @doc
%%  Register a task with the task handler.
%%
%% @spec register_task(TaskName::task_name(), TaskImpl::task_impl(), Deps::deps(), Callable::bool(), Desc::desc()) -> ok
%% @end
%%--------------------------------------------------------------------
register_task(TaskName, TaskImpl, Deps, Callable, Desc) when is_atom(TaskName), is_atom(TaskImpl) ->
    register_task(TaskName, TaskImpl, Deps, Callable, Desc, []).

%%--------------------------------------------------------------------
%% @doc
%%  Register a task with the task handler.
%%
%% @spec register_task(TaskName::task_name(), TaskImpl::task_impl(), Deps::deps(), Callable::bool(),
%%                     Desc::desc(), Opts::opts()) -> ok
%%
%% @end
%%--------------------------------------------------------------------
register_task(TaskName, TaskImpl, Deps, Callable, Desc, Opts)
  when is_atom(TaskName), is_atom(TaskImpl) ->
    NewCallable = case Callable of
                      true ->
                          true;
                      _ ->
                          false
                  end,
    register_task(#task{name=TaskName, task_impl=TaskImpl,
                        deps=Deps, callable=NewCallable,
                        desc=Desc, opts=Opts}).

%%--------------------------------------------------------------------
%% @doc
%%  Register a task with the task handler.
%%
%% @spec register_task(TaskRecord::record(task)) -> ok
%%
%% @end
%%--------------------------------------------------------------------
register_task(TaskRecord) when is_record(TaskRecord, task) ->
    gen_server:cast(?SERVER, {register_task, TaskRecord}).

%%--------------------------------------------------------------------
%% @doc
%%  unregister a task with the system. In this case the task.
%%
%% @spec unregister_task(TaskName::task_name()) -> ok
%% @end
%%--------------------------------------------------------------------
unregister_task(TaskName) when is_atom(TaskName) ->
    gen_server:cast(?SERVER, {unregister_task, TaskName}).

%%--------------------------------------------------------------------
%% @doc
%%  Generates the list of tasks that TaskName depends on in the order that they should be executed.
%%
%% Task name is The list of dependent tasks in the proper order
%%
%% @spec gen_task_chain(TaskName::task_name()) -> Result::TaskChain
%%    TaskChain = [TaskName]
%% @end
%%--------------------------------------------------------------------
gen_task_chain(TaskName) when is_atom(TaskName) ->
    gen_server:call(?SERVER, {task_chain, TaskName}).

%%--------------------------------------------------------------------
%% @doc
%%  Get the definition for a task identified by TaskName. Returns the task name, the module name
%%  that implements the task, wether or not the task is callable directly by the user and
%%  the help/description for the task.
%%
%% @spec get_task_def(TaskName::task_name()) -> record(task) | none
%% @end
%%--------------------------------------------------------------------
get_task_def(TaskName) when is_atom(TaskName) ->
    gen_server:call(?SERVER, {task_def, TaskName}).


%%--------------------------------------------------------------------
%% @doc
%%  Get the command line options for the system.
%% @spec get_task_opts(TaskName::task_name()) -> Opts::opts()
%% @end
%%--------------------------------------------------------------------
get_task_opts(TaskName) when is_atom(TaskName) ->
    gen_server:call(?SERVER, {task_opts, TaskName}).

%%--------------------------------------------------------------------
%% @doc
%%  get all the task descriptions in the system
%% @spec get_task_defs() -> [{TaskName::task_name(), Desc::record(task)}]
%% @end
%%--------------------------------------------------------------------
get_task_defs() ->
    gen_server:call(?SERVER, all_task_defs).

%%--------------------------------------------------------------------
%% @doc
%%  Given an implementation get a name
%% @spec (TaskImpl::term()) -> TaskName::term()
%% @end
%%--------------------------------------------------------------------
get_task_name(TaskImpl) when is_atom(TaskImpl) ->
    gen_server:call(?SERVER, {task_name, TaskImpl}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initializes the gen server. In this case, checking to see
%% if a task list has been saved. If it has loading the presaved list.
%% if it has not then creating an empty list.
%%
%% @spec init([Tid]) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @end
%% @private
%%--------------------------------------------------------------------
init([Tid]) ->
    {ok, #state{tid=Tid}}.

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%%
%% @end
%%--------------------------------------------------------------------
handle_call({task_chain, TaskName}, _From, State = #state{tid=Tid}) ->
    TaskChain = resolve_tasks(reorder_tasks(gather_tasks(Tid, TaskName, [])), Tid, []),
    {reply, TaskChain, State};
handle_call({task_def, TaskName}, _From, State = #state{tid=Tid}) ->
    Res = get_value(ets:lookup(Tid, {desc, TaskName})),
    {reply, Res, State};
handle_call({task_opts, TaskName}, _From, State = #state{tid=Tid}) ->
    Res = get_opts(ets:lookup(Tid, {desc, TaskName})),
    {reply, Res, State};
handle_call(all_task_defs, _From, State = #state{tid=Tid}) ->
    List1 = ets:tab2list(Tid),
    {reply, strip_to_defs(List1, []), State};
handle_call({task_name, TaskImpl}, _From, State = #state{tid=Tid}) ->
    [{_, Res}] = ets:lookup(Tid, {impl, TaskImpl}),
    {reply, Res, State}.

%%--------------------------------------------------------------------
%% @spec (Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%%
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({register_task, TaskDesc}, State = #state{tid=Tid}) ->
    TaskName = TaskDesc#task.name,
    register_task(Tid, TaskName, TaskDesc),
    {noreply, State};
handle_cast({unregister_task, TaskName}, State = #state{tid=Tid}) ->
    unregister_task(Tid, TaskName),
    {noreply, State};
handle_cast(shutdown, _) ->
    exit(normal).

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Strip out the key from the key/value pairs. Return just the values.
%% @spec (Tasks::[{task_name(), record(task)}], Acc::[record(task)]) -> Records::[record(task)]
%% @end
%% @private
%%--------------------------------------------------------------------
strip_to_defs([{{desc, _}, Desc} | Rest], Acc) ->
    strip_to_defs(Rest, [Desc | Acc]);
strip_to_defs([_ | Rest], Acc) ->
    strip_to_defs(Rest, Acc);
strip_to_defs([], Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @doc
%%  Take a task name and task desc and register it with the task list.
%%
%% @spec register_task(Tid::tid(), TaskName::task_name(), TaskDesc::record(task)) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
register_task(Tid, TaskName, TaskDesc) when is_record(TaskDesc, task) ->
    ets:insert(Tid, {{desc, TaskName}, TaskDesc}),
    ets:insert(Tid, {{impl, TaskDesc#task.task_impl}, TaskName}).


%%--------------------------------------------------------------------
%% @doc
%%  Remove the task from the list of tasks.
%%
%% @spec unregister_task(Tid::tid(), TaskName::task_name()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
unregister_task(Tid, TaskName) ->
    ets:delete(Tid, {desc, TaskName}).

%%--------------------------------------------------------------------
%% @doc
%%  Gather up all the task in the list and return them as graph edges
%%  in the {Task, DepTask} format.
%%
%% @spec gather_tasks(Tid::tid(), Task::task_desc(), Acc::[task_name()]) -> NewAcc::[task_name()]
%% @end
%% @private
%%--------------------------------------------------------------------
gather_tasks(Tid, TaskName, Acc) ->
    gather_depends(Tid, TaskName,
                   get_deps(ets:lookup(Tid, {desc, TaskName})),
                   Acc).


%%--------------------------------------------------------------------
%% @doc
%%  Gather up the dependences of a task and format them into edges.
%%
%% @spec gather_depends(Tid::tid(), TaskName::task_name(), Deps::deps(), Acc::[task_name()]) -> NewAcc::deps()
%% @end
%% @private
%%--------------------------------------------------------------------
gather_depends(_, TaskName, [], []) ->
    [{'NONE', TaskName}];
gather_depends(_, _TaskName, [], Acc) ->
    Acc;
gather_depends(Tid, TaskName, [H|T], Acc) ->
    Acc2 = gather_tasks(Tid, H, [{H, TaskName} | Acc]),
    gather_depends(Tid, TaskName, T, Acc2).

%%--------------------------------------------------------------------
%% @doc
%%  Reorder the tasks according to thier dependency set.
%%
%% @spec reorder_tasks(OTaskList::Tasks) -> TaskList::Tasks
%%   Tasks = [{Task::task_name(), TaskDep::task_name()}]
%% @end
%% @private
%%--------------------------------------------------------------------
reorder_tasks(OTaskList) ->
    case eta_topo:sort(OTaskList) of
        {ok, TaskList} ->
            TaskList;
        {cycle, _} ->
            eta_event:meta_event(cycle_fault,
                                 "There was a cycle in the task list. "
                                 "Unable to complete build!"),
            throw(task_list_cycle)
    end.

%%--------------------------------------------------------------------
%%
%% @doc
%%  Given a list of tasks names that may include the dummy task 'NONE' resolve the implementations
%%  coresponding to said names.
%% @spec resolve_tasks(TaskName::Tasks, Tid::tid(), Acc::list()) -> TaskImpls::TaskImplList
%%   Tasks = [{Task::task_name(), TaskDep::task_name()}]
%%   TaskImplList = [task_name()]
%% @end
%% @private
%%--------------------------------------------------------------------
resolve_tasks(['NONE' | Tail], Tid, Acc) ->
    resolve_tasks(Tail, Tid, Acc);
resolve_tasks([Task | Tail], Tid, Acc) ->
    Impl = get_impl(ets:lookup(Tid, {desc, Task})),
    resolve_tasks(Tail, Tid, [Impl | Acc]);
resolve_tasks([], _Tid, Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @doc
%%  Retrieve the deps from the task tuple.
%%
%% @spec get_deps(TaskDesc::record(task)) -> Deps::deps()
%% @end
%% @private
%%--------------------------------------------------------------------
get_deps([{{desc, _}, TaskDesc}]) ->
    TaskDesc#task.deps;
get_deps([]) ->
    none.


%%--------------------------------------------------------------------
%% @doc
%%  Get the task implementation from the lyst
%% @spec get_impl(TaskDesc::record(task)) -> Impl::task_impl()
%% @end
%% @private
%%--------------------------------------------------------------------
get_impl([{_, TaskDesc}]) ->
    TaskDesc#task.task_impl;
get_impl([]) ->
    none.

%%--------------------------------------------------------------------
%% @doc
%%  Get the opts for the system.
%% @spec get_opts(TaskDesc::record(task)) -> Opts::opts()
%% @end
%% @private
%%--------------------------------------------------------------------
get_opts([{_, TaskDesc}]) ->
    TaskDesc#task.opts;
get_opts([]) ->
    [].

%%--------------------------------------------------------------------
%% @doc
%%  Strip the ets crap off of the lookup return
%% @spec get_value([{Key::task_name(), TaskDesc::record(task)}]) -> TaskDesc::record(task) | none
%% @end
%% @private
%%--------------------------------------------------------------------
get_value([{_, TaskDesc}]) ->
    TaskDesc;
get_value([]) ->
    none.

%%====================================================================
%%% Tests
%%====================================================================
gen_deps_test() ->
    Tid = ets:new(test, [public, set]),
    register_task(Tid, task1, #task{name=task1, task_impl=task1_impl, deps=[task2, task3], callable=true, desc="", opts=[]}),
    register_task(Tid, task2, #task{name=task2, task_impl=task2_impl, deps=[task3, task4], callable=true, desc="", opts=[]}),
    register_task(Tid, task3, #task{name=task3, task_impl=task3_impl, deps=[], callable=true, desc="", opts=[]}),
    register_task(Tid, task4, #task{name=task4, task_impl=task4_impl, deps=[task3], callable=true, desc="", opts=[]}),
    TaskList = resolve_tasks(reorder_tasks(gather_tasks(Tid, task1, [])), Tid, []),
    ets:delete(Tid),
    [task3_impl, task4_impl, task2_impl, task1_impl] == TaskList.

gen_server_test() ->
    %% Give the server a chance to shut down. wait 100 millis
    timer:sleep(100),
    Tid = ets:new(test, [public, set]),
    {ok, _} = start_link(Tid),
    register_task(task1, task1_impl, [task2, task3], true, ""),
    register_task(task2, task2_impl, [task3, task4], true, ""),
    register_task(task3, task3_impl, [], true, ""),
    register_task(task4, task4_impl, [task3], true, ""),
    TaskList = gen_task_chain(task1),
    shutdown(),
    ets:delete(Tid),
    [task3_impl, task4_impl, task2_impl, task1_impl] ==  TaskList.


get_task_def_test() ->
    %% Give the server a chance to shut down. wait 100 millis
    timer:sleep(100),
    Tid = ets:new(test, [public, set]),
    {ok, _} = start_link(Tid),
    register_task(task1, task1_impl, [task2, task3], true, ""),
    register_task(task2, task2_impl, [task3, task4], true, ""),
    register_task(task3, task3_impl, [], true, ""),
    register_task(task4, task4_impl, [task3], true, ""),
    TaskDef = get_task_def(task1),
    shutdown(),
    ets:delete(Tid),
    io:format("~p", [TaskDef]),
    #task{name=task1, task_impl=task1_impl, deps=[task2, task3], callable=true, desc="", opts=[]} = TaskDef.
