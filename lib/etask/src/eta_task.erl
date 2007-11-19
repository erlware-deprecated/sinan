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
%%%  Accepts task descriptions, handles task chaining.
%%% @end
%%% @copyright (C) 2007, Eric Merritt
%%% Created : 18 Oct 2007 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(eta_task).

-behaviour(gen_server).

-include("eunit.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, register_task/5, unregister_task/1, gen_task_chain/1, get_task_def/1, shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tasks}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec shutdown() -> ok
%% 
%% @doc 
%%  Tell this server to shutdown.
%% @end
%%--------------------------------------------------------------------
shutdown() ->
    gen_server:cast(?SERVER, shutdown).

%%--------------------------------------------------------------------
%% @spec register_task(TaskName::atom(), TaskImpl::atom(), Deps::list(), Callable::bool(), Desc::string()) -> ok
%%
%% @doc
%%  Register a task with the task handler.
%% @end
%%--------------------------------------------------------------------
register_task(TaskName, TaskImpl, Deps, Callable, Desc) when is_atom(TaskName), is_atom(TaskImpl) ->
    register_task(TaskName, TaskImpl, Deps, Callable, Desc, []).

%%--------------------------------------------------------------------
%% @spec register_task(TaskName::atom(), TaskImpl::atom(), Deps::list(), Callable::bool(), 
%%                     Desc::string(), Opt::string()) -> ok
%%
%% @doc
%%  Register a task with the task handler.
%% @end
%%--------------------------------------------------------------------
register_task(TaskName, TaskImpl, Deps, Callable, Desc, Opts) when is_atom(TaskName), is_atom(TaskImpl) ->
    NewCallable = case Callable of
                      true ->
                          true;
                      _ ->
                          false
                  end,
    gen_server:cast(?SERVER, {register_task, TaskName, TaskImpl, Deps, NewCallable, Desc, Opts}).
    
%%--------------------------------------------------------------------
%% @spec unregister_task(TaskName::atom()) -> ok
%%
%% @doc
%%  unregister a task with the system. In this case the task.
%% @end
%%--------------------------------------------------------------------
unregister_task(TaskName) when is_atom(TaskName) ->
    gen_server:cast(?SERVER, {unregister_task, TaskName}).

%%--------------------------------------------------------------------
%% @spec gen_task_chain(TaskName) -> Result::TaskChain
%%
%% @type TaskChain = [TaskName]. The list of dependent tasks in the proper order
%%
%% @doc
%%  Generates the list of tasks that TaskName depends on in the order that they should be executed.
%% @end
%%--------------------------------------------------------------------
gen_task_chain(TaskName) when is_atom(TaskName) ->
    gen_server:call(?SERVER, {task_chain, TaskName}).

%%--------------------------------------------------------------------
%% @spec get_task_def(TaskName) -> {value, TaskDef} | none
%%
%% @type TaskDef = {TaskImplementation::atom(), Deps::list(), Callable::bool(), Desc::string()}
%%
%% @doc
%%  Get the definition for a task identified by TaskName. Returns the task name, the module name
%%  that implements the task, wether or not the task is callable directly by the user and
%%  the help/description for the task.
%% @end
%%--------------------------------------------------------------------
get_task_def(TaskName) when is_atom(TaskName) ->
    gen_server:call(?SERVER, {task_def, TaskName}).


%%--------------------------------------------------------------------
%% @doc 
%%  Get the command line options for the system.
%% @spec get_task_opts(TaskName::atom()) -> Opts::list()
%% @end
%%--------------------------------------------------------------------
get_task_opts(TaskName) when is_atom(TaskName) ->
    gen_server:call(?SERVER, {task_opts, TaskName}).


%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}.
%%
%% @doc
%% Initializes the gen server. In this case, checking to see
%% if a task list has been saved. If it has loading the presaved list.
%% if it has not then creating an empty list.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{tasks=gb_trees:empty()}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%%
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call({task_chain, TaskName}, _From, State = #state{tasks=Tasks}) ->
    TaskChain = resolve_tasks(reorder_tasks(gather_tasks(Tasks, TaskName, [])), Tasks, []),
    {reply, TaskChain, State};
handle_call({task_def, TaskName}, _From, State = #state{tasks=Tasks}) ->
    Res = gb_trees:lookup(TaskName, Tasks),
    {reply, Res, State};
handle_call({task_opts, TaskName}, _From, State = #state{tasks=Tasks}) ->
    Res = gb_trees:lookup(TaskName, Tasks),
    Opts = get_opts(Res),
    {reply, Opts, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%%
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({register_task, TaskName, TaskImpl, Deps, Callable, Desc, Opts}, #state{tasks=Tasks}) ->
    {noreply, #state{tasks=register_task(TaskName, {TaskImpl, Deps, Callable, Desc, Opts}, Tasks)}};
handle_cast({unregister_task, TaskName}, #state{tasks=Tasks}) ->
    {noreply, #state{tasks=unregister_task(TaskName, Tasks)}};
handle_cast(shutdown, _) ->
    exit(normal).

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%%
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void().
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
%%--------------------------------------------------------------------
%% @spec register_task(TaskName::atom(), TaskDesc::tuple(), Tasks::gb_tree()) -> NewTasks::gb_tree()
%%
%% @doc
%%  Take a task name and task desc and register it with the task list.
%% @end
%%--------------------------------------------------------------------
register_task(TaskName, TaskDesc = {_, _, _, _, _}, Tasks) ->
    gb_trees:enter(TaskName, TaskDesc, Tasks).

%%--------------------------------------------------------------------
%% @spec unregister_task(TaskName::atom(), Tasks::gb_tree()) -> NewTasks::gb_tree()
%%
%% @doc
%%  Remove the task from the list of tasks.
%% @end
%%--------------------------------------------------------------------
unregister_task(TaskName, Tasks) ->
    gb_trees:delete(TaskName, Tasks).

%%--------------------------------------------------------------------
%% @spec gather_tasks(Task, Acc) -> NewAcc.
%%
%% @doc
%%  Gather up all the task in the list and return them as graph edges
%%  in the {Task, DepTask} format.
%% @end
%% @private
%%--------------------------------------------------------------------
gather_tasks(Tasks, TaskName, Acc) ->
    gather_depends(Tasks, TaskName,
                   get_deps(gb_trees:lookup(TaskName, Tasks)),
                   Acc).


%%--------------------------------------------------------------------
%% @spec gather_depends(Tasks, TaskName, Deps, Acc) -> NewAcc
%%
%% @doc
%%  Gather up the dependences of a task and format them into edges.
%% @end
%% @private
%%--------------------------------------------------------------------
gather_depends(_, TaskName, [], []) ->
    [{'NONE', TaskName}];
gather_depends(_, _TaskName, [], Acc) ->
    Acc;
gather_depends(Tasks, TaskName, [H|T], Acc) ->
    Acc2 = gather_tasks(Tasks, H, [{H, TaskName} | Acc]),
    gather_depends(Tasks, TaskName, T, Acc2).

%%--------------------------------------------------------------------
%% @spec reorder_tasks(Tasks) -> NewTasks.
%%
%% @doc
%%  Reorder the tasks according to thier dependency set.
%% @end
%% @private
%%--------------------------------------------------------------------
reorder_tasks(Tasks) ->
    case eta_topo:sort(Tasks) of
        {ok, TaskList} ->
            TaskList;
        {cycle, _} ->
            ewl_talk:say("There was a cycle in the task list. "
                         "Unable to complete build!"),
            throw(task_list_cycle)
    end.

%%--------------------------------------------------------------------
%% @spec resolve_tasks(TaskName::list(), Tasks::gb_trees(), Acc::list()) -> TaskImpls::list()
%%
%% @doc
%%  Given a list of tasks names that may include the dummy task 'NONE' resolve the implementations
%%  coresponding to said names.
%% @end
%%--------------------------------------------------------------------
resolve_tasks(['NONE' | Tail], Tasks, Acc) ->
    resolve_tasks(Tail, Tasks, Acc);
resolve_tasks([Task | Tail], Tasks, Acc) ->
    Impl = get_impl(gb_trees:lookup(Task, Tasks)),
    resolve_tasks(Tail, Tasks, [Impl | Acc]);
resolve_tasks([], _Tasks, Acc) ->
    lists:reverse(Acc).

%%--------------------------------------------------------------------
%% @spec get_deps(TaskDesc::tuple()) -> Deps::list()
%%
%% @doc
%%  Retrieve the deps from the task tuple.
%% @end
%%--------------------------------------------------------------------
get_deps({value, {_, Deps, _, _, _}}) ->
    Deps;
get_deps(none) ->
    none.


%%--------------------------------------------------------------------
%% @spec get_impl(TaskDesc::tuple()) -> Impl::atom()
%%
%% @doc
%%  Get the task implementation from the lyst
%% @end
%%--------------------------------------------------------------------
get_impl({value, {Impl, _, _, _, _}}) ->
    Impl;
get_impl(none) ->
    none.

%%--------------------------------------------------------------------
%% @doc 
%%  Get the opts for the system.
%% @spec get_opts(TaskDesc::tuple()) -> Opts
%% @end
%%--------------------------------------------------------------------
get_opts({value, {_, _, _, _, Opts}}) ->
    Opts;
get_opts(none) ->
    [].


%%====================================================================
%%% Tests
%%====================================================================
gen_deps_test() ->
    Trees = register_task(task1, {task1_impl, [task2, task3], true, ""}, gb_trees:empty()),
    Trees1 = register_task(task2, {task2_impl, [task3, task4], true, ""}, Trees),
    Trees2 = register_task(task3, {task3_impl, [], true, ""}, Trees1),
    Trees3 = register_task(task4, {task4_impl, [task3], true, ""}, Trees2),
    TaskList = resolve_tasks(reorder_tasks(gather_tasks(Trees3, task1, [])), Trees3, []),
    [task3_impl, task4_impl, task2_impl, task1_impl] == TaskList.

gen_server_test() ->
    %% Give the server a chance to shut down. wait 100 millis
    timer:sleep(100),
    {ok, _} = start_link(),
    register_task(task1, task1_impl, [task2, task3], true, ""),
    register_task(task2, task2_impl, [task3, task4], true, ""),
    register_task(task3, task3_impl, [], true, ""),
    register_task(task4, task4_impl, [task3], true, ""),
    TaskList = gen_task_chain(task1),
    shutdown(),
    [task3_impl, task4_impl, task2_impl, task1_impl] ==  TaskList.


get_task_def_test() ->
    %% Give the server a chance to shut down. wait 100 millis
    timer:sleep(100),
    {ok, _} = start_link(),
    register_task(task1, task1_impl, [task2, task3], true, ""),
    register_task(task2, task2_impl, [task3, task4], true, ""),
    register_task(task3, task3_impl, [], true, ""),
    register_task(task4, task4_impl, [task3], true, ""),
    TaskDef = get_task_def(task1),
    shutdown(),
    {value, {task1_impl, [task2, task3], true, ""}} = TaskDef.
