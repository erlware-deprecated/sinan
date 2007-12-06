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
%%%   This module uses both the eta_task and eta_meta_task to generate a list
%%%   of tasks and pre post handlers. It then executes each task (plus pre
%%%   post events) in the order in which the tasks are designed to be run.
%%%
%%%   Prior to the start of each run the system generates a 'RunRef' that is
%%%   basically a unique id that is used to identify an individual instance of
%%%   a run. At the same time it generates the runs 'pre' tasks.
%%%
%%%   It then runs the tasks as specified. Running pre and post task handlers
%%%   as required.
%%%
%%%   A the end of a task run the system runs the post chain handlers. It runs
%%%   the post chain handlers even if the build has failed for some reason or
%%%   other.
%%%
%%%   @type handler() = atom() | function(). A pre/post event task handler.
%%%   @type handler_list() = [handler()]. A list of handler items
%%%   @type task() = atom() | function(). A task that needs to be run.
%%%   @type task_list() = [task()]. A set of tasks that need to be run.
%%%   @type chain() = atom(). An id for a task chain, basically a namespace for tasks.
%%%   @type run_id() = term(). A unique id used to identify an instance of a run.
%%%   @type arg_list() = [string()]. A list of args from the command line.
%%% @end
%%% @copyright 2006 Erlware
%%%---------------------------------------------------------------------------
-module(eta_task_runner).


%% API
-export([run_task/3, run_task/2]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Start the task running process, reply to the gen_server who
%%  made the request.
%% @spec run_task(From::pid(), Chain::chain(), Args::arg_list()) -> ok
%% @end
%%--------------------------------------------------------------------
run_task(From, Chain, Args) ->
    proc_lib:spawn_link(fun() ->
                                {Target, LArgs} = get_target(Args),
                                Res = do_tasks(Chain, Target, LArgs),
                                gen_server:reply(From, Res)
                        end).

%%--------------------------------------------------------------------
%% @doc
%%   Runs the selected task with no attempt at a return value
%%
%% @spec run_task(Chain::chain(), Args::arg_list()) -> ok
%% @end
%%--------------------------------------------------------------------
run_task(Chain, Args) ->
    proc_lib:spawn_link(fun() ->
                                {Target, LArgs} = get_target(Args),
                                do_tasks(Chain, Target, LArgs)
                        end).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%%
%% @doc
%%  We want a bare arg as the first argument. If anything starts
%%  with a - or is an empty list there isn't a target.
%%
%% @spec get_target(Args::arg_list()) -> none | {Target::string(),
%%                                               Rest::list() }
%% @end
%% @private
%%--------------------------------------------------------------------
get_target(All = [[$- | _] | _]) ->
    {none, All};
get_target([Target | Rest]) ->
    {Target, Rest};
get_target([]) ->
    {none, []}.

%%--------------------------------------------------------------------
%% @doc
%%  run the task chain and event handlers, gathering any exceptions.
%% @spec run_engine(Chain::chain(), Task::task(), Args::arg_list()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
do_tasks(Chain, Task, Args) ->
    RunId = setup_run(),
    eta_event:run_start(RunId),
    PreHandlers = eta_meta_task:get_pre_chain_handlers(Chain),
    execute_handlers(RunId, PreHandlers, Chain, Args),
    Tasks = get_tasks(Task),
    try run_tasks(RunId, Chain, Tasks, Args) catch
    _:problem ->
        eta_event:run_fault(RunId, "Task failed");
    _:Error ->
        eta_event:run_fault(RunId,
                            {"Error : ~p, Stacktrace : \n ~p",
                             [Error, erlang:get_stacktrace()]})
    after
        do_post_event_handlers(RunId, Chain, Args),
        eta_event:run_stop(RunId)
    end.

%%--------------------------------------------------------------------
%% @doc
%%   Run the post event handlers for the task chain.
%% @spec do_post_event_handlers(RunId::run_id(), Chain::chain(), Args::arg_list()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
do_post_event_handlers(RunId, Chain, Args) ->
    PostHandlers = eta_meta_task:get_post_chain_handlers(Chain),
    try execute_handlers(RunId, PostHandlers, Chain, Args) catch
    _:problem ->
        eta_event:run_fault(RunId, "Task failed");
    _:Error ->
        eta_event:run_fault(RunId,
                            {"Error : ~p, Stacktrace : \n ~p",
                             [Error, erlang:get_stacktrace()]})
    end.


%%--------------------------------------------------------------------
%% @doc
%%  Get the tasks that we need to run from the task server.
%% @spec get_tasks(Task::task()) -> Tasks::task_list().
%% @end
%% @private
%%--------------------------------------------------------------------
get_tasks(Task) when is_list(Task) ->
    get_tasks(list_to_atom(Task));
get_tasks(Task) when is_atom(Task) ->
    case eta_task:gen_task_chain(Task) of
        [] ->
            throw(invalid_task);
        TaskImplList ->
            TaskImplList
    end.

%%-------------------------------------------------------------------
%% @doc
%%  executes the task at the head of the task list. It also
%%  executes any pre and post handlers required.
%% @spec run_handlers(RunId::run_id(), Chain::chain(), Tasks::task_list(), Args::arg_list()) -> ok
%% @end
%% @private
%%-------------------------------------------------------------------
run_tasks(RunId, Chain, [Task | RestTasks], Args)  ->
    eta_event:task_start(RunId, Task),
    try execute_task_stack(RunId, Chain, Task, Args) catch
        _:{eta_exec, Problem} ->
            eta_event:task_fault(RunId, Task, {"~p", Problem}),
            throw(problem);
        _:{eta_excep, _Problem, Description} ->
            eta_event:task_fault(RunId, Task, Description),
            throw(problem);
        _:{'EXIT', {undef, _}} ->
            eta_event:task_fault(RunId, Task, "Undefined implementation"),
            throw(problem);
        _:{'EXIT', Reason} ->
            eta_event:task_fault(RunId, Task, {"~p", [Reason]}),
            throw(problem);
        _:Error ->
            eta_event:task_fault(RunId, Task, {"~p", [Error]}),
            throw(problem)
    end,
    run_tasks(RunId, Chain, RestTasks, Args);
run_tasks(_, _, [], _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Executes the full task stack. This includes pre and post handlers.
%%  This does not catch exceptions. The stack execution fails
%%  and no more tasks are run if a failure occures.
%% @spec execute_task_stack(RunId::run_id(), Chain::chain(), Task::task(), Args::arg_list()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
execute_task_stack(RunId, Chain, Task, Args) ->
    PreHandlers = eta_meta_event:get_pre_task_handlers(Chain, Task),
    execute_handlers(RunId, PreHandlers, Chain, Args),
    Res = apply_task(RunId, Task, Args),
    PostHandlers = eta_meta_event:get_post_task_handlers(Chain, Task),
    execute_handlers(RunId, PostHandlers, Chain, Args),
    Res.

%%--------------------------------------------------------------------
%% @doc
%%  Executes the set of handlers with RunId, Chain and Arg information.
%% @spec execute_handlers(RunId::run_id(), Handlers::handler_list(), Chain::chain(), Args::arg_list()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
execute_handlers(RunId, [Handler | Rest], Chain, Args) ->
    apply_task(RunId, Handler, Args),
    execute_handlers(RunId, Rest, Chain, Args);
execute_handlers(_, [], _, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Apply the task. If its a fun, call the fun. If its an atom call
%%  its do_task function.
%% @spec apply_task(RunId::run_id(), Task::task(), Args::arg_list()) -> Result::term()
%% @end
%% @private
%%--------------------------------------------------------------------
apply_task(RunId, Task, Args) when is_function(Task) ->
    Task(RunId, Args);
apply_task(RunId, Task, Args) when is_atom(Task) ->
    Task:do_task(RunId, Args).

%%--------------------------------------------------------------------
%% @doc
%%   Does all of the work to setup a run. This includes (at the very
%%   least) generating a unique id for this 'run'.
%% @spec setup_run() -> UniqueBuildId::run_id()
%% @end
%% @private
%%--------------------------------------------------------------------
setup_run() ->
    make_ref().

