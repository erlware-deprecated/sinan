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
%%% @copyright 2006-2010 Erlware
%%%---------------------------------------------------------------------------
-module(eta_task_runner).


%% API
-export([run_task_reply/4, run_task_reply/5, run_task/3, run_task/4]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Start the task running process, reply to the gen_server who
%%  made the request.
%% @spec (From::pid(), Chain::chain(), Target::task(),
%%        PrePostHandler::function()) -> ok
%% @end
%%--------------------------------------------------------------------
run_task_reply(From, Chain, Target, PrePostHandler) ->
    proc_lib:spawn_link(fun() ->
                                Res = do_tasks(Chain, Target, PrePostHandler),
                                gen_server:reply(From, Res)
                        end).


%%--------------------------------------------------------------------
%% @doc
%%  Start the task running process, reply to the gen_server who
%%  made the request.
%% @spec (From::pid(), Chain::chain(), Target::task(), RunId::run_id(),
%%        PrePostHandler::function()) -> ok
%% @end
%%--------------------------------------------------------------------
run_task_reply(From, Chain, Target, RunId, PrePostHandler) ->
    proc_lib:spawn_link(fun() ->
                                Res = do_tasks(Chain, Target, RunId,
					       PrePostHandler),
                                gen_server:reply(From, Res)
                        end).

%%--------------------------------------------------------------------
%% @doc
%%   Runs the selected task with no attempt at a return value
%%
%% @spec (Chain::chain(), Target::task(),
%%         PrePostHandler::function()) -> ok
%% @end
%%--------------------------------------------------------------------
run_task(Chain, Target, PrePostHandler) ->
    proc_lib:spawn_link(fun() ->
                                do_tasks(Chain, Target, PrePostHandler)
                        end).


%%--------------------------------------------------------------------
%% @doc
%%   Runs the selected task with no attempt at a return value
%%
%% @spec (Chain::chain(), Target::task(), RunId::run_id(),
%%        PrePostHandler::function()) -> ok
%% @end
%%--------------------------------------------------------------------
run_task(Chain, Target, RunId, PrePostHandler) ->
    proc_lib:spawn_link(fun() ->
                                do_tasks(Chain, Target, RunId, PrePostHandler)
                        end).

%%====================================================================
%%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%%  run the task chain and event handlers, gathering any exceptions.
%% @spec (Chain::chain(), Task::task(), PrePostHandler::function()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
do_tasks(Chain, Task, PrePostHandler) ->
    RunId = eta_engine:make_run_id(),
    do_tasks(Chain, Task, RunId, PrePostHandler).


%%--------------------------------------------------------------------
%% @doc
%% Do the task with a run id.
%% @spec (Chain::chain(), Task::task(), RunId::run_id(),
%%        PrePostHandler::function()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
do_tasks(Chain, Task, RunId, PrePostHandler) ->
    eta_event:run_start(RunId),
    PreHandlers = eta_meta_task:get_pre_chain_handlers(Chain),
    execute_handlers(RunId, PreHandlers, Chain),
    Tasks = get_tasks(Task),
    try run_tasks(RunId, Chain, Tasks, PrePostHandler) catch
     _:problem ->
         eta_event:run_fault(RunId, "Task failed");
     _:Error ->
         eta_event:run_fault(RunId,
                             {"Error : ~p, Stacktrace : \n ~p",
                              [Error, erlang:get_stacktrace()]})
      after
        do_post_event_handlers(RunId, Chain),
        eta_event:run_stop(RunId)
    end,
    RunId.

%%--------------------------------------------------------------------
%% @doc
%%   Run the post event handlers for the task chain.
%% @spec do_post_event_handlers(RunId::run_id(), Chain::chain()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
do_post_event_handlers(RunId, Chain) ->
    PostHandlers = eta_meta_task:get_post_chain_handlers(Chain),
    try execute_handlers(RunId, PostHandlers, Chain) catch
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
%% @spec get_tasks(Task::task()) -> Tasks::task_list()
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
%% @spec (RunId::run_id(), Chain::chain(), Tasks::task_list(),
%%        PrePostHandlers::function()) -> ok
%% @end
%% @private
%%-------------------------------------------------------------------
run_tasks(RunId, Chain, [Task | RestTasks], PrePostHandler)  ->
    try execute_task_stack(RunId, Chain, Task, PrePostHandler) catch
        _:{eta_excep, Problem} ->
            eta_event:task_fault(RunId, Task, {"~p", [Problem]}),
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
            eta_event:task_fault(RunId, Task, {"~p: ~p",
                                               [Error,
                                                erlang:get_stacktrace()]}),
            throw(problem)
    end,
    run_tasks(RunId, Chain, RestTasks, PrePostHandler);
run_tasks(_, _, [], _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Executes the full task stack. This includes pre and post handlers.
%%  This does not catch exceptions. The stack execution fails
%%  and no more tasks are run if a failure occures.
%% @spec (RunId::run_id(), Chain::chain(), Task::task(),
%%        PrePostHandler::function()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
execute_task_stack(RunId, Chain, Task, PrePostHandler) ->
    PreHandlers = eta_meta_task:get_pre_task_handlers(Chain, Task),
    execute_handlers(RunId, PreHandlers, Chain),
    Res = apply_task(RunId, Task, PrePostHandler),
    PostHandlers = eta_meta_task:get_post_task_handlers(Chain, Task),
    execute_handlers(RunId, PostHandlers, Chain),
    Res.

%%--------------------------------------------------------------------
%% @doc
%%  Executes the set of handlers with RunId, Chain and Arg information.
%% @spec (RunId::run_id(), Handlers::handler_list(), Chain::chain()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
execute_handlers(RunId, [Handler | Rest], Chain) ->
    apply_handler(RunId, Handler),
    execute_handlers(RunId, Rest, Chain);
execute_handlers(_, [], _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Apply the task. If its a fun, call the fun. If its an atom call
%%  its do_task function.
%% @spec (RunId::run_id(), Task::task(), PrePostHandler::fun()) -> Result::term()
%% @end
%% @private
%%--------------------------------------------------------------------
apply_task(RunId, Task, PrePostHandler) when is_function(Task) ->
    apply_pre_post_handler(pre, Task, RunId, PrePostHandler),
    Task(RunId),
    apply_pre_post_handler(post, Task, RunId, PrePostHandler);
apply_task(RunId, Task, PrePostHandler) when is_atom(Task) ->
    apply_pre_post_handler(pre, Task, RunId, PrePostHandler),
    Task:do_task(RunId),
    apply_pre_post_handler(post, Task, RunId, PrePostHandler).


%%--------------------------------------------------------------------
%% @doc
%%  Apply the task. If its a fun, call the fun. If its an atom call
%%  its do_task function.
%% @spec (RunId::run_id(), Handler::handler()) -> Result::term()
%% @end
%% @private
%%--------------------------------------------------------------------
apply_handler(RunId, Handler) when is_function(Handler) ->
    Handler(RunId);
apply_handler(RunId, Handler) when is_atom(Handler) ->
    Handler:do_event(RunId).

%%--------------------------------------------------------------------
%% @doc
%%  Apply the pre/post handler if it exists
%% @spec (Type::atom(), Task::task(), RunId::run_id(), Handler::term()) -> Result::term()
%% @end
%% @private
%%--------------------------------------------------------------------
apply_pre_post_handler(_Type, _Task, _RunId, none) ->
    ok;
apply_pre_post_handler(Type, Task, RunId, Handler) ->
    TaskName = eta_task:get_task_name(Task),
    Handler(Type, TaskName, RunId).

