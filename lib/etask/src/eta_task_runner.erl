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
%%%   The async task runner for the sinan engine
%%% @end
%%% @copyright 2006 Eric Merritt <cyberlync@gmail.com>
%%%---------------------------------------------------------------------------
-module(eta_task_runner).


%% API
-export([run_task/2, run_task/1]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec run_task(From::pid(), Args::list()) -> ok
%%
%% @doc
%%  Start the task running process, reply to the gen_server who
%%  made the request.
%% @end
%%--------------------------------------------------------------------
run_task(From, Args) ->
    proc_lib:spawn_link(fun() ->
                                {Target, LArgs} = get_target(Args),
                                Res = do_tasks(Target, LArgs),
                                gen_server:reply(From, Res)
                        end).

%%--------------------------------------------------------------------
%% @spec run_task(Args::list()) -> ok
%%
%% @doc
%%   Runs the selected task with no attempt at a return value
%% @end
%%--------------------------------------------------------------------
run_task(Args) ->
    proc_lib:spawn_link(fun() ->
                                {Target, LArgs} = get_target(Args),
                                do_tasks(Target, LArgs)
                        end).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec get_target(Args::list()) -> none | {Target::string(),
%%                                           Rest::list() }
%%
%% @doc
%%  We want a bare arg as the first argument. If anything starts
%%  with a - or is an empty list there isn't a target.
%% @end
%%--------------------------------------------------------------------
get_target(All = [[$- | _] | _]) ->
    {none, All};
get_target([Target | Rest]) ->
    {Target, Rest};
get_target([]) ->
    {none, []}.

%%--------------------------------------------------------------------
%% @doc
%%  run the task set, gathering any exceptions.
%% @spec run_engine(Task, Args) -> ok.
%% @end
%% @private
%%--------------------------------------------------------------------
do_tasks(Task, Args) ->
    RunRef = setup_run(),
    eta_event:run_start(RunRef),
    Tasks = get_tasks(Task),
    try run_tasks(RunRef, Tasks, Args) catch
    _:problem ->
        eta_event:run_fault(RunRef, "Task failed");
    _:Error ->
        eta_event:run_fault(RunRef,
                            {"Error : ~p, Stacktrace : \n ~p",
                             [Error, erlang:get_stacktrace()]})
    after
        eta_event:run_stop(RunRef)
    end.


%%--------------------------------------------------------------------
%% @doc
%% @spec get_tasks(Task) -> TaskList.
%%  Get the tasks that we need to run from the task server.
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
%% @spec run_handlers(Task, Handler) -> ok
%% @doc
%%   Runs the specified handler.
%% @end
%% @private
%%-------------------------------------------------------------------
run_tasks(RunRef, [Task | RestTasks], Args)  ->
    eta_event:task_start(RunRef, Task),
    try apply_task(Task, RunRef, Args),
        run_tasks(RunRef, RestTasks, Args) catch
        _:{eta_exec, Problem} ->
            eta_event:task_fault(RunRef, Task, {"~p", Problem}),
            throw(problem);
        _:{eta_excep, _Problem, Description} ->
            eta_event:task_fault(RunRef, Task, Description),
            throw(problem);
        _:{'EXIT', {undef, _}} ->
            eta_event:task_fault(RunRef, Task, "Undefined implementation"),
            throw(problem);
        _:{'EXIT', Reason} ->
            eta_event:task_fault(RunRef, Task, {"~p", [Reason]}),
            throw(problem);
        _:Error ->
            eta_event:task_fault(RunRef, Task, {"~p", [Error]}),
            throw(problem)
    end;
run_tasks(_, [], _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Apply the task. If its a fun, call the fun. If its an atom call
%%  its do_task function.
%% @spec apply_task(Task, RunRef, Args) -> Result
%% @end
%%--------------------------------------------------------------------
apply_task(Task, RunRef, Args) when is_function(Task) ->
    Task(RunRef, Args);
apply_task(Task, RunRef, Args) when is_atom(Task) ->
    Task:do_task(RunRef, Args).

%%--------------------------------------------------------------------
%% @doc
%%   Does all of the work to setup a run. This includes (at the very
%%   least) generating a unique id for this 'run'.
%% @spec setup_run() -> UniqueBuildId.
%% @end
%%--------------------------------------------------------------------
setup_run() ->
    make_ref().

