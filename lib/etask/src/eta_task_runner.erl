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
    sin_event:event(RunRef, {run, start}),
    Tasks = get_tasks(Task),
    try run_tasks(RunRef, Tasks, Args) catch
    _:Error ->
        sin_event:event(RunRef, {{run, error}, {Error, erlang:get_stacktrace()}})
    after
        sin_event:event(RunRef, {run, complete})
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
    case sin_task:get_task_chain(Task) of
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
run_tasks(RunRef, [Task | RestTasks], Args) ->
    sin_event:event(RunRef, {{task, start}, Task}),
    case catch Task:do_task(RunRef, Args) of
        {'EXIT', {undef, _}} ->
            sin_event:event(RunRef, {{task, fail}, {undefined, Task}}),
            throw({undefined_task, {"Undefined task handler", Task}});
        {'EXIT', _} ->
            sin_event:event(RunRef, {{task, fail}, {error, Task}});
         _ ->
            sin_event:event(RunRef, {task, complete})
    end,
    run_tasks(RunRef, RestTasks, Args);
run_tasks(_, [], _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Setup the run.
%% @spec setup_run() -> ok.
%% @end
%%--------------------------------------------------------------------
setup_run() ->
    make_ref().

