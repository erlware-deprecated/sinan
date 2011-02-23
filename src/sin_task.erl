%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2006 - 2011, Eric Merritt
%%% @doc
%%%  Provides task sorting and manipulation support to the sinan.
%%% @end
%%%-------------------------------------------------------------------
-module(sin_task).

-include("internal.hrl").

-export([get_task/1,
	 get_task_list/1,
	 get_tasks/0,
	 behaviour_info/1]).

-export_type([task_description/0,
	      task_name/0]).

%%====================================================================
%%% Types
%%====================================================================

-type task_description() :: record(task).
-type task_name() :: atom().

%%====================================================================
%%% API Functions
%%====================================================================

%% @doc get a specific task description
-spec get_task(task_name()) -> [task_name()].
get_task(TaskName) ->
    Tasks = get_tasks(),
    get_task(TaskName, Tasks).

%% @doc get a dependency ordered list of tasks from the system.
-spec get_task_list(task_name()) -> [task_name()].
get_task_list(TaskName) ->
    Tasks = get_tasks(),
    RootTask = get_task(TaskName, Tasks),
    lists:map(fun(DepTaskName) ->
		      get_task(DepTaskName, Tasks)
	      end,
	      process_deps(RootTask, Tasks)).

%% @doc get a list of all tasks in the system
-spec get_tasks() -> [record(task)].
get_tasks() ->
    [sin_task_depends:description(),
     sin_task_version:description(),
     sin_task_test:description(),
     sin_task_shell:description(),
     sin_task_release:description(),
     sin_task_help:description(),
     sin_task_gen:description(),
     sin_task_doc:description(),
     sin_task_dist:description(),
     sin_task_clean:description(),
     sin_task_build:description(),
     sin_task_xref:description()].

%% @doc define the behaviour for tasks.
behaviour_info(callbacks) ->
    [{description, 0}, {do_task, 1}];
behaviour_info(_) ->
    undefined.

%%====================================================================
%%% Internal functions
%%====================================================================

-spec get_task(task_name(), [task_description()]) -> task_description().
get_task(TaskName, [Task = #task{name = TaskName} | _]) ->
    Task;
get_task(TaskName, [_ | Rest]) ->
    get_task(TaskName, Rest);
get_task(TaskName, _) ->
    throw({task_not_found, TaskName}).

process_deps(Task, Tasks) ->
    {DepChain, _, _} = process_deps(Task, Tasks, []),
    ['NONE' | Rest] =
	reorder_tasks(lists:flatten([{'NONE', Task#task.name} | DepChain])),
    Rest.

process_deps(Task, Tasks, Seen) ->
    case lists:member(Task, Seen) of
	true ->
	    {[], Tasks, Seen};
	false ->
	    Deps = Task#task.deps,
	    DepList = lists:map(fun(Dep) ->
					{Dep, Task#task.name}
				end, Deps),
	    {NewDeps, _, NewSeen} = lists:foldl(fun process_dep/2,
						{[], Tasks, Seen}, Deps),
	    {[DepList | NewDeps], Tasks, NewSeen}
    end.

process_dep(TaskName, {Deps, Tasks, Seen}) ->
    Task = get_task(TaskName, Tasks),
    {NewDeps, _, NewSeen} = process_deps(Task, Tasks, [TaskName | Seen]),
    {[Deps | NewDeps], Tasks, NewSeen}.

%% @doc Reorder the tasks according to thier dependency set.
reorder_tasks(OTaskList) ->
    case sin_topo:sort(OTaskList) of
        {ok, TaskList} ->
            TaskList;
        {cycle, _} ->
            ?SIN_RAISE_D(cycle_fault,
			 "There was a cycle in the task list. "
			 "Unable to complete build!"),
            throw(task_list_cycle)
    end.

%%====================================================================
%%% Tests
%%====================================================================


