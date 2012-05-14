%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2006 - 2011, Eric Merritt
%%% @doc
%%%  Provides task sorting and manipulation support to the sinan.
%%% @end
%%%-------------------------------------------------------------------
-module(sin_task).


-include_lib("sinan/include/sinan.hrl").

-export([get_task/2,
         get_task_list/2,
         get_tasks/0,
         behaviour_info/1,
         format_exception/1,
         ensure_started/1]).

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
-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        {error, {already_started, _}} ->
            ok;
        ok ->
            ok;
        Error ->
          erlang:throw({error_starting_app, App, Error})
    end.

%% @doc get a specific task description
-spec get_task(sin_state:state(), task_name()) -> [task_name()].
get_task(State, TaskName) ->
    Tasks = get_tasks(),
    get_task(State, TaskName, Tasks).

%% @doc get a dependency ordered list of tasks from the system.
-spec get_task_list(sin_state:state(), task_name()) -> [task_name()].
get_task_list(State, TaskName) ->
    Tasks = get_tasks(),
    RootTask = get_task(State, TaskName, Tasks),
    lists:map(fun(DepTaskName) ->
                      get_task(State, DepTaskName, Tasks)
              end,
              process_deps(State, RootTask, Tasks)).

%% @doc get a list of all tasks in the system
-spec get_tasks() -> [record(task)].
get_tasks() ->
    [sin_task_depends:description(),
     sin_task_dialyzer:description(),
     sin_task_version:description(),
     sin_task_eunit:description(),
     sin_task_proper:description(),
     sin_task_eqc:description(),
     sin_task_shell:description(),
     sin_task_release:description(),
     sin_task_help:description(),
     sin_task_gen:description(),
     sin_task_doc:description(),
     sin_task_dist:description(),
     sin_task_clean:description(),
     sin_task_build:description(),
     sin_task_xref:description(),
     sin_task_erts:description(),
     sin_task_escript:description(),
     sin_task_echo:description(),
     sin_task_cucumber:description()].

%% @doc define the behaviour for tasks.
behaviour_info(callbacks) ->
    [{description, 0}, {do_task, 2}];
behaviour_info(_) ->
    undefined.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%%% Internal functions
%%====================================================================

-spec get_task(sin_state:state(),
               task_name(), [task_description()]) -> task_description().
get_task(_State, TaskName, [Task = #task{name = TaskName} | _]) ->
    Task;
get_task(State, TaskName, [_ | Rest]) ->
    get_task(State, TaskName, Rest);
get_task(State, TaskName, _) ->
    ?SIN_RAISE(State, {task_not_found, TaskName}).

process_deps(State, Task, Tasks) ->
    {DepChain, _, _} = process_deps(State, Task, Tasks, []),
    ['NONE' | Rest] =
        reorder_tasks(State,
                      lists:flatten([{'NONE', Task#task.name} | DepChain])),
    Rest.

process_deps(State, Task, Tasks, Seen) ->
    case lists:member(Task, Seen) of
        true ->
            {[], Tasks, Seen};
        false ->
            Deps = Task#task.deps,
            DepList = lists:map(fun(Dep) ->
                                        {Dep, Task#task.name}
                                end, Deps),
            {NewDeps, _, NewSeen} =
                lists:foldl(fun(Arg, Acc) ->
                                    process_dep(State, Arg, Acc)
                            end,
                                                {[], Tasks, Seen}, Deps),
            {[DepList | NewDeps], Tasks, NewSeen}
    end.

process_dep(State, TaskName, {Deps, Tasks, Seen}) ->
    Task = get_task(State, TaskName, Tasks),
    {NewDeps, _, NewSeen} = process_deps(State,
                                         Task, Tasks, [TaskName | Seen]),
    {[Deps | NewDeps], Tasks, NewSeen}.

%% @doc Reorder the tasks according to thier dependency set.
reorder_tasks(State, OTaskList) ->
    case sin_topo:sort(OTaskList) of
        {ok, TaskList} ->
            TaskList;
        {cycle, _} ->
            ?SIN_RAISE(State, cycle_fault,
                       "There was a cycle in the task list. "
                       "Unable to complete build!")
    end.

%%====================================================================
%%% Tests
%%====================================================================


