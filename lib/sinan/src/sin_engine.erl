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
%%%   Task runner for the system.
%%% @end
%%% @copyright 2006 Eric Merritt <cyberlync@gmail.com>
%%%---------------------------------------------------------------------------
-module(sin_engine).


%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {autokill}).

-define(SERVER, ?MODULE).

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
start_link(AutoKill, Target, Args) ->
    gen_server:start_link(?MODULE, 
                          [AutoKill, Target, Args], []).


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
%% Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([AutoKill, Target, Args]) ->
    run_engine(Target, Args, AutoKill),
    {ok, #state{autokill=AutoKill}}.

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
handle_call({run, Task, Args}, _From, State) ->
    run_engine(Task, Args, State#state.autokill),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast({run, Task, Args}, State) ->
    run_engine(Task, Args, State#state.autokill), 
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

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
%% @doc 
%%  Capture any exceptions 
%% @spec run_engine(Task, Args, AutoKill) -> ok.
%% @end
%% @private
%%--------------------------------------------------------------------
run_engine(Task, Args, autokill) ->
    BuildRef = setup_build(),
    try start_run(BuildRef, Task, Args) catch
      _:Error -> 
       ewl_talk:say("unexpected error ~p: ~p", [Error, erlang:get_stacktrace()])
    after
       ewl_talk:say("run complete"),
       init:stop()
    end;  
run_engine(Task, Args, _) ->
    BuildRef = setup_build(),
    try start_run(BuildRef, Task, Args) catch
     _:Error ->            
       ewl_talk:say("unexpected error ~p: ~p", [Error, erlang:get_stacktrace()]),  
       exit(normal)
    after
       fconf:exit(BuildRef), 
       ewl_talk:say("run complete")
    end.

%%-------------------------------------------------------------------
%% @spec run(Task, Args) -> ok.
%% @doc
%%   Run the specified task given the specified args. Make sure that 
%%  all dependency tasks have also been run and all thier 
%%  dependencies etc, etc.
%% @end
%% @private
%%-------------------------------------------------------------------
start_run(BuildRef, Task, Args) ->
    Flavor = get_flavor(BuildRef, Args),
    fconf:store(BuildRef, "build.args", Args),
    fconf:store(BuildRef, "build.task", Task),
    handle_build_config(Flavor, BuildRef),
    ATask = get_task(BuildRef, Task),
    Tasks = reorder_tasks(gather_tasks(BuildRef, ATask, [])),
    case check_tasks(BuildRef, Tasks) of 
        go ->
            run_tasks(BuildRef, Flavor, Tasks);
        _ ->
            throw(invalid_task)
    end.


%%-------------------------------------------------------------------
%% @doc
%%  Get the current build flavor or the default flavor if 
%%  required.
%% @spec get_flavor(BuildRef, FlavorList) -> Flavor
%% @end
%%-------------------------------------------------------------------
get_flavor(_BuildRef, [Flavor]) ->
    Flavor;
get_flavor(BuildRef, []) ->
    fconf:get_value(BuildRef, "default_flavor", "development");    
get_flavor(BuildRef, undefined) ->
    fconf:get_value(BuildRef, "default_flavor", "development").



%%--------------------------------------------------------------------
%% @doc 
%%  Check the task list to make sure its valide.
%% @spec check_tasks(Tasks) -> go | abort.
%% @end
%% @private
%%--------------------------------------------------------------------
check_tasks(_, []) ->
    ewl_talk:say("No valid task specified. Aborting build!"),
    abort;
check_tasks(_, ['NONE']) ->
    ewl_talk:say("No valid task specified. Aborting build!"),
    abort;
check_tasks(BuildRef, ['NONE', Task]) ->
    case fconf:get_value(BuildRef, {path, ["tasks", Task, "handler"]}) of
        undefined ->
                ewl_talk:say("No valid task specified. Aborting build!"),
            abort;
        _ ->
            go
    end;
check_tasks(_, _) ->
    go.


%%--------------------------------------------------------------------
%% @spec get_task(BuildRef, Task) -> Task.
%% 
%% @doc 
%%  gets the main target task. If task is undefined it looks in the 
%%  config to detect the actual task. If no task is defined in the 
%%  config it defaults to build.
%% @end
%% @private
%%--------------------------------------------------------------------
get_task(BuildRef, undefined) ->
    case fconf:get_value(BuildRef, "default_task") of
        undefined ->
            ewl_talk:say("I couldn't find a default task, so I am "
                         " just going to use build!"),
            build;
        Task ->
            ewl_talk:say("Using task ~s", [Task]),
            list_to_atom(Task)
    end;
get_task(_, Task) ->
    Task.

%%--------------------------------------------------------------------
%% @spec gather_tasks(Task, Acc) -> NewAcc.
%% 
%% @doc 
%%  Gather up all the task in the list and return them as graph edges
%%  in the {Task, DepTask} format.
%% @end
%% @private
%%--------------------------------------------------------------------
gather_tasks(BuildRef, Task, Acc) when is_atom(Task) ->
    gather_tasks(BuildRef, atom_to_list(Task), Acc);
gather_tasks(BuildRef, Task, Acc) ->
    gather_depends(BuildRef, Task, 
                   fconf:get_value(BuildRef, 
                                   {path, ["tasks", Task, "depends"]}),
                   Acc).

%%--------------------------------------------------------------------
%% @spec gather_depends(_Task, Deps, Acc) -> NewAcc.
%% 
%% @doc 
%%  Gather up the dependences of a task and format them into edges.
%% @end
%% @private
%%--------------------------------------------------------------------
gather_depends(_, Task, undefined, []) ->
    [{'NONE', Task}];
gather_depends(_, _Task, undefined, Acc) ->
    Acc;
gather_depends(BuildRef, Task, [H|T], Acc) ->
    Acc2 = gather_tasks(BuildRef, H, [{H, Task} | Acc]),
    gather_depends(BuildRef, Task, T, Acc2);
gather_depends(_, Task, [], Acc) when length(Acc) == 0 ->
    [{'NONE', Task}];
gather_depends(_, _Task, [], Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @spec reorder_tasks(Tasks) -> NewTasks.
%% 
%% @doc 
%%  Reorder the tasks according to thier dependency set.
%% @end
%% @private
%%--------------------------------------------------------------------
reorder_tasks(Tasks) ->
    case sin_topo:sort(Tasks) of
        {ok, TaskList} ->
            TaskList;
        {cycle, _} ->
            ewl_talk:say("There was a cycle in the task list. "
                         "Unable to complete build!"),
            throw(task_list_cycle)
    end.

%%--------------------------------------------------------------------
%% @spec run_tasks(BuildRef, Flavor, Tasks) -> ok.
%% 
%% @doc 
%%  Run the list of tasks.
%% @end
%% @private
%%--------------------------------------------------------------------
run_tasks(BuildRef, Flavor, ['NONE' | T]) ->
    run_tasks(BuildRef, Flavor, T);
run_tasks(BuildRef, Flavor, [Task | T]) ->
    run_handlers(BuildRef, Flavor, Task, 
                 fconf:get_value(BuildRef, {path, 
                                            ["tasks", Task, "handler"]})),
    run_tasks(BuildRef, Flavor, T);
run_tasks(_BuildRef, _Flavor, []) ->
    ok.

%%-------------------------------------------------------------------
%% @spec run_handlers(Task, Handler) -> ok
%% @doc
%%   Runs the specified handler. 
%% @end
%% @private
%%-------------------------------------------------------------------
run_handlers(BuildRef, Flavor, Task, undefined) ->
    ewl_talk:say("Executing task ~s ...", [Task]),
    Args = fconf:get_value(BuildRef, {path, ["flavors", Flavor, Task]}, []),
    ATask = list_to_atom(Task),
    case catch ATask:ATask(BuildRef, Args) of
        {'EXIT', {undef, _}} ->
            ewl_talk:say("Task ~s doesn't seem to be defined!",
                         [Task]),
            throw({undefined_task, {"Undefined task handler", Task}});
        {'EXIT', _} ->
            ewl_talk:say("Error executing task ~s",
                         [Task]);
        _ ->
            ok
    end;
run_handlers(BuildRef, Flavor, Task, Handler) ->
    ewl_talk:say("Executing task ~s ...", [Task]),
    Args = fconf:get_value(BuildRef, {path, ["flavors", Flavor, Task]}, []),
    Fun = list_to_atom(Task),
    Module = list_to_atom(Handler),
    Module:Fun(BuildRef, Args).




%%-------------------------------------------------------------------
%% @spec handle_build_config(Flavor, BuildRef) -> ok.
%% @doc
%%   Look for the build config. Pass it off to the processor when
%%   its found.
%% @end
%% @private
%%-------------------------------------------------------------------
handle_build_config(Flavor, BuildRef) ->
    {ok, Start} = file:get_cwd(),
    case find_build_config(Start) of
        no_build_config ->
            ewl_talk:say("No build config found!!"),
            throw(no_build_config);
        BuildConfig ->
            process_build_config(Flavor, BuildRef, BuildConfig)
    end.

%%-------------------------------------------------------------------
%% @spec process_build_config(Flavor, BuildRef, BuildConfig) -> ok.
%% @doc
%%   Read in the build config/parse and send to the config process.
%% @end
%% @private
%%-------------------------------------------------------------------
process_build_config(Flavor, BuildRef, BuildConfig) ->
    Dir = filename:dirname(BuildConfig),
    case fconf:parse_config(BuildRef, BuildConfig) of
        ok ->
            ok;
        {error, {Reason, Line, Char}} ->
            ewl_talk:say("The config file seems to be miss formatted. "
                         "Got '~s' on line ~w in column ~w",
                         [Reason, Line + 1, Char + 1]),
            throw(config_parse_error)
    end,
    BuildRoot = fconf:get_value(BuildRef, "build_dir", "_build"),
    BuildDir = filename:join([Dir, BuildRoot, Flavor]),
    register_loggers(BuildDir),
    fconf:store(BuildRef, "build.root", BuildRoot),
    fconf:store(BuildRef, "build.flavor", Flavor),
    fconf:store(BuildRef, "build.dir", BuildDir),
    fconf:store(BuildRef, "build.config", BuildConfig),
    fconf:store(BuildRef, "project.dir", Dir).

%%--------------------------------------------------------------------
%% @doc 
%%  Register all of the sasl logger for file logging and the 
%%  custom logger for tty logging. 
%%
%% @spec register_loggers(BuildDir) -> ok.
%% @end
%% @private
%%--------------------------------------------------------------------
register_loggers(BuildDir) ->
    remove_existing_loggers(),
    LogFile = filename:join([BuildDir, "log", "build.log"]),
    filelib:ensure_dir(LogFile),
    %% kick off the standard sasl file logger so we can get
    %% nice build log file
    gen_event:add_handler(error_logger, sasl_report_file_h, {LogFile, all}),
    gen_event:add_handler(error_logger, sin_tty_logger, []).


%%--------------------------------------------------------------------
%% @doc 
%%  remove existing loggers that may or may not exist.
%%
%% @spec remove_existing_loggers() -> ok.
%% @end
%% @private
%%--------------------------------------------------------------------
remove_existing_loggers() ->
    Loggers = gen_event:which_handlers(error_logger),
    lists:foreach(fun(Logger) ->
                          case Logger of
                              sas_report_file_h ->
                                  gen_event:delete_handler(error_logger, 
                                                           Logger,
                                                           normal);
                              sin_tty_logger ->
                                  gen_event:delete_handler(error_logger, 
                                                           Logger,
                                                           normal);
                              error_logger ->
                                  %% Remove the primitive logger.
                                  gen_event:delete_handler(error_logger, 
                                                           Logger,
                                                           normal);
                              _ ->
                                  ok
                          end
                  end, Loggers).
                          


%%-------------------------------------------------------------------
%% @spec find_build_config(Dir::string()) -> ok.
%% @doc
%%   find "_build_config" in the current directory. if not recurse
%%   with parent directory.
%% @end
%% @private
%%-------------------------------------------------------------------
find_build_config("/") ->
    no_build_config;
find_build_config(Start) ->
    ConfigFile = filename:join(Start, "_build.cfg"),
    case file:read_file_info(ConfigFile) of
        {ok, _FileInfo} ->
            ConfigFile;
        {error, _Reason} ->
            find_build_config(sin_utils:parent_dir(Start))
    end.


%%--------------------------------------------------------------------
%% @doc 
%%  Setup the build.
%% @spec setup_build() -> ok.
%% @end
%%--------------------------------------------------------------------
setup_build() ->
    BuildRef = make_ref(),
    DefaultConfig = 
        filename:join([code:priv_dir(sinan), 
                       "default_build"]),
    fconf:start_config(BuildRef, 
                       fun sin_parse_handle:parse_config_file/1),
    fconf:parse_config(BuildRef, 
                       DefaultConfig),
    BuildRef.
