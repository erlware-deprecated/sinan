%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  The module provides a simple api for the sinan system.
%%%  Two possible arguments may be passed in. The start dir that should
%%%  be somewhere inside a project and a list of args for the system.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%-------------------------------------------------------------------
-module(sinan).

%% API
-export([do/1,
         do/2,
         do/3,
         main/0,
         main/1,
         run_sinan/0,
         run_sinan/1,
         manual_start/0,
         usage/0]).

-export_type([args/0,
              task_name/0,
              app/0]).

-include_lib("sinan/include/sinan.hrl").


%%====================================================================
%% Types
%%====================================================================

-type args() :: [string()].
-type task_name() :: sin_task:task_name().
-type app() :: record(app).

%%====================================================================
%% API
%%====================================================================
-spec do(atom()) -> ok | error.
do(Task) ->
    do(Task, [], []).

-spec do(atom(), [{atom(), term()}]) -> ok | error.
do(Task, Options) ->
    do(Task, [], Options).

-spec do(atom(), [string()], term()) -> {ok | error, sin_state:state()}.
do(undefined, _, Options) ->
    do(help, [], Options);
do(Target, Rest, Options) ->
    case do_internal(Target, Rest, Options) of
        {ok, _} ->
            ok;
        {error, _} ->
            error
    end.

%% @doc run the specified task with a full project dir
-spec do_task_full(sin_config:config(), sin_state:state(), task_name()) -> ok.
do_task_full(Config0, State0, Task) when is_atom(Task) ->
    try
        {Config1, State1} = sin_discover:discover(Config0, State0),
        State2 = sin_sig:load(sin_state:get_value(build_dir, State1), State1),
        ProjectRoot = sin_state:get_value(project_dir, State2),
        State3 = run_task(Task, ProjectRoot, Config1, State2),
        sin_sig:save(sin_state:get_value(build_dir, State3), State3)
    catch
        no_build_config ->
            sin_log:normal(Config0, "No build config found."),
            sin_state:add_run_error(Task, no_build_config, State0);
        Error = {pe, NewState, {Module, _, _}} ->
            sin_log:normal(Config0, Module:format_exception(Error)),
            NewState;
        Type:Exception ->
            sin_log:normal(Config0, "build problem ~p:~p:~p",
                         [Type, Exception, erlang:get_stacktrace()]),
            sin_state:add_run_error(Task, Exception, State0)
    end.

%% @doc run the specified task, without expecting a build config and what not.
-spec do_task_bare(string(), sin_config:config(),
                   sin_state:state(), task_name()) -> ok.
do_task_bare(StartDir, Config, State, Task) when is_atom(Task) ->
    run_task(Task, StartDir, Config, State).

%% @doc do the full run of sinan as required by the command line args, halt the
%% system when the run is complete.
-spec main() -> sin_config:config() | ok.
main() ->
    case run_sinan() of
        {ok, _} ->
            init:stop();
        {error, _} ->
            init:stop(101)
    end.

-spec main([string()]) -> sin_config:config() | ok.
main(Args) ->
    case run_sinan(Args) of
        {ok, _} ->
            init:stop();
        {error, _} ->
            init:stop(101)
    end.

%% @doc do the full run of sinan as required by the command line args
-spec run_sinan() -> sin_config:config() | ok.
run_sinan() ->
    Args = init:get_plain_arguments(),
    run_sinan(Args).

%% @doc do a full run of sinan with arbitrary args that may be parsed like
%% command line args
-spec run_sinan([string()]) -> {ok, sin_config:config()} | {error, term()}.
run_sinan(Args) ->
    case manual_start() of
        ok ->
            case getopt:parse(option_spec_list(), Args) of
                {ok, {Options, [Target | Rest]}} ->
                    do_internal(erlang:list_to_atom(Target), Rest, Options);
                {ok, {Options, []}} ->
                    do_internal(undefined, [], Options);
                {error, {Reason, Data}} ->
                    io:format("Error: ~s ~p~n~n", [Reason, Data]),
                    usage(),
                    {error, failed}
            end;
        Else ->
            Else
    end.



%%% @doc allow the sinan application to manually start itself
%%% and all dependencies
-spec manual_start() -> ok.
manual_start() ->
    lists:foldl(fun(App, ok) ->
                        case application:start(App) of
                            {error, {already_started, App}} ->
                                ok;
                            ok ->
                                ok;
                            Error ->
                                io:format("Error starting OTP Application ~p~n",
                                          [Error]),
                                Error
                        end;
                   (_App, Else) ->
                        Else
                end,
                ok,
                [kernel,
                 stdlib,
                 erlware_commons,
                 getopt]).

-spec usage() -> ok.
usage() ->
    usage(option_spec_list()).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc run the specified task
-spec do_task(task_name(), string(), sin_config:config()) -> ok.
do_task(Task, StartDir, Config) ->
    try
        State = sin_state:store(start_dir, StartDir, sin_state:new()),
        TaskDesc = sin_task:get_task(State, Task),
        case TaskDesc#task.bare of
            false ->
                do_task_full(Config, State, Task);
            true ->
                do_task_bare(StartDir, Config, State, Task)
        end
    catch
        {pe, NewState, {_, _, {task_not_found, TaskName}}} ->
            sin_log:normal(Config, "Task not found ~s.", [TaskName]),
            NewState
    end.

usage(OptSpecList) ->
    getopt:usage(OptSpecList, "", "[command] [option1 option2]....",
                 [{"var=value",
                   "Variables that will affect the compilation (e.g. debug=1)"},
                  {"command",
                   "Commands that will be executed by sinan (e.g. build)"}]).

-spec option_spec_list() -> list().
option_spec_list() ->
    [{verbose, $v, "verbose", {boolean, false},
      "Be verbose about what gets done"},
     {start_dir, $s, "start-dir", string, "The search location for the project"},
     {user_dir, $u, "user-dir", string,
      "The directory to use as the users home directory"},
     {release, $r, "release", string, "the release to build"},
     {project, $p, "project", string, "the name of the project"},
     {version, $n, "nversion", string, "the version of the project"}].

%% @doc run the task including all task dependencies
-spec run_task(task_name(), string(), sin_config:config(),
               sin_state:state()) -> ok.
run_task(Task, ProjectDir, Config0, State0) ->
    Tasks = sin_task:get_task_list(Config0, Task),
    case sin_hooks:get_hooks_function(State0, ProjectDir) of
        no_hooks ->
            lists:foldl(
              fun(TaskDesc, State1) ->
                      sin_log:verbose(Config0, "starting: ~p",
                                   TaskDesc#task.name),
                      Matcher = sin_config:create_matcher([{release,
                                                            sin_state:get_value(release, State0)},
                                                            {task, TaskDesc#task.name}],
                                                          Config0),
                      (TaskDesc#task.task_impl):do_task(Matcher, State1)
              end, State0, Tasks);
        HooksFun ->
            lists:foldl(
              fun(TaskDesc, State1) ->
                      sin_log:verbose(Config0, "starting: ~p", TaskDesc#task.name),
                      State2 = HooksFun(pre, TaskDesc#task.name, Config0, State1),
                      Matcher = sin_config:create_matcher([{task, TaskDesc#task.name}],
                                                          Config0),
                      State3 =
                          (TaskDesc#task.task_impl):do_task(Matcher, State2),
                      HooksFun(post, TaskDesc#task.name, Config0, State3)
              end, State0, Tasks)
       end.

%% @doc parse the start dir out of the args passed in.
-spec find_start_dir(Options::term()) -> string().
find_start_dir(Options) ->
    case lists:keysearch(start_dir, 1, Options) of
         {value, {start_dir, StartDir}} ->
            StartDir;
        _ ->
            {ok, Dir} = file:get_cwd(),
            Dir
    end.

%% @doc Setup all configuration overrides from the command line
-spec setup_config_overrides(Options::term(), term()) ->
    sin_config:config().
setup_config_overrides(Options, Args) ->
    push_values_if_exist(sin_config:add(additional_args, Args,
                                        sin_config:new()),
                         Options,
                         [{release, '-r'},
                          {verbose, verbose},
                          {user_dir, user_dir},
                          {start_dir, start_dir},
                          {project, project_name},
                          {version, project_vsn}]).

%% @doc This pushes the values given in the args (if they exist) into the
%% override config under the name specified in key.
%%
%% For example the name/key mapping expects an atom for the name that matches
%% the atom specified in the release and a string as the key. The string is used
%% as the key to push into the config
-spec push_values_if_exist(sin_config:config(),
                           term(), [{atom(), string()}]) ->
    sin_config:config().
push_values_if_exist(Config, Options, [{Name, Key} | Rest]) ->
    case lists:keysearch(Name, 1, Options) of
        {value, {Name, Value}} ->
            push_values_if_exist(sin_config:add(Key, Value, Config),
                                 Options, Rest);
        _ ->
            push_values_if_exist(Config, Options, Rest)
    end;
push_values_if_exist(Config, _Options, []) ->
    Config.



-spec do_internal(atom(), [string()], term()) -> {ok | error, sin_state:state()}.
do_internal(undefined, _, Options) ->
    do_internal(help, [], Options);
do_internal(Target, Rest, Options) ->
    Result = do_task(Target,
                     find_start_dir(Options),
                     setup_config_overrides(Options, Rest)),

    case sin_state:get_run_errors(Result) of
        [] ->
            {ok, Result};
        _ ->
            {error, Result}
    end.
%%====================================================================
%% Tests
%%====================================================================
-ifndef(NOTEST).
-include_lib("eunit/include/eunit.hrl").

push_values_if_exist_test() ->
    Options = [{foo, "bar"},
               {bar, "baz"},
               {z, 333},
               {noid, avoid}],
    Config = push_values_if_exist(sin_config:new(),
                                  Options, [{bar, monsefu},
                                            {z, chiclayo}]),
    ?assertMatch("baz", sin_config:match(monsefu, Config)),
    ?assertMatch(333, sin_config:match(chiclayo, Config)),
    ?assertMatch(undefined, sin_config:match(noid, undefined, Config)).

-endif.
