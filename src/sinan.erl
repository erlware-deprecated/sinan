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
-export([main/0,
	 run_sinan/0,
	 run_sinan/1,
         do_task/3]).

-export_type([args/0,
	      task_name/0]).

-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%%====================================================================
%% Types
%%====================================================================

-type args() :: ktj_parse:object().
-type task_name() :: sin_task:task_name().

%%====================================================================
%% API
%%====================================================================

%% @doc run the specified task
-spec do_task(task_name(), string(), sin_config:config()) -> ok.
do_task(Task, StartDir, Override) ->
    try
        TaskDesc = sin_task:get_task(Task),
        case TaskDesc#task.bare of
            false ->
                do_task_full(StartDir, Override, Task);
            true ->
                do_task_bare(StartDir, Override, Task)
        end
    catch
        {pe, {_, _, {task_not_found, TaskName}}} ->
            sin_error_store:signal_error(),
            ewl_talk:say("Task not found ~s.", [TaskName])
    end.

%% @doc run the specified task with a full project dir
-spec do_task_full(string(), sin_config:config(), task_name()) -> ok.
do_task_full(StartDir, Override, Task) when is_atom(Task) ->
    try
        Config = sin_discover:discover(StartDir, Override),
        ProjectRoot = sin_config:get_value(Config, "project.dir"),
        run_task(Task, ProjectRoot, Config)
    catch
        no_build_config ->
            sin_error_store:signal_error(),
            ewl_talk:say("No build config found.");
        {unable_to_create_canonical, {_, _,Desc}}  ->
            sin_error_store:signal_error(),
            ewl_talk:say("Error discovering project layout: ~s", Desc);
        Error = {pe, {Module, _, _}} ->
            sin_error_store:signal_error(),
            ewl_talk:say("build problem ~s", [Module:format_exception(Error)]);
        Type:Exception ->
            sin_error_store:signal_error(),
            ewl_talk:say("build problem ~p:~p:~p",
                         [Type, Exception, erlang:get_stacktrace()])
    end.

%% @doc run the specified task, without expecting a build config and what not.
-spec do_task_bare(string(), task_name(), args()) -> ok.
do_task_bare(StartDir, Config, Task) when is_atom(Task) ->
    run_task(Task, StartDir, Config).

%% @doc do the full run of sinan as required by the command line args, halt the
%% system when the run is complete.
-spec main() -> sin_config:config() | ok.
main() ->
    run_sinan(),
    case sin_error_store:has_errors() of
	ErrCount when ErrCount =< 0 ->
	    init:stop();
	_ ->
	    init:stop(101)
    end.

%% @doc do the full run of sinan as required by the command line args
-spec run_sinan() -> sin_config:config() | ok.
run_sinan() ->
    Args = init:get_plain_arguments(),
    run_sinan(Args).

%% @doc do a full run of sinan with arbitrary args that may be parsed like
%% command line args
-spec run_sinan([string()]) -> sin_config:config().
run_sinan(Args) ->
    case getopt:parse(option_spec_list(), Args) of
        {ok, {Options, NonOptArgs}} ->
	    do_build(Options, NonOptArgs);
	{error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            usage()
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec do_build(term(), [string()]) -> sin_config:config().
do_build(Options, [Target | Rest]) ->
    do_task(list_to_atom(Target),
	    find_start_dir(Options),
	    setup_config_overrides(Options, Rest));
do_build(Options, []) ->
    do_build(Options, ["build"]).

-spec usage() -> ok.
usage() ->
    usage(option_spec_list()).

-spec usage(term()) -> ok.
usage(OptSpecList) ->
    getopt:usage(OptSpecList, "", "[command] [option1 option2]....",
                 [{"var=value",
                   "Variables that will affect the compilation (e.g. debug=1)"},
                  {"command",
                   "Commands that will be executed by erlb (e.g. compile)"}]).

-spec option_spec_list() -> list().
option_spec_list() ->
    [{verbose, $v, "verbose", {boolean, false},
      "Be verbose about what gets done"},
     {start_dir, $s, "start-dir", string, "The search location for the project"},
     {release, $r, "release", string, "the release to build"},
     {project, $p, "project", string, "the name of the project"},
     {version, $n, "nversion", string, "the version of the project"}].

%% @doc run the task including all task dependencies
-spec run_task(task_name(), string(), sin_config:config()) -> ok.
run_task(Task, ProjectDir, BuildConfig) ->
    try
       Tasks = sin_task:get_task_list(Task),
       case sin_hooks:get_hooks_function(ProjectDir) of
           no_hooks ->
               lists:foldl(
                 fun(TaskDesc, NewConfig) ->
                         ewl_talk:say("starting: ~p",
                                      TaskDesc#task.name),
                         NewNewConfig =
                             (TaskDesc#task.task_impl):do_task(NewConfig),
                         NewNewConfig
                 end, BuildConfig, Tasks);
           HooksFun ->
               lists:foldl(
                 fun(TaskDesc, NewConfig) ->
                         ewl_talk:say("starting: ~p", TaskDesc#task.name),
                         HooksFun(pre, Task, NewConfig),
                         NewNewConfig =
                             (TaskDesc#task.task_impl):do_task(NewConfig),
                         HooksFun(post, Task, NewNewConfig),
                         NewNewConfig
                 end, BuildConfig, Tasks)
       end
    catch
        throw:{task_not_found, Task} ->
            sin_talk:say("Unknown task ~p", [Task])
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
    push_values_if_exist(sin_config:parse_args(Args, sin_config:new()),
                         Options,
                         [{release, "-r"},
                          {start_dir, "start_dir"},
                          {project, "project.name"},
                          {version, "project.vsn"}]).

%% @doc This pushes the values given in the args (if they exist) into the
%% override config under the name specified in key.
%%
%% For example the name/key mapping expects an atom for the name that matches
%% the atom specified in the release and a string as the key. The string is used
%% as the key to push into the config
-spec push_values_if_exist(sin_config:config(), term(), [{atom(), string()}]) ->
    sin_config:config().
push_values_if_exist(Config, Options, [{Name, Key} | Rest]) ->
    case lists:keysearch(Name, 1, Options) of
        {value, {Name, Value}} ->
            push_values_if_exist(sin_config:store(Config, Key, Value),
                                 Options, Rest);
        _ ->
            push_values_if_exist(Config, Options, Rest)
    end;
push_values_if_exist(Config, _Options, []) ->
    Config.

%%====================================================================
%% Tests
%%====================================================================
push_values_if_exist_test() ->
    Options = [{foo, "bar"},
               {bar, "baz"},
               {z, 333},
               {noid, avoid}],
    Config = push_values_if_exist(sin_config:new(), Options, [{bar, "monsefu"},
                                                              {z, "chiclayo"}]),
    ?assertMatch("baz", sin_config:get_value(Config, "monsefu")),
    ?assertMatch(333, sin_config:get_value(Config, "chiclayo")),
    ?assertMatch(undefined, sin_config:get_value(Config, "noid")).
