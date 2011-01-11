%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
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
%%%  The module provides a simple api for the sinan system.
%%%  Two possible arguments may be passed in. The start dir that should
%%%  be somewhere inside a project and a list of args for the system.
%%%
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created :  8 Dec 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sinan).

%% API
-export([main/0,
	 main/1,
         do_task/3,
         start/0]).

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
%% @doc
%%  run the specified task
%% @end
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
        {task_not_found, TaskName} ->
            ewl_talk:say("Task not found ~s.", [TaskName])
    end.

%% @doc
%%  run the specified task with a full project dir
%% @end
-spec do_task_full(string(), sin_config:config(), task_name()) -> ok.
do_task_full(StartDir, Override, Task) when is_atom(Task) ->
    try
        Config = sin_discover:discover(StartDir, Override),
	ProjectRoot = sin_config:get_value(Config, "project.dir"),
	run_task(Task, ProjectRoot, Config)
    catch
        no_build_config ->
            ewl_talk:say("No build config found.");
        {unable_to_create_canonical, {_, _,Desc}}  ->
	    ewl_talk:say("Error discovering project layout: ~s", Desc);
	{sin_excep, Problem}  ->
	    ewl_talk:say("build problem ~s", [Problem]);
	{sin_excep, _, {Description, EArgs}}  ->
	    ewl_talk:say(Description, EArgs);
    	{sin_excep, _, Description}  ->
	    ewl_talk:say("~s", [Description])
    end.

%% @doc
%%  run the specified task, without expecting a build config and
%% what not.
%% @end
-spec do_task_bare(string(), task_name(), args()) -> ok.
do_task_bare(StartDir, Config, Task) when is_atom(Task) ->
    run_task(Task, StartDir, Config).


main() ->
    Args = init:get_plain_arguments(),
    main(Args).


main(Args) ->
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
do_build(Options, [Target | Rest]) ->
    do_task(list_to_atom(Target),
	    find_start_dir(Rest),
	    setup_config_overrides(Options, Rest));
do_build(Options, []) ->
    do_build(Options, ["build"]).

usage() ->
    usage(option_spec_list()).

usage(OptSpecList) ->
    getopt:usage(OptSpecList, "", "[var1=val1 ...] [command1 ...]",
                 [{"var=value", "Variables that will affect the compilation (e.g. debug=1)"},
                  {"command",   "Commands that will be executed by erlb (e.g. compile)"}]).

option_spec_list() ->
    [{verbose, $v, "verbose", {boolean, false},
      "Be verbose about what gets done"},
     {release, $r, "release", string, "the release to build"},
     {project, $p, "project", string, "the name of the project"},
     {version, $n, "nversion", string, "the version of the project"}].



%% @doc
%%  Allows sinan to be easily started from the shell. This is a
%%  helper function thats mostly just useful in development.
%% @end
-spec start() -> ok.
start() ->
    application:start(tools),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(edoc),
    application:start(sasl),
    application:start(ibrowse),
    application:start(eunit),
    application:start(ktuo),
    application:start(ewlib),
    application:start(ewrepo),
    application:start(gs),
    application:start(hipe),
    application:start(xmerl),
    application:start(mnesia),
    application:start(dialyzer),
    application:start(sgte),
    application:start(parsetools),
    application:start(asn1),
    application:start(getopt),
    application:start(sinan).


%% @doc
%% run the task including all task dependencies
%% @end
-spec run_task(task_name(), string(), sin_config:config()) -> ok.
run_task(Task, ProjectDir, BuildConfig) ->
    try
       Tasks = sin_task:get_task_list(Task),
       case sin_hooks:get_hooks_function(ProjectDir) of
	   no_hooks ->
	       lists:foldl(fun(TaskDesc, NewConfig) ->
				   ewl_talk:say("starting: ~p",
						TaskDesc#task.name),
				   NewNewConfig =
				       (TaskDesc#task.task_impl):do_task(NewConfig),
				   NewNewConfig
			   end, BuildConfig, Tasks);
	   HooksFun ->
	       lists:foldl(fun(TaskDesc, NewConfig) ->
				   ewl_talk:say("starting: ~p", TaskDesc#task.name),
				   HooksFun(pre, Task, NewConfig),
				   NewNewConfig = (TaskDesc#task.task_impl):do_task(NewConfig),
				   HooksFun(post, Task, NewNewConfig),
				NewNewConfig
			   end, BuildConfig, Tasks)
       end
    catch
	throw:{task_not_found, Task} ->
	    sin_talk:say("Unknown task ~p", [Task])
    end.


%% @doc
%%  parse the start dir out of the args passed in.
%% @end
-spec find_start_dir(args()) -> string().
find_start_dir({obj, Data}) ->
    find_start_dir(Data);
find_start_dir(Data) ->
    case lists:keysearch("build", 1, Data) of
         {value, {"build", {obj, Data2}}} ->
            case lists:keysearch("start_dir", 1,  Data2) of
                {value, {"start_dir", StartDir}} ->
                    StartDir;
                _ ->
		    {ok, Dir} = file:get_cwd(),
		    Dir
            end;
        _ ->
	    {ok, Dir} = file:get_cwd(),
	    Dir
    end.

%% @doc
%% Setup all configuration overrides from the command line
%% @end
-spec setup_config_overrides(Options::term(), term()) ->
    sin_config:config().
setup_config_overrides(Options, Args) ->
    push_values_if_exist(sin_config:parse_args(Args, sin_config:new()),
			 Options,
			 [{release, "-r"},
			  {project, "project.name"},
			  {version, "project.vsn"}]).


%% @doc
%% This pushes the values given in the args (if they exist) into the override
%% config under the name specified in key.
%%
%% For example the name/key mapping expects an atom for the name that matches
%% the atom specified in the release and a string as the key. The string is
%% used as the key to push into the config
%% @end
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

