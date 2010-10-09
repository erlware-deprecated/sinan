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
-export([main/1,
	 build/1,
         analyze/1,
         doc/1,
         shell/1,
         gen/1,
         clean/1,
         help/1,
         version/1,
         depends/1,
         test/1,
         release/1,
         dist/1,
         do_task/2,
         start/0]).

-export_type([args/0,
	     task_name/0]).

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
%%  Run the build with the specified args.
%% {@link sin_task_build}
%% @end
-spec build(args()) -> ok.
build(Args) ->
    do_task(build, Args).

%% @doc
%%  Run the analyze task.
%%
%% {@link sin_task_analyze}
%% @end
-spec analyze(args()) -> ok.
analyze(Args) ->
    do_task(analyze, Args).

%% @doc
%%  run the doc task.
%% {@link sin_task_doc}
%% @end
-spec doc(args()) -> ok.
doc(Args) ->
    do_task(doc, Args).


%% @doc
%%  run the shell task.
%% {@link sin_task_shell}
%% @end
-spec shell(args()) -> ok.
shell(Args) ->
   do_task(shell, Args).

%% @doc
%%  Run the gen task.
%% {@link sin_task_gen}
%% @end
-spec gen(args()) -> ok.
gen(Args) ->
    do_task_bare(gen, Args).


%% @doc
%%  run the clean task.
%% {@link sin_task_clean}
%% @end
-spec clean(args()) -> ok.
clean(Args) ->
    do_task(clean, Args).


%% @doc
%%  run the help task.
%% {@link sin_task_help}
%% @end
-spec help(args()) -> ok.
help(Args) ->
    do_task_bare(help, Args).

%% @doc
%%  run the version task.
%% {@link sin_task_version}
%% @end
-spec version(args()) -> ok.
version(Args) ->
    do_task_bare(version, Args).

%% @doc
%%  run the depends task.
%% {@link sin_task_depends}
%% @end
-spec depends(args()) -> ok.
depends(Args) ->
    do_task(depends, Args).


%% @doc
%%  run the test task.
%% {@link sin_task_test}
%% @end
-spec test(args()) -> ok.
test(Args) ->
    do_task(test, Args).

%% @doc
%%  run the release task. {@link sin_release_builder}
%% @end
-spec release(args()) -> ok.
release(Args) ->
    do_task(release, Args).

%% @doc
%%  run the dist task. {@link sin_dist_builder}
%% @end
-spec dist(args()) -> ok.
dist(Args) ->
    do_task(dist, Args).

%% @doc
%%  run the specified task
%% @end
-spec do_task(task_name(), args()) -> ok.
do_task(Task, Args) when is_atom(Task) ->
    StartDir = find_start_dir(Args),
    try
        ProjectRoot = sin_utils:find_project_root(StartDir),
        Seed = sin_build_config:get_seed(ProjectRoot),
        BuildConfig = sin_build_config:new(ProjectRoot, Seed, Args),
	run_task(Task, ProjectRoot, BuildConfig)
    catch
        no_build_config ->
            sin_talk:say("No build config found.");
        {unable_to_create_canonical, {_, _,Desc}}  ->
	    sin_talk:say("Error discovering project layout: ~s", Desc);
	{sin_excep, Problem}  ->
	    sin_talk:say("build problem ~s", [Problem]);
	{sin_excep, _, {Description, EArgs}}  ->
	    sin_talk:say(Description, EArgs);
    	{sin_excep, _, Description}  ->
	    sin_talk:say("~s", [Description])
    end.

%% @doc
%%  run the specified task, without expecting a build config and
%% what not.
%% @end
-spec do_task_bare(task_name(), args()) -> ok.
do_task_bare(Task, Args) when is_atom(Task) ->
    StartDir = find_start_dir(Args),
    Config = sin_build_config:new(StartDir, Args),
    run_task(Task, StartDir, Config).


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
do_build(_Options, [Target]) ->
    sinan:do_task(list_to_atom(Target), []);
do_build(_Options, []) ->
    sinan:do_task(build, []).


usage() ->
    usage(option_spec_list()).

usage(OptSpecList) ->
    getopt:usage(OptSpecList, "", "[var1=val1 ...] [command1 ...]",
                 [{"var=value", "Variables that will affect the compilation (e.g. debug=1)"},
                  {"command",   "Commands that will be executed by erlb (e.g. compile)"}]).

option_spec_list() ->
    [{verbose, $v, "verbose", {boolean, false},
      "Be verbose about what gets done"}].



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
    application:start(sinan).


%%====================================================================
%% Internal functions
%%====================================================================
%% @doc
%% run the task including all task dependencies
%% @end
-spec run_task(task_name(), string(), sin_build_config:build_config()) -> ok.
run_task(Task, ProjectDir, BuildConfig) ->
    Tasks = sin_task:get_task_list(Task),
    case sin_hooks:get_hooks_function(ProjectDir) of
	no_hooks ->
	    lists:foldl(fun(TaskDesc, NewConfig) ->
				sin_talk:say("starting: ~p", TaskDesc#task.name),
				NewNewConfig = (TaskDesc#task.task_impl):do_task(NewConfig),
				NewNewConfig
			end, BuildConfig, Tasks);
	HooksFun ->
	    lists:foldl(fun(TaskDesc, NewConfig) ->
				sin_talk:say("starting: ~p", TaskDesc#task.name),
				HooksFun(pre, Task, NewConfig),
				NewNewConfig = (TaskDesc#task.task_impl):do_task(NewConfig),
				HooksFun(post, Task, NewNewConfig),
				NewNewConfig
			end, BuildConfig, Tasks)
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

