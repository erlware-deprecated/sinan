%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%%-------------------------------------------------------------------
%%% Copyright (c) 2007-2010 Erlware
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
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Provides utitlities to generate an polar complient otp/erlang
%%%  project
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%%-------------------------------------------------------------------
-module(sin_gen).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, gen/1]).

-define(TASK, gen).
-define(DEPS, []).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec () -> ok
%%
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Generates a buildable default project layout ",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = []},
    eta_task:register_task(TaskDesc).


%%--------------------------------------------------------------------
%% @doc
%%  Do the task defined in this module.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    gen(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Kicks off the generation process. Handles the individual steps
%%  in new project generation.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
gen(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK),
    {{Year, _, _}, {_, _, _}} = erlang:localtime(),
    get_user_information(BuildRef, [{year, integer_to_list(Year)}]),
    eta_event:task_stop(BuildRef, ?TASK).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Prints out a nice error message if everything was ok.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
all_done(BuildRef) ->
    eta_event:task_event(BuildRef, ?TASK, info,
                         "Project was created, you should be good to go!").

%%--------------------------------------------------------------------
%% @doc
%%  Builds the build config dir in the root of the project.
%% @spec (BuildRef, Env) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_build_config(BuildRef, Env) ->
    ProjectDir = get_env(project_dir, Env),
    ProjectName = get_env(project_name, Env),
    ConfName = filename:join([ProjectDir, "_build.cfg"]),
    ErlwareFile = filename:join([ProjectDir,  "bin", "erlware_release_start_helper"]),
    BinFile = filename:join([ProjectDir,  "bin", ProjectName]),
    ConfigFile = filename:join([ProjectDir,  "config", "sys.config"]),
    sin_skel:build_config(Env, ConfName),
    sin_skel:bin(Env, BinFile),
    sin_skel:bin_support(Env, ErlwareFile),
    sin_skel:sysconfig(Env, ConfigFile),
    all_done(BuildRef).


%%--------------------------------------------------------------------
%% @doc
%%  Given the project directory and a list of application names, builds
%%  out the application directory structure.
%% @spec (ProjDir, Apps) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_applications(BuildRef, Env) ->
    Apps = get_env(apps, Env),
    build_out_applications(BuildRef, Env, Apps).

build_out_applications(BuildRef, Env, [AppName | T]) ->
    ProjDir = get_env(project_dir, Env),
    AppDir = filename:join([ProjDir, "lib", AppName]),
    case filelib:is_dir(AppDir) of
        false ->
            make_dir(BuildRef, AppDir),
            make_dir(BuildRef, filename:join(AppDir, "doc")),
            make_dir(BuildRef, filename:join(AppDir, "ebin")),
            make_dir(BuildRef, filename:join(AppDir, "include")),
            AppSrc = make_dir(BuildRef, filename:join(AppDir, "src")),
            build_out_otp(BuildRef, Env, AppSrc, AppName),
            build_out_applications(BuildRef, Env, T);
       true ->
            ok
    end;
build_out_applications(BuildRef, Env, []) ->
    build_out_build_config(BuildRef, Env).

%%--------------------------------------------------------------------
%% @doc
%% Build out the top level otp parts of the application.
%% @spec (BuildRef, Env, AppSrc, App) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_otp(BuildRef, Env, AppSrc, App) ->
    FileName = filename:join(AppSrc, App ++ "_app.erl"),
    case filelib:is_file(FileName) of
        true ->
            build_out_super(BuildRef, Env, AppSrc, App);
        false ->
            sin_skel:application(Env, FileName, App),
            build_out_super(BuildRef, Env, AppSrc, App)
    end.


%%--------------------------------------------------------------------
%% @doc
%% Builds out the supervisor for the app.
%% @spec (BuildRef, Env, AppSrc, App) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_super(BuildRef, Env, AppSrc, App) ->
    FileName = filename:join(AppSrc, App ++ "_sup.erl"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            sin_skel:supervisor(Env, FileName, App),
            build_out_app_src(BuildRef, Env, App)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Builds out the app descriptor for the app.
%% @spec (BuildRef, Env, App) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_app_src(BuildRef, Env, App) ->
    ProjDir = get_env(project_dir, Env),
    AppEbin = filename:join([ProjDir, "lib", App, "ebin"]),
    FileName = filename:join(AppEbin, App ++ ".app"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            sin_skel:app_info(Env, FileName, App),
	    build_out_app_doc(BuildRef, Env, App)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Builds out the overview.edoc for the app.
%% @spec (BuildRef, Env, App) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_app_doc(_BuildRef, Env, App) ->
    ProjDir = get_env(project_dir, Env),
    AppEbin = filename:join([ProjDir, "lib", App, "doc"]),
    FileName = filename:join(AppEbin, "overview.edoc"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            sin_skel:edoc_overview(Env, FileName, App)
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Given the project directory builds out the various directories
%%  required for an application.
%% @spec (BuildRef, Env) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_skeleton(BuildRef, Env) ->
    ProjDir = get_env(project_dir, Env),
    make_dir(BuildRef, filename:join(ProjDir, "doc")),
    make_dir(BuildRef, filename:join(ProjDir, "bin")),
    make_dir(BuildRef, filename:join(ProjDir, "config")),
    build_out_applications(BuildRef, Env).

%%--------------------------------------------------------------------
%% @doc
%% Build out the project directory structure
%% @spec (BuildRef, Env) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_project(BuildRef, Env) ->
    ProjDir = get_env(project_dir, Env),
    make_dir(BuildRef, ProjDir),
    build_out_skeleton(BuildRef, Env).


%%--------------------------------------------------------------------
%% @doc
%%  Queries the user for a list of application names. The user
%% can choose to skip this part.
%% @spec (BuildRef, Env) -> AppNames
%% @end
%%--------------------------------------------------------------------
get_application_names(BuildRef, Env) ->
    Env2 = [{apps,
             sin_build_config:get_value(BuildRef,
                                        "tasks.gen.apps")} | Env],
    build_out_project(BuildRef, Env2).

%%--------------------------------------------------------------------
%% @doc
%% Queries the user for the name of this project
%% @spec (BuildRef, Env) -> Env2
%% @end
%%--------------------------------------------------------------------
get_new_project_name(BuildRef, Env) ->
    CDir = sin_build_config:get_value(BuildRef, "build.start_dir"),
    Name = sin_build_config:get_value(BuildRef,
                                      "tasks.gen.project_info.project_name"),
    Dir = filename:join(CDir, Name),
    Version =
        sin_build_config:get_value(BuildRef,
                                   "tasks.gen.project_info.project_version"),
    {ok, ErtsVersion} = application:get_env(sinan, erts_version),
    Env2 = [{project_version, Version},
            {project_name, Name},
            {project_dir, Dir},
	    {erts_version, ErtsVersion} | Env],
    get_application_names(BuildRef, Env2).



%%--------------------------------------------------------------------
%% @doc
%% Queries the user for his name and email address
%% @spec (BuildRef, Env) -> Env
%% @end
%%--------------------------------------------------------------------
get_user_information(BuildRef, Env) ->
    Name = sin_build_config:get_value(BuildRef,
                                        "tasks.gen.user_info.username"),
    Address = sin_build_config:get_value(BuildRef,
                                           "tasks.gen.user_info.email_address"),
    CopyHolder =
        sin_build_config:get_value(BuildRef,
                                     "tasks.gen.user_info.copyright_holder"),
    Env2 = [{username, Name}, {email_address, Address},
            {copyright_holder, CopyHolder} | Env],
    get_new_project_name(BuildRef, Env2).


%%--------------------------------------------------------------------
%% @doc
%% Helper function that makes the specified directory and all parent
%% directories.
%% @spec (BuildRef, DirName) -> ok
%% @end
%%--------------------------------------------------------------------
make_dir(BuildRef, DirName) ->
    filelib:ensure_dir(DirName),
    is_made(BuildRef, DirName, file:make_dir(DirName)),
    DirName.

%%--------------------------------------------------------------------
%% @doc
%% Helper function that makes sure a directory is made by testing
%% the output of file:make_dir().
%% @spec (BuildRef, DirName, Output) -> ok
%% @end
%%--------------------------------------------------------------------
is_made(BuildRef, DirName, {error, eexist})->
    eta_event:task_event(BuildRef, ?TASK, info, {"~s exists ok.", [DirName]});
is_made(BuildRef, DirName, ok) ->
    eta_event:task_event(BuildRef, ?TASK, info, {"~s created ok.", [DirName]}).

%%--------------------------------------------------------------------
%% @doc
%%  Get the value from the environment.
%% @spec get_env(Name, Env) -> Value
%% @end
%%--------------------------------------------------------------------
get_env(Name, Env) ->
    {value, {Name, Value}} = lists:keysearch(Name, 1, Env),
    Value.
