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
-module(sin_task_gen).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1, gen/1]).

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
description() ->
    Desc = "Generates a buildable default project layout ",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = true,
	  deps = ?DEPS,
	  desc = Desc,
	  opts = []}.


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
    {{Year, _, _}, {_, _, _}} = erlang:localtime(),
    get_user_information([{year, integer_to_list(Year)}]),
    BuildRef.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Queries the user for his name and email address
%% @spec (BuildRef, Env) -> Env
%% @end
%%--------------------------------------------------------------------
get_user_information(Env) ->
    ewl_talk:say("Please specify your name "),
    Name = ewl_talk:ask("your name"),
    ewl_talk:say("Please specify your email address "),
    Address = ewl_talk:ask("your email"),
    ewl_talk:say("Please specify the copyright holder "),
    CopyHolder = ewl_talk:ask_default("copyright holder", Name),
    Env2 = [{username, Name}, {email_address, Address},
           {copyright_holder, CopyHolder} | Env],
    get_new_project_name(Env2).


%%--------------------------------------------------------------------
%% @doc
%% Queries the user for the name of this project
%% @spec (BuildRef, Env) -> Env2
%% @end
%%--------------------------------------------------------------------
get_new_project_name(Env) ->
    {ok, CDir} = file:get_cwd(),
    ewl_talk:say("Please specify name of your project"),
    Name = ewl_talk:ask("project name"),
    Dir = filename:join(CDir, Name),
    ewl_talk:say("Please specify version of your project"),
    Version = ewl_talk:ask("project version"),
    ErtsVersion = ewl_talk:ask_default("Please specify the erst version", erlang:system_info(version)),
    Env2 = [{project_version, Version},
            {project_name, Name},
            {project_dir, Dir},
	    {erts_version, ErtsVersion} | Env],
    get_application_names(Env2).


%%--------------------------------------------------------------------
%% @spec get_application_names() -> AppNames.
%% @doc
%%  Queries the user for a list of application names. The user
%% can choose to skip this part.
%% @end
%%--------------------------------------------------------------------
get_application_names(Env) ->
    ewl_talk:say("Please specify the names of the OTP apps"
		 " that belong to this project. One application to a"
		 " line. Finish with a blank line."),
    get_application_names(Env, ewl_talk:ask("app"), []).

get_application_names(Env, [], Acc) ->
    Env2 = [{apps, Acc} | Env],
    build_out_project(Env2);
get_application_names(Env, App, Acc) ->
    get_application_names(Env, ewl_talk:ask_default("app", ""), [App | Acc]).

%%--------------------------------------------------------------------
%% @doc
%% Build out the project directory structure
%% @spec (BuildRef, Env) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_project(Env) ->
    ProjDir = get_env(project_dir, Env),
    make_dir(ProjDir),
    build_out_skeleton(Env).

%%--------------------------------------------------------------------
%% @doc
%%  Given the project directory builds out the various directories
%%  required for an application.
%% @spec (BuildRef, Env) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_skeleton(Env) ->
    ProjDir = get_env(project_dir, Env),
    make_dir(filename:join(ProjDir, "doc")),
    make_dir(filename:join(ProjDir, "bin")),
    make_dir(filename:join(ProjDir, "config")),
    build_out_applications(Env).

%%--------------------------------------------------------------------
%% @doc
%%  Given the project directory and a list of application names, builds
%%  out the application directory structure.
%% @spec (ProjDir, Apps) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_applications(Env) ->
    Apps = get_env(apps, Env),
    build_out_applications(Env, Apps).

build_out_applications(Env, [AppName | T]) ->
    ProjDir = get_env(project_dir, Env),
    AppDir = filename:join([ProjDir, "lib", AppName]),
    case filelib:is_dir(AppDir) of
        false ->
            make_dir(AppDir),
            make_dir(filename:join(AppDir, "doc")),
            make_dir(filename:join(AppDir, "ebin")),
            make_dir(filename:join(AppDir, "include")),
            AppSrc = make_dir(filename:join(AppDir, "src")),
            build_out_otp(Env, AppSrc, AppName),
            build_out_applications(Env, T);
       true ->
            ok
    end;
build_out_applications(Env, []) ->
    build_out_build_config(Env).

%%--------------------------------------------------------------------
%% @doc
%% Builds out the supervisor for the app.
%% @spec (BuildRef, Env, AppSrc, App) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_super(Env, AppSrc, App) ->
    FileName = filename:join(AppSrc, App ++ "_sup.erl"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            sin_skel:supervisor(Env, FileName, App),
            build_out_app_src(Env, App)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Builds out the app descriptor for the app.
%% @spec (BuildRef, Env, App) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_app_src(Env, App) ->
    ProjDir = get_env(project_dir, Env),
    AppEbin = filename:join([ProjDir, "lib", App, "ebin"]),
    FileName = filename:join(AppEbin, App ++ ".app"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            sin_skel:app_info(Env, FileName, App),
	    build_out_app_doc(Env, App)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Builds out the overview.edoc for the app.
%% @spec (BuildRef, Env, App) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_app_doc(Env, App) ->
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
%% Build out the top level otp parts of the application.
%% @spec (BuildRef, Env, AppSrc, App) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_otp(Env, AppSrc, App) ->
    FileName = filename:join(AppSrc, App ++ "_app.erl"),
    case filelib:is_file(FileName) of
        true ->
            build_out_super(Env, AppSrc, App);
        false ->
            sin_skel:application(Env, FileName, App),
            build_out_super(Env, AppSrc, App)
    end.


%%--------------------------------------------------------------------
%% @doc
%%  Builds the build config dir in the root of the project.
%% @spec (BuildRef, Env) -> ok
%% @end
%%--------------------------------------------------------------------
build_out_build_config(Env) ->
    ProjectDir = get_env(project_dir, Env),
    ProjectName = get_env(project_name, Env),
    ConfName = filename:join([ProjectDir, "sinan.cfg"]),
    ErlwareFile = filename:join([ProjectDir,  "bin", "erlware_release_start_helper"]),
    BinFile = filename:join([ProjectDir,  "bin", ProjectName]),
    ConfigFile = filename:join([ProjectDir,  "config", "sys.config"]),
    sin_skel:build_config(Env, ConfName),
    sin_skel:bin(Env, BinFile),
    sin_skel:bin_support(Env, ErlwareFile),
    sin_skel:sysconfig(Env, ConfigFile),
    all_done().

%%--------------------------------------------------------------------
%% @doc
%%  Prints out a nice error message if everything was ok.
%% @end
%%--------------------------------------------------------------------
all_done() ->
    ewl_talk:say("Project was created, you should be good to go!").


%%--------------------------------------------------------------------
%% @doc
%% Helper function that makes the specified directory and all parent
%% directories.
%% @spec (BuildRef, DirName) -> ok
%% @end
%%--------------------------------------------------------------------
make_dir(DirName) ->
    filelib:ensure_dir(DirName),
    is_made(DirName, file:make_dir(DirName)),
    DirName.

%%--------------------------------------------------------------------
%% @doc
%% Helper function that makes sure a directory is made by testing
%% the output of file:make_dir().
%% @spec (BuildRef, DirName, Output) -> ok
%% @end
%%--------------------------------------------------------------------
is_made(DirName, {error, eexist})->
    ewl_talk:say("~s exists ok.", [DirName]);
is_made(DirName, ok) ->
   ewl_talk:say("~s created ok.", [DirName]).

%%--------------------------------------------------------------------
%% @doc
%%  Get the value from the environment.
%% @spec get_env(Name, Env) -> Value
%% @end
%%--------------------------------------------------------------------
get_env(Name, Env) ->
    {value, {Name, Value}} = lists:keysearch(Name, 1, Env),
    Value.
