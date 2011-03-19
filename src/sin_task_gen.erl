%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Provides utitlities to generate an polar complient otp/erlang
%%%  project
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_gen).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0,
	 do_task/1,
	 gen/1]).

-define(TASK, gen).
-define(DEPS, []).

%%============================================================================
%% Types
%%============================================================================

-type env() :: [{Key::atom(), Value::term()}].

%%============================================================================
%% API
%%============================================================================

%% @doc describe this task to the system.
-spec description() -> ok.
description() ->
    Desc = "Generates a buildable default project layout ",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = true,
	  deps = ?DEPS,
	  example = "gen",
	  short_desc = Desc,
	  desc = Desc,
	  opts = []}.

%% @doc Do the generation of an erlang project
-spec do_task(sin_config:config()) -> ok.
do_task(Config) ->
    gen(Config).

%% @doc Kicks off the generation process. Handles the individual steps in new
%%  project generation.
-spec gen(sin_config:config()) -> ok.
gen(Config) ->
    {{Year, _, _}, {_, _, _}} = erlang:localtime(),
    get_user_information(Config, [{year, integer_to_list(Year)}]).

%%============================================================================
%% Internal functions
%%============================================================================

%% @doc Queries the user for various information that we need to generate the
%% project
-spec get_user_information(sin_config:config(), env()) -> ok.
get_user_information(Config, Env) ->
    ewl_talk:say("Please specify your name "),
    Name = ewl_talk:ask("your name"),
    ewl_talk:say("Please specify your email address "),
    Address = ewl_talk:ask("your email"),
    ewl_talk:say("Please specify the copyright holder "),
    CopyHolder = ewl_talk:ask_default("copyright holder", Name),
    Env2 = [{username, Name}, {email_address, Address},
           {copyright_holder, CopyHolder} | Env],
    get_project_information(Config, Env2).

%% @doc Get the project name, either from the command line or from the user.
-spec get_project_name(sin_config:config()) -> string().
get_project_name(Config) ->
    case sin_config:get_value(Config, "command_line.arg") of
	undefined ->
	    ewl_talk:say("Please specify name of your project"),
	    ewl_talk:ask("project name");
	Value ->
	    Value
    end.

%% @doc Queries the user for the name of this project
-spec get_project_information(sin_config:config(), env()) -> ok.
get_project_information(Config, Env) ->
    {ok, CDir} = file:get_cwd(),
    Name = get_project_name(Config),
    Dir = filename:join(CDir, Name),
    ewl_talk:say("Please specify version of your project"),
    Version = ewl_talk:ask("project version"),
    ErtsVersion = ewl_talk:ask_default("Please specify the ERTS version",
				       erlang:system_info(version)),
    Env2 = [{project_version, Version},
            {project_name, Name},
            {project_dir, Dir},
	    {erts_version, ErtsVersion} | Env],

    case ewl_talk:ask_default("Is this a single application project",
			      boolean, "n") of
	false ->
	    get_application_names([{single_app_project, false} | Env2]);
	true ->
	    build_out_skeleton([{single_app_project, true} | Env2])
    end,
    Config.

%% @doc Queries the user for a list of application names. The user can choose
%% to skip this part.
-spec get_application_names(env()) -> ok.
get_application_names(Env) ->
    ewl_talk:say("Please specify the names of the OTP apps"
		 " that will be developed under this project. One "
		 "application to a line. Finish with a blank line."),

    get_application_names(Env, ewl_talk:ask("app"), []).

-spec get_application_names(env(), [string()], []) -> [Names::string()].
get_application_names(Env, no_data, Acc) ->
    Env2 = [{apps, Acc} | Env],
    build_out_project(Env2);
get_application_names(Env, App, Acc) ->
    NewAcc = case lists:member(App, Acc) of
		 true ->
		     ewl_talk:say("App ~s is already specified", [App]),
		     Acc;
		 false ->
		     [App | Acc]
	     end,
    get_application_names(Env, ewl_talk:ask_default("app", ""), NewAcc).

%% @doc Build out the project directory structure
-spec build_out_project(env()) -> ok.
build_out_project(Env) ->
    ProjDir = get_env(project_dir, Env),
    make_dir(ProjDir),
    build_out_skeleton(Env).

%% @doc Given the project directory builds out the various directories required
%% for an application.
-spec build_out_skeleton(env()) -> ok.
build_out_skeleton(Env) ->
    ProjDir = get_env(project_dir, Env),
    make_dir(filename:join(ProjDir, "doc")),
    make_dir(filename:join(ProjDir, "bin")),
    make_dir(filename:join(ProjDir, "config")),
    build_out_applications(Env).

%% @doc Given the project directory and a list of application names, builds out
%% the application directory structure.
-spec build_out_applications(env()) -> ok.
build_out_applications(Env) ->
    case get_env(single_app_project, Env) of
	false ->
	    Apps = get_env(apps, Env),
	    build_out_applications(Env, Apps);
	true ->
	    ProjDir = get_env(project_dir, Env),
	    ProjVsn = get_env(project_version, Env),
	    ProjName = get_env(project_name, Env),
	    build_out_application(Env, ProjDir, ProjName, ProjVsn),
	    build_out_build_config(Env)
    end.

-spec build_out_applications(env(), AppNames::[string()]) -> ok.
build_out_applications(Env, [AppName | T]) ->
    ProjDir = get_env(project_dir, Env),
    AppDir = filename:join([ProjDir, "lib", AppName]),
    build_out_application(Env, AppDir, AppName, "0.1.0"),
    build_out_applications(Env, T);
build_out_applications(Env, []) ->
    build_out_build_config(Env).

%% @doc build out all the things required by an application
-spec build_out_application(env(), AppDir::string(),
			    AppName::string(), AppVsn::string()) -> ok.
build_out_application(Env, AppDir, AppName, AppVsn) ->
    EbinDir = make_dir(filename:join(AppDir, "ebin")),
    AppSrc = make_dir(filename:join(AppDir, "src")),
    make_dir(filename:join(AppDir, "include")),
    make_dir(filename:join(AppDir, "doc")),
    build_out_super(Env, AppSrc, AppName),
    build_out_app_src(Env, EbinDir, AppName, AppVsn),
    build_out_otp(Env, AppSrc, AppName),
    build_out_app_doc(Env, EbinDir, AppName).

%% @doc Builds out the supervisor for the app.
-spec build_out_super(env(), AppSrc::string(), AppName::string()) -> ok.
build_out_super(Env, AppSrc, AppName) ->
    FileName = filename:join(AppSrc, AppName ++ "_sup.erl"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            sin_skel:supervisor(Env, FileName, AppName)
    end.

%% @doc Builds out the app descriptor for the app.
-spec build_out_app_src(env(), EbinDir::string(),
			AppName::string(), AppVsn::string()) -> ok.
build_out_app_src(Env, EbinDir, AppName, AppVsn) ->
    FileName = filename:join(EbinDir, AppName ++ ".app"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
	    sin_skel:app_info(Env, FileName, AppName, AppVsn)
    end.

%% @doc Builds out the overview.edoc for the app.
-spec build_out_app_doc(env(), EbinDir::string(), AppName::string()) -> ok.
build_out_app_doc(Env, EbinDir, AppName) ->
    FileName = filename:join(EbinDir, "overview.edoc"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            sin_skel:edoc_overview(Env, FileName, AppName)
    end.

%% @doc Build out the top level otp parts of the application.
-spec build_out_otp(env(), AppSrc::string(), AppName::string()) -> ok.
build_out_otp(Env, AppSrc, AppName) ->
    FileName = filename:join(AppSrc, AppName ++ "_app.erl"),
    case filelib:is_file(FileName) of
        true ->
	    ok;
        false ->
            sin_skel:application(Env, FileName, AppName)
    end.

%% @doc Builds the build config dir in the root of the project.
build_out_build_config(Env) ->
    case ewl_talk:ask_default("Would you like a build config?",
			      boolean, "y") of
	true ->
	    ProjectDir = get_env(project_dir, Env),
	    ProjectName = get_env(project_name, Env),
	    ConfName = filename:join([ProjectDir, "sinan.cfg"]),
	    ErlwareFile =
		filename:join([ProjectDir,  "bin",
			       "erlware_release_start_helper"]),
	    BinFile = filename:join([ProjectDir,  "bin", ProjectName]),
	    ConfigFile = filename:join([ProjectDir,  "config", "sys.config"]),
	    sin_skel:build_config(Env, ConfName),
	    sin_skel:bin(Env, BinFile),
	    sin_skel:bin_support(Env, ErlwareFile),
	    sin_skel:sysconfig(Env, ConfigFile);
	false ->
	    ok
    end,
    all_done().

%% @doc Prints out a nice error message if everything was ok.
-spec all_done() -> ok.
all_done() ->
    ewl_talk:say("Project was created, you should be good to go!").

%% @doc Helper function that makes the specified directory and all parent
%% directories.
-spec make_dir(DirName::string()) -> ok.
make_dir(DirName) ->
    filelib:ensure_dir(DirName),
    is_made(DirName, file:make_dir(DirName)),
    DirName.

%% @doc Helper function that makes sure a directory is made by testing the
%% output of file:make_dir().
is_made(DirName, ok) ->
   ewl_talk:say("~s created ok.", [DirName]).

%% @doc Get the value from the environment.
-spec get_env(Key::string(), env()) -> ok.
get_env(Name, Env) ->
    {value, {Name, Value}} = lists:keysearch(Name, 1, Env),
    Value.
