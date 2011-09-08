%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Provides utitlities to generate a complient otp/erlang
%%%  project when arguments are correctly passed to it.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_gen).

%% API
-export([gen/1]).

%%============================================================================
%% Types
%%============================================================================

-type env() :: [{Key::atom(), Value::term()}].

%%============================================================================
%% API
%%============================================================================

%% @doc Kicks off the generation process. Handles the individual steps in new
%%  project generation.
-spec gen(sin_config:config()) -> ok.
gen(Env) ->
    build_out_skeleton(Env).

%%============================================================================
%% Internal functions
%%============================================================================
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
    case get_env(wants_build_config, Env) of
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
    end.

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
   ewl_talk:say("~s created ok.", [DirName]);
is_made(DirName, {error, eexist}) ->
   ewl_talk:say("~s created ok.", [DirName]).

%% @doc Get the value from the environment.
-spec get_env(Key::string(), env()) -> ok.
get_env(Name, Env) ->
    {value, {Name, Value}} = lists:keysearch(Name, 1, Env),
    Value.
