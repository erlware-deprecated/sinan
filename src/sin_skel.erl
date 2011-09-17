%% -*- mode: Erlang; fill-column: 80; comment-column: 70; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Provides utitlities to generate an polar complient otp/erlang
%%%  project
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_skel).

%%% API
-export([application/3,
         supervisor/3,
         app_info/4,
         edoc_overview/3,
         build_config/2,
         sysconfig/2]).

-export_type([env/0]).

%%============================================================================
%% Types
%%============================================================================

-type env() :: [{Key::atom(), Value::term()}].

%%====================================================================
%% API
%%====================================================================

%% @doc Writes out the application file
-spec application(env(), FileName::string(), AppName::string()) -> ok.
application(Env, FileName, AppName) ->
    write_template("application", [{app_name, AppName} |  Env], FileName).

%% @doc Writes out a generic supervisor to the filename provided.
-spec supervisor(env(), FileName::string(), AppName::string()) -> ok.
supervisor(Env, FileName, AppName) ->
    write_template("supervisor", [{app_name, AppName} | Env], FileName).

%% @doc Writes out a generic application to the filename provided.
-spec app_info(env(), FileName::string(), AppName::string(), AppVsn::string()) -> ok.
app_info(Env, FileName, AppName, AppVsn) ->
    write_template("dotapp", [{app_name, AppName},
                              {app_vsn, AppVsn}| Env], FileName).

%% @doc Writes out a overview.edoc to the filename provided.
-spec edoc_overview(env(), FileName::string(), AppName::string()) -> ok.
edoc_overview(Env, FileName, AppName) ->
    write_template("overview", [{app_name, AppName} | Env], FileName).

%% @doc Writes the build_config to the specified library with the specified
%% repo.
-spec build_config(env(), FileName::string()) -> ok.
build_config(Env, FileName) ->
    write_template("buildcfg", Env, FileName).

%% @doc Writes the sys config out to the specifiecd place
-spec sysconfig(env(), Filename::string()) -> ok.
sysconfig(Env, FileName) ->
    write_template("sysconfig", Env, FileName).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Write the template with the Env data to FileName.
-spec write_template(TemplateName::string(), env(), FileName::string()) -> ok.
write_template(TemplateName, Env, FileName) ->
    Template = compile_template(TemplateName),
    Out = sgte:render_str(Template, Env),
    file:write_file(FileName, list_to_binary(Out)).

%% @doc Compile a template for the specified type.
-spec compile_template(TemplateName::string()) -> Template::term().
compile_template(TemplateName) ->
    PrivDir = code:priv_dir(sinan),
    TemplateFile = filename:join([PrivDir, TemplateName]),
    case sgte:compile_file(TemplateFile) of
        {error, _} ->
            exit(unable_to_compile_template_file);
        {ok, Template} ->
            Template
    end.

%%%====================================================================
%%% tests
%%%====================================================================



