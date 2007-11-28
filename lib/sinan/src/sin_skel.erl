%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%% Provides skeletons for application generation
%%% @end
%%%-------------------------------------------------------------------
-module(sin_skel).

%%% API
-export([application/3, supervisor/3,
         app_info/3, build_config/2]).

%-include("eunit.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec application(DirName, UserMail, CopyHolder, AppName) -> ok.
%% @doc
%%  Writes out the application file
%% @end
%%--------------------------------------------------------------------
application(Env, FileName, AppName) ->
    write_template("application", [{app_name, AppName} | Env], FileName).

%%--------------------------------------------------------------------
%% @spec supervisor(Env, FileName, AppName) -> ok.
%% @doc
%%    Writes out a generic supervisor to the filename provided.
%% @end
%%--------------------------------------------------------------------
supervisor(Env, FileName, AppName) ->
    write_template("supervisor", [{app_name, AppName} | Env], FileName).


%%--------------------------------------------------------------------
%% @spec app_info(Env, FileName, AppName) -> ok.
%% @doc
%%    Writes out a generic application to the filename provided.
%% @end
%%--------------------------------------------------------------------
app_info(Env, FileName, AppName) ->
    write_template("dotapp", [{app_name, AppName} | Env], FileName).

%%--------------------------------------------------------------------
%% @spec build_config(Env, FileName) -> ok.
%% @doc
%%  Writes the build_config to the specified library with
%%  the specified repo.
%% @end
%%--------------------------------------------------------------------
build_config(Env, FileName) ->
    write_template("buildcfg", Env, FileName).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec write_template(Type, Env, FileName) -> ok.
%%
%% @doc
%%  Write the template with the Env data to FileName.
%% @end
%%--------------------------------------------------------------------
write_template(Type, Env, FileName) ->
    Template = compile_template(Type),
    Out = sgte:render_str(Template, Env),
    file:write_file(FileName, list_to_binary(Out)).

%%--------------------------------------------------------------------
%% @spec compile_template(Type) -> Template.
%%
%% @doc
%%  Compile a template for the specified type.
%% @end
%%--------------------------------------------------------------------
compile_template(Type) ->
    PrivDir = code:priv_dir(projgen),
    TemplateFile = filename:join([PrivDir, Type]),
    case sgte:compile({file, TemplateFile}) of
        {error, _} ->
            exit(unable_to_compile_template_file);
        {ok, Template} ->
            Template
    end.

%%%====================================================================
%%% tests
%%%====================================================================



