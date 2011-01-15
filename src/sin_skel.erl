%% -*- mode: Erlang; fill-column: 79; comment-column: 70; -*-
%%%---------------------------------------------------------------------------
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Provides utitlities to generate an polar complient otp/erlang
%%%  project
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%%---------------------------------------------------------------------------
-module(sin_skel).

%%% API
-export([application/3, supervisor/3,
         app_info/4, edoc_overview/3,
	 build_config/2,
	 sysconfig/2,
	 bin_support/2,
	 bin/2]).

-export_type([env/0]).

%%============================================================================
%% Types
%%============================================================================
-type env() :: [{Key::atom(), Value::term()}].

%%====================================================================
%% API
%%====================================================================
%% @doc
%%  Writes out the application file
%% @end
-spec application(env(), FileName::string(), AppName::string()) -> ok.
application(Env, FileName, AppName) ->
    write_template("application", [{app_name, AppName} |  Env], FileName).

%% @doc
%%    Writes out a generic supervisor to the filename provided.
%% @end
-spec supervisor(env(), FileName::string(), AppName::string()) -> ok.
supervisor(Env, FileName, AppName) ->
    write_template("supervisor", [{app_name, AppName} | Env], FileName).

%% @doc
%%    Writes out a generic application to the filename provided.
%% @end
-spec app_info(env(), FileName::string(), AppName::string(), AppVsn::string()) -> ok.
app_info(Env, FileName, AppName, AppVsn) ->
    write_template("dotapp", [{app_name, AppName},
			      {app_vsn, AppVsn}| Env], FileName).
%% @doc
%%    Writes out a overview.edoc to the filename provided.
%% @end
-spec edoc_overview(env(), FileName::string(), AppName::string()) -> ok.
edoc_overview(Env, FileName, AppName) ->
    write_template("overview", [{app_name, AppName} | Env], FileName).

%% @doc
%%  Writes the build_config to the specified library with
%%  the specified repo.
%% @end
-spec build_config(env(), FileName::string()) -> ok.
build_config(Env, FileName) ->
    write_template("buildcfg", Env, FileName).

%% @doc
%% Writes the bin file out to the specified library
%% @end
-spec bin(env(), Filename::string()) -> ok.
bin(Env, FileName) ->
    write_template("exe", Env, FileName).

%% @doc
%% Writes the bin support file
%% @end
-spec bin_support(env, Filename::string()) -> ok.
bin_support(Env, FileName) ->
    write_template("erlware_release_start_helper", Env, FileName).

%% @doc
%%  Writes the sys config out to the specifiecd place
%% @end
-spec sysconfig(env(), Filename::string()) -> ok.
sysconfig(Env, FileName) ->
    write_template("sysconfig", Env, FileName).

%%====================================================================
%% API
%%====================================================================
%% @doc
%%  Write the template with the Env data to FileName.
%% @end
-spec write_template(TemplateName::string(), env(), FileName::string()) -> ok.
write_template(TemplateName, Env, FileName) ->
    Template = compile_template(TemplateName),
    Out = sgte:render_str(Template, Env),
    file:write_file(FileName, list_to_binary(Out)).

%% @doc
%%  Compile a template for the specified type.
%% @end
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



