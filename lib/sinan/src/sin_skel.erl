%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%%-------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Erlware
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
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%% Provides skeletons for application generation
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%%-------------------------------------------------------------------
-module(sin_skel).

%%% API
-export([application/3, supervisor/3,
         app_info/3, edoc_overview/3,
	 build_config/2,
	 sysconfig/2,
	 bin_support/2,
	 bin/2]).

%-include("eunit.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Writes out the application file
%% @spec application(Env, FileName, AppName) -> ok
%% @end
%%--------------------------------------------------------------------
application(Env, FileName, AppName) ->
    write_template("application", [{app_name, AppName} | Env], FileName).

%%--------------------------------------------------------------------
%% @doc
%%    Writes out a generic supervisor to the filename provided.
%% @spec supervisor(Env, FileName, AppName) -> ok
%% @end
%%--------------------------------------------------------------------
supervisor(Env, FileName, AppName) ->
    write_template("supervisor", [{app_name, AppName} | Env], FileName).


%%--------------------------------------------------------------------
%% @doc
%%    Writes out a generic application to the filename provided.
%% @spec app_info(Env, FileName, AppName) -> ok
%% @end
%%--------------------------------------------------------------------
app_info(Env, FileName, AppName) ->
    write_template("dotapp", [{app_name, AppName} | Env], FileName).

%%--------------------------------------------------------------------
%% @doc
%%    Writes out a overview.edoc to the filename provided.
%% @spec (Env, FileName, AppName) -> ok
%% @end
%%--------------------------------------------------------------------
edoc_overview(Env, FileName, AppName) ->
    write_template("overview", [{app_name, AppName} | Env], FileName).

%%--------------------------------------------------------------------
%% @doc
%%  Writes the build_config to the specified library with
%%  the specified repo.
%% @spec build_config(Env, FileName) -> ok
%% @end
%%--------------------------------------------------------------------
build_config(Env, FileName) ->
    write_template("buildcfg", Env, FileName).


%%--------------------------------------------------------------------
%% @doc
%% Writes the bin file out to the specified library
%% @spec bin(Env, Filename) -> ok
%% @end
%%-------------------------------------------------------------------
bin(Env, FileName) ->
    write_template("bin", Env, FileName).

%%--------------------------------------------------------------------
%% @doc
%% Writes the bin support file
%% @spec bin_support(Env, Filename) -> ok
%% @end
%%-------------------------------------------------------------------
bin_support(Env, FileName) ->
    write_template("erlware_release_start_helper", Env, FileName).


%%--------------------------------------------------------------------
%% @doc
%%  Writes the sys config out to the specifiecd place
%% @spec sysconfig(Env, Filename) -> ok
%% @end
%%-------------------------------------------------------------------
sysconfig(Env, FileName) ->
    write_template("sysconfig", Env, FileName).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Write the template with the Env data to FileName.
%% @spec write_template(Type, Env, FileName) -> ok
%% @end
%%--------------------------------------------------------------------
write_template(Type, Env, FileName) ->
    Template = compile_template(Type),
    Out = sgte:render_str(Template, Env),
    file:write_file(FileName, list_to_binary(Out)).

%%--------------------------------------------------------------------
%% @doc
%%  Compile a template for the specified type.
%% @spec compile_template(Type) -> Template
%% @end
%%--------------------------------------------------------------------
compile_template(Type) ->
    PrivDir = code:priv_dir(sinan),
    TemplateFile = filename:join([PrivDir, Type]),
    case sgte:compile_file(TemplateFile) of
        {error, _} ->
            exit(unable_to_compile_template_file);
        {ok, Template} ->
            Template
    end.

%%%====================================================================
%%% tests
%%%====================================================================



