%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
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
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%  Creates edoc format documentation for the project
%%% @end
%%% Created : 16 Oct 2006 by Eric Merritt <cyberlync@gmail.com>
%%%---------------------------------------------------------------------------
-module(sin_edoc).


%% API
-export([doc/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  Run the docs.
%% 
%% @spec doc(BuildRef) -> ok. 
%% @end
%%--------------------------------------------------------------------
doc(BuildRef, _) ->
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    DocDir = filename:join([BuildDir, "docs", "edoc"]),
    filelib:ensure_dir(filename:join([DocDir, "tmp"])),
    Apps = fconf:get_value(BuildRef, "project.apps"),
    run_docs(BuildRef, Apps, [{dir, DocDir}]).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec run_docs(AppList, Opts) -> ok.
%% 
%% @doc 
%%  Run edoc on all the modules in all of the applications.
%% @end
%%--------------------------------------------------------------------
run_docs(BuildRef, [{AppName, _, _} | T], Opts) ->
    AppDir = fconf:get_value(BuildRef, {path, ["apps", 
                                            atom_to_list(AppName), 
                                            "basedir"]}),
    edoc:application(AppName,
                     AppDir,
                     Opts),
    run_docs(BuildRef, T, Opts);
run_docs(_BuildRef, [], _Opts) ->
    ok.
