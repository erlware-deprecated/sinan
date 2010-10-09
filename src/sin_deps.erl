%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2010 Erlware
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
%%% @copyright 2008 Anders Nygren
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Find the applications that an application or realease
%%% is depending on.
%%% @end
%%%-------------------------------------------------------------------
-module(sin_deps).

%% API
-export([app/1,
	 rel/3]).

-export([find/2]).

-define(PRELOADED, [erlang, init, prim_file, erl_prim_loader]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec rel(LibDir::string(), ExtraApps, ExcludeApps) -> Res
%% ExtraApps = [atom()]
%% ExcludeApps = [atom()]
%% @doc Find all applications and their versions that have to be
%% included in a release.
%% @end
%%--------------------------------------------------------------------
rel(LibDir, ExtraApps, Exclude) ->
    MyApps = find_apps(LibDir),
    AppsInPath = lists:foldl(fun (D, Acc) ->
				  case find_app(D) of
				      {'',"."} -> Acc;
				      App -> [App|Acc]
				  end
			  end,
			  [],code:get_path()),
    Extras  = lists:map(fun(App) ->
				{value,Val}= lists:keysearch(App, 1,
							     AppsInPath),
				Val
			end, ExtraApps),
    InitApps = MyApps++Extras,
    lists:sort(app_deps(InitApps, Exclude)).


app_deps(InitApps, Exclude) ->
    As = [read_app_file(App, EbinDir) || {App, EbinDir} <- InitApps],
    Acc= [{App, Vsn} || {app, App, Vsn, _Modules} <-As],
    app_deps(InitApps, Exclude, Acc).

app_deps(Apps, Exclude, Done) ->
    lists:foldl(
      fun(App, Acc) ->
	      Deps = app(App),
	      Deps1 = [{A,Vsn}||{A,Vsn}<-Deps,
				not lists:keymember(A, 1, Acc),
				not lists:member(A, Exclude)],
	      Deps1 ++ Acc
      end, Done, Apps).

%%--------------------------------------------------------------------
%% @spec app(App::atom()) -> Result
%% Result = [{App::atom(), Vsn::string()}]
%% @doc Find all applications and their versions that an application
%% depends on.
%% @end
%%--------------------------------------------------------------------
app(App) when is_atom(App) ->
    [D] = find(".", "^"++atom_to_list(App)++".app$"),
    Dir = filename:dirname(D),
    app({App, Dir});

app({App, Dir}) when is_list(Dir) ->
    {app, App, _Vsn, Modules} = read_app_file(App, Dir),
    Ds = [lists:usort(get_deps(Module)) || Module <- Modules],
    R = lists:umerge(Ds),
    Exts = lists:subtract(R, Modules++?PRELOADED),
    AppFiles = lists:usort([mod_to_appfile(M) || M<-Exts]),
    [appfile_to_app(AppFile) || AppFile <- AppFiles].

%%====================================================================
%% Internal functions
%%====================================================================
find_apps(LibDir) ->
    AppFiles = find(LibDir, "^.*.app$"),
    [split_app_path(AppFile) || AppFile <- AppFiles].

find_app(Dir) ->
    AppFile = find(Dir, "^.*.app$"),
    split_app_path(AppFile).

split_app_path(Path) ->
    AppName = list_to_atom(filename:rootname(filename:basename(Path))),
    Ebin = filename:dirname(Path),
    {AppName, Ebin}.

read_app_file(App, Dir) ->
    AppFile = filename:join(Dir, atom_to_list(App)++".app"),
    {ok, [{application, App, Attrs}]} = file:consult(AppFile),
    {value, {vsn, Vsn}} = lists:keysearch(vsn, 1, Attrs),
    {value, {modules, Modules}} = lists:keysearch(modules, 1, Attrs),
    {app, App, Vsn, Modules}.

%%====================================================================
get_deps(Module) ->
    {ok,{_,[{imports,Is}]}} = beam_lib:chunks(code:which(Module),[imports]),
    lists:usort([Mod || {Mod, _Fun, _Ar} <- Is]).

mod_to_appfile(Mod) ->
    case code:which(Mod) of
	non_existing ->
	    {app_not_found, Mod};
	preloaded ->
	    {preloaded, Mod};
	Path ->
	    [AppFile]=find(filename:dirname(Path), "^.*.app$"),
	    AppFile
    end.

appfile_to_app({app_not_found,_App}=A) ->
    A;
appfile_to_app(AppFile) ->
    {ok,[{application,App,Attrs}]} = file:consult(AppFile),
    {value,{vsn,Vsn}} = lists:keysearch(vsn,1,Attrs),
    {App, Vsn}.

find(Root, RegExp) ->
    filelib:fold_files(Root, RegExp, true, fun (F,Acc) -> [F|Acc] end,[]).


