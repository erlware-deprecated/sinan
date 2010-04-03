%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
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
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @copyright (C) 2009-2010 Erlware
%%% @doc
%%%
%%% @end
%%% Created : 20 Dec 2009 by Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_release).

-export([get_release/4, get_release/3, get_deps/1,
	get_erts_vsn/1, get_rel_info/1]).

%%====================================================================
%% API
%%====================================================================
get_release(RootDir, Flavor, Name, Version) ->
    VName = [Name, "-", Version, ".rel"],
    NName = [Name, ".rel"],

    % Test <RootDir>/release/<Flavor>/<Name>-<Version>.rel
    Paths = [filename:join([RootDir, "releases", Flavor, VName]),

	     % Test <RootDir>/release/<Flavor>/<Name>.rel
	     filename:join([RootDir, "releases", Flavor, NName]),

	     % Test <RootDir>/release/<Name>-<Version>.rel
	     filename:join([RootDir, "releases", VName]),

	     % Test <RootDir>/release/<Name>.rel
	     filename:join([RootDir, "releases", NName])],

    find_rel_file(Paths).


get_release(RootDir, Name, Version) ->
    get_release(RootDir, none, Name, Version).

get_deps({release, _RelInfo, _ErtsInfo, Deps}) ->
    lists:foldr(fun({Name, Vsn}, Acc) ->
			[{Name, Vsn} | Acc];
		   ({Name, Vsn, _}, Acc) ->
			[{Name, Vsn} | Acc];
		   ({Name, Vsn, _, _}, Acc) ->
			[{Name, Vsn} | Acc]
		end,
		[],
		Deps).

get_erts_vsn({release, _RelInfo, {erts, Vsn}, _Deps}) ->
    Vsn.

get_rel_info({release, RelInfo, _ErtsInfo, _Deps}) ->
    RelInfo.

%%====================================================================
%% Internal functions
%%====================================================================


get_file(Path) ->
    case sin_utils:file_exists(Path) of
	true ->
	    file:consult(Path);
	false ->
	    no_file
    end.


find_rel_file([Path | Rest]) ->
    case get_file(Path) of
	{ok, [Data]} ->
	    Data;
	no_file ->
	    find_rel_file(Rest)
    end;
find_rel_file([]) ->
    no_file.








