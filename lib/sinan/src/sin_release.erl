%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @copyright (C) 2009, PEAK6 LP
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








