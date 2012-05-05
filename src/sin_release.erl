%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @copyright (C) 2009 - 2011, Erlware
%%% @doc
%%%  correctly generate release information
%%% @end
%%%-------------------------------------------------------------------
-module(sin_release).

-export([get_release/4,
         get_deps/1,
         get_erts_vsn/1,
         get_rel_info/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc get the rel file for the specified information
-spec get_release(sin_state:state(),
                  string(), string(), string()) ->
    RelFilePath::string().
get_release(State, RootDir, Name, Version) ->
    VName = [Name, "-", Version, ".rel"],
    NName = [Name, ".rel"],

    % Test <RootDir>/release/<Name>-<Version>.rel
    Paths = [filename:join([RootDir, "releases", VName]),

             % Test <RootDir>/release/<Flavor>/<Name>.rel
             filename:join([RootDir, "releases", NName])],

    find_rel_file(State, Paths).

%% @doc get dependenciens for the release
-spec get_deps(ReleaseInfo::term()) ->
    [{Name::string(), Version::string()}].
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

%% @doc get the erts version from the release informatino
-spec get_erts_vsn(ReleaseInfo::term()) -> Vsn::string().
get_erts_vsn({release, _RelInfo, {erts, Vsn}, _Deps}) ->
    Vsn.
%% @doc get the release name from the system
-spec get_rel_info(ReleaseInfo::term()) -> string().
get_rel_info({release, RelInfo, _ErtsInfo, _Deps}) ->
    RelInfo.

%%====================================================================
%% Internal functions
%%====================================================================

get_file(State, Path) ->
    case sin_utils:file_exists(State, Path) of
        true ->
            file:consult(Path);
        false ->
            no_file
    end.

find_rel_file(State, [Path | Rest]) ->
    case get_file(State, Path) of
        {ok, [Data]} ->
            Data;
        no_file ->
            find_rel_file(State, Rest)
    end;
find_rel_file(_State, []) ->
    no_file.
