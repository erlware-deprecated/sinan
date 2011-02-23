%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Checks to see if a file has been changed.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%--------------------------------------------------------------------------
-module(sin_sig).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([save_sig_info/4,
	 get_sig_info/3,
	 changed/3,
	 update/3,
	 target_changed/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Take a term and put it in the correct place in the sig area.
-spec save_sig_info(string(), string(), string(), Data::term()) -> boolean().
save_sig_info(NS, BuildDir, File, Terms) ->
    Target = make_filename(File, []),
    SIG = filename:join([BuildDir, "sig", NS, Target]),
    filelib:ensure_dir(filename:join([BuildDir, "sig", NS, "tmp"])),
    file:write_file(SIG,io_lib:fwrite("~p.\n",[Terms])).

%% @doc Take a predefined sig term file and return the values in that file.
-spec get_sig_info(string(), string(), string()) -> boolean().
get_sig_info(NS, BuildDir, File) ->
    Target = make_filename(File, []),
    SIG = filename:join([BuildDir, "sig", NS, Target]),
    case file:consult(SIG) of
	{ok, [Terms]} ->
	    Terms;
	_  ->
	    undefined
    end.

%% @doc Check to see if the file has been changed. The build dir should be the
%% fully qualified path to the%% projects top level build directory.
-spec changed(string(), string(), string()) -> boolean().
changed(NS, BuildDir, File) ->
    Target = make_filename(File, []),
    SIG = filename:join([BuildDir, "sig", NS, Target]),
    case {file:read_file_info(SIG), file:read_file_info(File)} of
        {_, {error, enoent}} ->
            file_not_found;
        {_, {error, _}} ->
            unable_to_access;
        {{error, enoent}, _} ->
            true;
        {{ok, SigInfo}, {ok, FileInfo}}  when SigInfo#file_info.mtime =<
                                              FileInfo#file_info.mtime ->
            {ok, Bin} = file:read_file(File),
            MD5 = erlang:md5(Bin),
            {ok, MD52} = file:read_file(SIG),
            case MD5 == MD52 of
                true ->
                    false;
                false ->
                    true
            end;
        _ ->
            false
    end.

%% @doc Check to see if the file has changed in comparison to another file.
-spec target_changed(StartFile::string(), TargetFile::string()) -> boolean().
target_changed(StartFile, TargetFile) ->
    case {file:read_file_info(TargetFile), file:read_file_info(StartFile)} of
        {_, {error, enoent}} ->
            file_not_found;
        {_, {error, eacces}} ->
            unable_to_access;
        {{ok, TargetInfo}, {ok, FileInfo}}  when TargetInfo#file_info.mtime <
                                                 FileInfo#file_info.mtime ->
            true;
        {{ok, TargetInfo}, {ok, FileInfo}}  when TargetInfo#file_info.mtime >=
                                                 FileInfo#file_info.mtime ->
            false;
        _ ->
            true
    end.

%% @doc Update the signature for file. Build dir should be the build fully
%% qualified build directory of the system.
-spec update(string(), string(), string()) -> ok.
update(NS, BuildDir, File) ->
    Target = make_filename(File, []),
    SIG = filename:join([BuildDir, "sig", NS, Target]),
    filelib:ensure_dir(filename:join([BuildDir, "sig", NS, "tmp"])),
    case file:read_file(File) of
        {ok, Bin} ->
            MD5 = erlang:md5(Bin),
            file:write_file(SIG, MD5);
        Error = {error, _} ->
            Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc convert the directory/filename into something that can be a verified
%% filename.
make_filename([$/ | T], Acc) ->
    make_filename(T, [$_ | Acc]);
make_filename([$\\ | T], Acc) ->
    make_filename(T, [$_ | Acc]);
make_filename([$: | T], Acc) ->
    make_filename(T, [$_, $_ | Acc]);
make_filename([H | T], Acc) ->
    make_filename(T, [H | Acc]);
make_filename([], Acc) ->
    lists:reverse([$g, $i, $s, $. | Acc]).

%%====================================================================
%% Tests
%%====================================================================

make_filename_test() ->
    ?assertMatch("C___Windows_test_allac.sig",
                 make_filename("C:\\Windows\\test\\allac", [])),
    ?assertMatch("_home_test_testa_testn.sig",
                 make_filename("/home/test/testa/testn", [])).

