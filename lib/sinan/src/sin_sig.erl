%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
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
%%%  Checks to see if a file has been changed.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%%--------------------------------------------------------------------------
-module(sin_sig).

-include("file.hrl").
-include("eunit.hrl").


%% API
-export([save_sig_info/4,
	 get_sig_info/3,
	 changed/3, update/3,
	 target_changed/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Take a term and put it in the correct place in the sig area.
%% @spec (NS, BuildDir, File, Terms) -> true | false
%% @end
%%--------------------------------------------------------------------
save_sig_info(NS, BuildDir, File, Terms) ->
    Target = make_filename(File, []),
    SIG = filename:join([BuildDir, "sig", NS, Target]),
    filelib:ensure_dir(filename:join([BuildDir, "sig", NS, "tmp"])),
    file:write_file(SIG,io_lib:fwrite("~p.\n",[Terms])).

%%--------------------------------------------------------------------
%% @doc
%%  Take a predefined sig term file and return the values in that file.
%% @spec (NS, BuildDir, File) -> true | false
%% @end
%%--------------------------------------------------------------------
get_sig_info(NS, BuildDir, File) ->
    Target = make_filename(File, []),
    SIG = filename:join([BuildDir, "sig", NS, Target]),
    case file:consult(SIG) of
	{ok, [Terms]} ->
	    Terms;
	_  ->
	    undefined
    end.

%%--------------------------------------------------------------------
%% @doc
%%  Check to see if the file has been changed. The build dir should
%%  be the fully qualified path to the projects top level build
%%  directory.
%%
%% @spec (NS, BuildDir, File) -> true | false
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%%  Check to see if the file has changed in comparison to another
%%  file.
%% @spec (StartFile, TargetFile) -> true | false
%% @end
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% @doc
%%  Update the signature for file. Build dir should be the build
%% fully qualified build directory of the system.
%% @spec (NS, BuildDir, File) -> ok
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc
%%  convert the directory/filename into something that can be
%%  a verified filename.
%% @spec make_filename(Name, Acc) -> Filename::string()
%% @end
%% @private
%%--------------------------------------------------------------------
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

