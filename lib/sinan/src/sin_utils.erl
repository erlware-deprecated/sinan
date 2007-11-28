%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
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
%%% @author Eric Merritt
%%% @doc
%%%  A group of utility functions for project automation.
%%% @end
%%% @copyright 2006
%%%---------------------------------------------------------------------------
-module(sin_utils).

-include("file.hrl").

%% API
-export([copy_dir/3,
         copy_dir/4,
         parent_dir/1,
         delete_dir/1,
         remove_code_paths/1,
         is_dir_ignorable/2,
         file_exists/1]).


%%====================================================================
%% API
%%====================================================================
%%-------------------------------------------------------------------
%% @doc
%%  Check to see if a file exists.
%%
%% @spec file_exists(FileName) -> true | false.
%% @end
%%-------------------------------------------------------------------
file_exists(FileName) ->
    case file:read_file_info(FileName) of
        {error, enoent} ->
            false;
        Error = {error, _} ->
            throw(Error);
        _ ->
            true
    end.

%%-------------------------------------------------------------------
%% @spec copy_dir(BTop, BuilderDir, TargetDir, Subdirs) -> ok.
%% @doc
%%  Copies the specified directory down to the build
%%  dir on a file by file basis. It only copies if the file has .
%% @end
%%-------------------------------------------------------------------
copy_dir(BuildDir, TargetDir, Sub) ->
    copy_dir(BuildDir, TargetDir, Sub, []).

copy_dir(BuildDir, TargetDir, SubDir, Ignorables) ->
    case are_dirs_ignorable(SubDir, Ignorables) of
        true ->
            ok;
        false ->
            Target = filename:join([BuildDir | SubDir]),
            filelib:ensure_dir(filename:join([Target, "tmp"])),
            CpyTarget = filename:join([TargetDir | SubDir]),
            {ok, Files} = file:list_dir(CpyTarget),
            lists:foldl(fun(IFile, _Acc) ->
                                File = filename:join([CpyTarget, IFile]),
                                case {is_dir_ignorable(IFile, Ignorables),
                                      filelib:is_dir(File)} of
                                    {true, _} ->
                                        ok;
                                    {_, true} ->
                                        copy_dir(BuildDir, TargetDir, SubDir ++
                                                 [IFile], Ignorables);
                                    {_, false} ->
                                        copy_file(Target, IFile, File)
                                end
                        end, [], Files)
    end.



%%-------------------------------------------------------------------
%% @spec parent_dir(Filename) -> DirName | top.
%% @doc
%%  Given a directory returns the name of the parent directory.
%% @end
%%-------------------------------------------------------------------
parent_dir(Filename) ->
    parent_dir(filename:split(Filename), []).

%%-------------------------------------------------------------------
%% @spec parent_dir(List::list()) -> DirName | top.
%% @doc
%%  Given list of directories, splits the list and returns all
%% dirs but the last as a path.
%% @end
%%-------------------------------------------------------------------
parent_dir([_H], []) ->
    top;
parent_dir([], []) ->
    top;
parent_dir([_H], Acc) ->
    filename:join(lists:reverse(Acc));
parent_dir([H | T], Acc) ->
    parent_dir(T, [ H | Acc]).

%%--------------------------------------------------------------------
%% @spec remove_code_path(Paths) -> ok.
%%
%% @doc
%%  Remove the set code paths from the system.
%% @end
%%--------------------------------------------------------------------
remove_code_paths([Path | T]) ->
    code:del_path(Path),
    remove_code_paths(T);
remove_code_paths([]) ->
    ok.

%%--------------------------------------------------------------------
%% @spec is_dir_ignorable(Directory, ListOfIgnores) -> true | false.
%%
%% @doc
%%  Return wether the directory is in the list of ignorables. If it
%%  is then return true, otherwise return false.
%% @end
%% @private
%%--------------------------------------------------------------------
is_dir_ignorable(Sub, [Ignore | Rest]) ->
    case ignore_dir(Sub, Ignore) of
        true ->
            true;
        false ->
            is_dir_ignorable(Sub, Rest)
    end;
is_dir_ignorable(_Sub, []) ->
    false.



%%====================================================================
%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @spec copy_file( Target, IFile, File) -> ok.
%% @doc
%%  Copies the file specified by file to the target specified by
%%  ifile.
%% @end
%% @private
%%-------------------------------------------------------------------
copy_file(_Target, [$. | _], _File) ->
    ok;
copy_file(Target, IFile, File) ->
    NFile = filename:join([Target, IFile]),
    case sin_sig:target_changed(File, NFile) of
        file_not_found ->
            ewl_talk:say("File ~s is not does not exist in the file "
                         "System. This shouldn't happen.", [File]),
            throw(file_not_on_disc);
        unable_to_access ->
            ewl_talk:say("File ~s exists but is inaccessable.", [File]),
            throw(file_inaccessable);
        true ->
            file:copy(File, NFile);
        _ ->
            ok
    end.


%%--------------------------------------------------------------------
%% @spec delete_dir(Dir) -> ok.
%%
%% @doc
%%  Delete the directory and all of its sub directories.
%% @end
%% @private
%%--------------------------------------------------------------------
delete_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foldl(fun(File, _Acc) ->
                                NFile = filename:join([Dir, File]),
                                case filelib:is_dir(NFile) of
                                    true ->
                                        case is_symlink(NFile) of
                                            true ->
                                                file:delete(NFile);
                                            false ->
                                                delete_dir(NFile)
                                        end;
                                    false ->
                                        file:delete(NFile)
                                end
                        end, [], Files),
            file:del_dir(Dir);
        _ ->
            ok
    end.


%%--------------------------------------------------------------------
%% @spec is_symlink(Name) -> true | false.
%%
%% @doc
%%  Check to see if the file is a symlink.
%% @end
%%--------------------------------------------------------------------
is_symlink(Name) ->
    case catch file:read_link_info(Name) of
        {ok, Env} ->
            Env#file_info.type == symlink;
        _Else ->
            false
    end.

%%--------------------------------------------------------------------
%% @spec ignore_dir(Directory, PossibleIgnorePrefix) -> true | false.
%%
%% @doc
%%  Check the directory against teh possible ignores to see if the
%%  prefix matches.
%% @end
%% @private
%%--------------------------------------------------------------------
ignore_dir([Char | SubRest], [Char | IgRest]) ->
    ignore_dir(SubRest, IgRest);
ignore_dir(_Sub, []) ->
    true;
ignore_dir(_Sub, _Ignorable) ->
    false.

%%--------------------------------------------------------------------
%% @spec are_dirs_ignorable([H | T], Igs) -> ok.
%%
%% @doc
%%  If any dirs in the list are ignorable ignore it
%% @end
%%--------------------------------------------------------------------
are_dirs_ignorable([H | T], Igs) ->
    case is_dir_ignorable(H, Igs) of
        false ->
            are_dirs_ignorable(T, Igs);
        true ->
            true
    end;
are_dirs_ignorable([], _Igs) ->
    false.
