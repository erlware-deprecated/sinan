%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  A group of utility functions for project automation.
%%% @end
%%% @copyright 2006 - 2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_utils).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("sinan/include/sinan.hrl").

%% API
-export([copy_dir/3,
         copy_dir/4,
         copy_dir/5,
         copy_dir/6,
         copy_entire_dir/3,
         delete_dir/1,
         file_exists/2,
         get_application_env/2,
         get_ignore_dirs/1,
         is_dir_ignorable/2,
         remove_code_paths/1,
         term_to_list/1,
         to_bool/1,
         format_exception/1,
         basename/1,
         get_erts_dir/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Trivially convert a value to a boolean
-spec to_bool(any()) -> boolean().
to_bool(true) ->
    true;
to_bool("True") ->
    true;
to_bool("true") ->
    true;
to_bool(_) ->
    false.

%% @doc Delete the directory and all of its sub directories.
%% @private
-spec delete_dir(Dir::string()) -> ok.
delete_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foldl(fun (File, _Acc) ->
                                NFile = filename:join([Dir, File]),
                                case filelib:is_dir(NFile) of
                                    true ->
                                        case is_symlink(NFile) of
                                            true -> file:delete(NFile);
                                            false -> delete_dir(NFile)
                                        end;
                                    false -> file:delete(NFile)
                                end
                        end,
                        [], Files),
            file:del_dir(Dir);
        _ ->
            ok
    end.

%% @doc Get the ignore dirs
-spec get_ignore_dirs(sin_state:state()) ->
                             IgnoreDirs::[string()].
get_ignore_dirs(State) ->
    sin_state:get_value(ignore_dirs, [], State).

%% @doc Check to see if a file exists.
-spec file_exists(sin_state:state(), FileName::string()) -> boolean().
file_exists(State, FileName) ->
    case file:read_file_info(FileName) of
        {error, enoent} ->
            false;
        Error = {error, _} ->
            ?SIN_RAISE(State, {file_access, FileName}, Error);
        _ ->
            true
    end.

%% @doc Copies the specified directory down to the build dir on a file by file
%% basis. It only copies if the file has .
-spec copy_dir(sin_state:state(), string(),
               string()) -> ok.
copy_dir(State, To, From) ->
    copy_dir(State, To, From, [], [], []).

-spec copy_dir(sin_state:state(), string(),
               string(), list()) -> ok.
copy_dir(State, To, From, Options) ->
    copy_dir(State, To, From, [], [], Options).

-spec copy_dir(sin_state:state(),
               string(),
               string(),
               string(),
               list()) -> ok.
copy_dir(State, To, From, Sub, Options) ->
    copy_dir(State, To, From, Sub, [], Options).

-spec copy_dir(sin_state:state(),
               string(),
               string(),
               string(),
               Ignorables::string(),
               list()) -> sin_state:state().
copy_dir(State0, To, From, SubDir, Ignorables, Options) ->
    case lists:member(keep_parent, Options) of
        %% Copy the enitre directory, instead of just subdirs/files
        true ->
            ParentDir = hd(lists:reverse(filename:split(From))),
            NewTo = filename:join([To, ParentDir]),
            filelib:ensure_dir(NewTo),
            copy_dir2(State0, NewTo, From, SubDir, Ignorables);
        false ->
            copy_dir2(State0, To, From, SubDir, Ignorables)
    end.

copy_dir2(State0, To, From, SubDir, Ignorables) ->
    check_not_circular(State0, To, From, SubDir),
    case are_dirs_ignorable(SubDir, Ignorables) of
        true ->
            State0;
        false ->
            Target = filename:join([To | SubDir]),
            filelib:ensure_dir(filename:join([Target, "tmp"])),
            CpyTarget = filename:join([From | SubDir]),
            {ok, Files} = file:list_dir(CpyTarget),
            lists:foldl(fun(IFile, State1) ->
                                File = filename:join([CpyTarget, IFile]),
                                case {is_dir_ignorable(IFile, Ignorables),
                                      filelib:is_dir(File)}
                                of
                                    {true, _} ->
                                        State1;
                                    {_, true} ->
                                        copy_dir2(State1,
                                                 To, From,
                                                 SubDir ++ [IFile], Ignorables);
                                    {_, false} ->
                                        copy_file(State1, Target, IFile, File)
                                end
                        end,
                        State0, Files)
    end.

%% @doc Copy the enitre directory, instead of just subdirs/files
-spec copy_entire_dir(sin_state:state(), string(),
               string()) -> ok.
copy_entire_dir(State, To, From) ->
    copy_dir(State, To, From, [], []).

%% @doc Remove the specified code paths from the system code paths.
-spec remove_code_paths(Paths::[string()]) -> ok.
remove_code_paths([Path | T]) ->
    code:del_path(Path), remove_code_paths(T);
remove_code_paths([]) ->
    ok.

%% @doc Return wether the directory is in the list of ignorables. If it is then
%%  return true, otherwise return false.
-spec is_dir_ignorable(Directory::string(), ListOfIgnores::[string()]) ->
    boolean().
is_dir_ignorable(Sub, [Ignore | Rest]) ->
    case ignore_dir(Sub, Ignore) of
        true ->
            true;
        false ->
            is_dir_ignorable(Sub, Rest)
    end;
is_dir_ignorable(_Sub, []) ->
    false.

%% Strip the extension off of a base.
-spec basename(string()) ->
    string().
basename(File) ->
    basename1(lists:reverse(File), []).

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%% @doc Get the directory were erts is located
-spec get_erts_dir() -> string().
get_erts_dir() ->
    Prefix = code:root_dir(),
    ErtsVersion = erlang:system_info(version),
    filename:join([Prefix, "erts-" ++ ErtsVersion]).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Copies the file specified by file to the target specified by ifile.
-spec copy_file(sin_state:state(),
                Target::string(), IFile::string(), File::string()) ->
                       sin_state:state().
copy_file(State, _Target, [$. | _], _File) ->
            State;
copy_file(State, Target, IFile, File) ->
    NFile = filename:join([Target, IFile]),
    case sin_sig:changed(copy, File, State) of
        file_not_found ->
            ec_talk:say("File ~s is not does not exist in the "
                       "file System. This shouldn't happen.",
                         [File]),
            ?SIN_RAISE(State, {file_not_on_disc, File});
        unable_to_access ->
            ec_talk:say("File ~s exists but is inaccessable.",
                         [File]),
            ?SIN_RAISE(State, {file_inaccessable, File});
        true ->
            {ok, FileInfo} = file:read_file_info(File),
            file:copy(File, NFile),
            file:write_file_info(NFile, FileInfo),
            sin_sig:update(copy, File, State);
        _ ->
            State
    end.

%% @doc Check to see if the file is a symlink.
-spec is_symlink(FileName::string()) -> boolean.
is_symlink(FileName) ->
    case catch file:read_link_info(FileName) of
        {ok, Env} ->
            Env#file_info.type == symlink;
        _Else ->
            false
    end.

%% @doc Check the directory against the possible ignores to see if the prefix
%%  matches.
-spec ignore_dir(Directory::string(), PossibleIgnorePrefix::[string()]) ->
    boolean().
ignore_dir([Char | SubRest], [Char | IgRest]) ->
    ignore_dir(SubRest, IgRest);
ignore_dir(_Sub, []) ->
    true;
ignore_dir(_Sub, _Ignorable) ->
    false.

%% @doc If any dirs in the list are ignorable ignore it
-spec are_dirs_ignorable(ListOfDirs::[string()], Igs::[string()]) -> boolean().
are_dirs_ignorable([Dir | RestOfDirs], Igs) ->
    case is_dir_ignorable(Dir, Igs) of
        false ->
            are_dirs_ignorable(RestOfDirs, Igs);
        true ->
            true
    end;
are_dirs_ignorable([], _Igs) ->
    false.


%% @doc convert a list body to a string
-spec listify_list_body(I::term(), Acc::list()) -> list().
listify_list_body([H | T], []) ->
    listify_list_body(T, term_to_list(H));
listify_list_body([H | T], Acc) ->
    listify_list_body(T, [Acc, ",", term_to_list(H)]);
listify_list_body([], Acc) ->
    Acc.

%% @doc Convert an arbitrary term to a list
-spec term_to_list(I::term()) -> list().
term_to_list(I) when is_integer(I) ->
    integer_to_list(I);
term_to_list(I) when is_atom(I) ->
    atom_to_list(I);
term_to_list(I) when is_float(I) ->
    float_to_list(I);
term_to_list(I) when is_tuple(I) ->
    ["{", listify_list_body(tuple_to_list(I), []), "}"];
term_to_list(I) when is_binary(I) ->
    binary_to_list(I);
term_to_list(I) when is_list(I) ->
    case is_string(I) of
        true ->
            ["\"", I, "\""];
        false ->
            ["[", listify_list_body(I, []), "]"]
    end.

-spec is_character(Char::integer()) -> boolean().
is_character(Char) when Char < 0 ->
    false;
is_character(Char) when Char > 255 ->
    false;
is_character(_) ->
    true.

-spec is_string(String::string()) -> boolean().
is_string(XY) when is_list(XY) ->
    lists:all(fun is_character/1, XY);
is_string(_) ->
    false.

%% @doc Get an enviroment variable, throw error if unavailable.
-spec get_application_env(sin_state:state(), atom()) -> term().
get_application_env(State, Key) ->
    case application:get_env(sinan, Key) of
        {ok, Value} ->
            Value;
      _ ->
            ?SIN_RAISE(State, variables_not_set,
                       "Key ~w not set, must be set as application "
                       "environment variable ",
                       [Key])
    end.

-spec check_not_circular(sin_state:state(), string(), string(), string()) ->
    ok.
check_not_circular(State, Target, Source, SubDir) ->
    case filename:split(Source) ++ SubDir ==
        filename:split(Target) of
        true ->
            ec_talk:say("Can't copy a directory to itself (~p "
                         "to ~p)",
                         [filename:join([Source | SubDir]), Target]),
            ?SIN_RAISE(State, circular_recursion);
        false ->
            ok
    end.


basename1([$. | Rest], _Acc) ->
    Rest;
basename1([H | Rest], Acc) ->
    basename1(Rest, [H | Acc]);
basename1([], Acc) ->
    lists:reverse(Acc).

%%====================================================================
%% tests
%%====================================================================

