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
-include_lib("sinan/include/sinan.hrl").

%% API
-export([new/0,
         do_if_changed/5,
         save_sig_info/3,
         get_sig_info/2,
         changed/3,
         update/3,
         save/2,
         load/2]).

-define(BUILD_STATE_KEY, '__sin_sig__').
-define(TERM_KEY, '__sin_sig_term__').

%%====================================================================
%% Types
%%====================================================================
-opaque sig() :: dict().

%%====================================================================
%% API
%%====================================================================
-spec new() -> sig().
new() ->
    dict:new().

%% @doc many times an action needs to occur on a file or directory only if that
%% file or directory have changed. Then the result of that action should be
%% saved. For example, lets say you want to parse a file if that file has
%% changed. If the file hasn't changed there is no reason reparse the file,
%% better just to return the data from the last time you parsed it. This
%% function helps you do that. Automatically checking if the file specified
%% changed. If it did change it calls the function passed in cachees the result
%% and continues. If the file did not change it simple returns the cached value.
-spec do_if_changed(term(),string(), fun(), fun(), sin_state:state()) ->
                           {sin_state:state(), term()}.
do_if_changed(NS, Path, DepsChangedAction, Function, State0) ->
    case sin_sig:changed(NS, Path, State0) of
        true ->
            {State1, Result} = Function(Path, State0),
            {sin_sig:save_sig_info({?MODULE, Path}, Result, State1),
             Result};
         false ->
            case sin_sig:get_sig_info({NS, Path}, State0) of
                {ok, Result} ->
                    case DepsChangedAction(State0, Result) of
                        true ->
                            {State1, Result} = Function(Path, State0),
                            {sin_sig:save_sig_info({?MODULE, Path},
                                                   Result, State1), Result};
                        false ->
                            {State0, Result}
                    end;
                _ ->
                    {State1, Result} = Function(Path, State0),
                    {sin_sig:save_sig_info({?MODULE, Path}, Result, State1),
                     Result}
            end
    end.


%% @doc Take a term and put it in the correct place in the sig area.
-spec save_sig_info(term(), term(), sin_state:state()) ->
                           sin_state:state().
save_sig_info(Key, Value, State) ->
    update_state(dict:store({?TERM_KEY, Key}, Value, get_sig(State)), State).

%% @doc Get the values determined by key from the sig
-spec get_sig_info(term(), sin_state:state()) ->
                          {ok, term()} | error.
get_sig_info(Key, BuildState) ->
    dict:find({?TERM_KEY, Key}, get_sig(BuildState)).

%% @doc Check to see if the file has been changed. The build dir should be the
%% fully qualified path to the%% projects top level build directory.
-spec changed(term(), string(), sin_state:state()) -> boolean().
changed(NS, TargetFile, State) ->
    case dict:find({file, NS, TargetFile}, get_sig(State)) of
        {ok, DiscInfoMtime} ->
            case file:read_file_info(TargetFile) of
                {error, enoent} ->
                    file_not_found;
                {error, eacces} ->
                    unable_to_access;
                {ok, #file_info{mtime=FileInfoMtime}} when DiscInfoMtime <
                                                           FileInfoMtime ->
                    true;
                {ok, #file_info{mtime=FileInfoMtime}} when DiscInfoMtime >=
                                                           FileInfoMtime ->
                    false;
                _ ->
                    true
            end;
        _ ->
            true
    end.

%% @doc Update the signature for file. Build dir should be the build fully
%% qualified build directory of the system.
-spec update(term(), string(), sin_state:state()) ->
                    sin_state:state().
update(NS, File, State) ->
    {ok, #file_info{mtime=FileInfoMTime}} = file:read_file_info(File),
    Sig0 = dict:store({file, NS, File}, FileInfoMTime, get_sig(State)),
    update_state(Sig0, State).

%% @doc save a sig stored in the build state to a file
-spec save(string(), sin_state:state()) ->
                  sin_state:state().
save(Directory, State) ->
    Sig = dict:to_list(get_sig(State)),
    file:write_file(filename:join(Directory, ".sig"),
                    io_lib:format("~p.\n",
                                  [Sig])),
    State.

%% @doc load a previously saved sig into the build state
-spec load(string(), sin_state:state()) ->
                  sin_state:state().
load(Directory, State) ->
    SigPath = filename:join(Directory, ".sig"),
    List = case file:consult(SigPath) of
               {error, enoent} ->
                   [];
               {ok, [Terms]} ->
                   Terms;
               Error ->
                   ?SIN_RAISE(State, {got_invalid_result_on_sig_load, Error})
           end,
    update_state(dict:from_list(List), State).

%%====================================================================
%% Internal functions
%%====================================================================
get_sig(BuildState) ->
    sin_state:get_value(?BUILD_STATE_KEY, new(), BuildState).

update_state(Sig, BuildState) ->
    sin_state:store(?BUILD_STATE_KEY, Sig, BuildState).


