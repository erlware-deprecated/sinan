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
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   A simple file logger for the sinan log file based on sasl_report_file_h
%%% @end
%%% @copyright (C) 2007-2010
%%% Created : 2007 by Eric Merritt
%%%---------------------------------------------------------------------------
-module(sin_file_logger).

%% API
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initalize by opening the file in 'append' mode.
%%
%% @spec init(File) -> Res
%% @end
%%--------------------------------------------------------------------
init(File) ->
    process_flag(trap_exit, true),
    case file:open(File, [append]) of
	{ok,Fd} ->
	    {ok, {Fd, File}};
	What ->
	    What
    end.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Handle events by printing out to a file using the sasl_report module.
%%
%% @spec handle_event(Message::term(), State::term()) -> {ok, State}
%% @end
%% @private
%%--------------------------------------------------------------------
handle_event({_Type, GL, _Msg}, State) when node(GL) /= node() ->
    {ok, State};
handle_event(Event, {Fd, File}) ->
    case is_sasl_reportable(Event) of
        true ->
            sasl_report:write_report(Fd, all, tag_event(Event));
        false ->
            write_event(Fd, tag_event(Event))
    end,
    {ok, {Fd, File}};
handle_event(_, State) ->
    {ok, State}.
%%--------------------------------------------------------------------
%% @doc
%%  Handle any messages from the system. In the case of an 'EXIT' message
%%  return remove_handler, in every other case return '{ok, State::term()}'
%% @spec handle_info(Message::term(), State::term()) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
handle_info({'EXIT', Fd, _Reason}, {Fd, _File, _Type}) ->
    remove_handler;
handle_info(_, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc
%%  We don't actually handle calls so just return {error, bad_query}
%% @spec handle_call(_Query, _State) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
handle_call(_Query, _State) ->
    {error, bad_query}.

%%--------------------------------------------------------------------
%% @doc
%%  Clean up on terminate. This means closing the open file descriptor
%% @spec (Ignorable, {Fd, _File, _Type}) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
terminate(_, {Fd, _File, _Type}) ->
    file:close(Fd),
    [].
%%--------------------------------------------------------------------
%% @doc
%%  Tag the event with the time. This is from sasl
%% @spec tag_event(Event) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
tag_event(Event) ->
    {calendar:local_time(), Event}.


%%--------------------------------------------------------------------
%% @doc
%%  Check to see if sasl can report on this error.
%% @spec is_sasl_reportable(Event::term()) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
is_sasl_reportable({error_report, _, {_,supervisor_report, _}}) ->
    true;
is_sasl_reportable({error_report, _, {_,crash_report, _}}) ->
    true;
is_sasl_reportable(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec write_event(Fd, {Time, {error, _GL, {Pid, Format, Args}}}) ->  void()
%% @end
%% @private
%%--------------------------------------------------------------------
write_event(Fd, {Time, {error, _GL, {Pid, Format, Args}}}) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    io:format(Fd, T ++ S, []);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    io:format(Fd, T ++ F, [Format,Args])
    end;
write_event(Fd, {Time, {emulator, _GL, Chars}}) ->
    T = write_time(maybe_utc(Time)),
    case catch io_lib:format(Chars, []) of
	S when is_list(S) ->
	    io:format(Fd, T ++ S, []);
	_ ->
	    io:format(Fd, T ++ "ERROR: ~p ~n", [Chars])
    end;
write_event(Fd, {Time, {info, _GL, {Pid, Info, _}}}) ->
    T = write_time(maybe_utc(Time)),
    io:format(Fd, T ++ add_node("~p~n",Pid),[Info]);
write_event(Fd, {Time, {error_report, _GL, {Pid, std_error, Rep}}}) ->
    T = write_time(maybe_utc(Time)),
    S = format_report(Rep),
    io:format(Fd, T ++ S ++ add_node("", Pid), []);
write_event(Fd, {Time, {info_report, _GL, {Pid, std_info, Rep}}}) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    S = format_report(Rep),
    io:format(Fd, T ++ S ++ add_node("", Pid), []);
write_event(Fd, {Time, {info_msg, _GL, {Pid, Format, Args}}}) ->
    T = write_time(maybe_utc(Time), "INFO REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    io:format(Fd, T ++ S, []);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    io:format(Fd, T ++ F, [Format,Args])
    end;
write_event(Fd, {Time, {warning_report, _GL, {Pid, std_warning, Rep}}}) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    S = format_report(Rep),
    io:format(Fd, T ++ S ++ add_node("", Pid), []);
write_event(Fd, {Time, {warning_msg, _GL, {Pid, Format, Args}}}) ->
    T = write_time(maybe_utc(Time), "WARNING REPORT"),
    case catch io_lib:format(add_node(Format,Pid), Args) of
	S when is_list(S) ->
	    io:format(Fd, T ++ S, []);
	_ ->
	    F = add_node("ERROR: ~p - ~p~n", Pid),
	    io:format(Fd, T ++ F, [Format,Args])
    end;
write_event(_, _) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec maybe_utc(Time) -> Time | false
%% @end
%% @private
%%--------------------------------------------------------------------
maybe_utc(Time) ->
    UTC = case application:get_env(sasl, utc_log) of
              {ok, Val} ->
                  Val;
              undefined ->
                  %% Backwards compatible:
                  case application:get_env(stdlib, utc_log) of
                      {ok, Val} ->
                          Val;
                      undefined ->
                          false
                  end
          end,
    if
        UTC =:= true ->
            {utc, calendar:local_time_to_universal_time_dst(Time)};
        true ->
            Time
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec format_report(Rep) -> void()
%% @end
%% @private
%%--------------------------------------------------------------------
format_report(Rep) when is_list(Rep) ->
    case string_p(Rep) of
	true ->
	    io_lib:format("~s~n",[Rep]);
	_ ->
	    format_rep(Rep)
    end;
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).


%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec format_rep(Rep) -> void()
%% @end
%% @private
%%--------------------------------------------------------------------
format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~p~n",[Tag,Data]) ++ format_rep(Rep);
format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);
format_rep(_) ->
    [].

%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec (X, Pid) -> X
%% @end
%% @private
%%--------------------------------------------------------------------
add_node(X, Pid) when is_atom(X) ->
    add_node(atom_to_list(X), Pid);
add_node(X, Pid) when node(Pid) =/= node() ->
    lists:concat([X,"** at node ",node(Pid)," **~n"]);
add_node(X, _) ->
    X.

%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec (Term) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec (X) -> true | false
%% @end
%% @private
%%--------------------------------------------------------------------
string_p1([H|T]) when is_integer(H), H >= $\s, H < 255 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
	true -> string_p1(T);
	_    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.

%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec (Time) -> void()
%% @end
%% @private
%%--------------------------------------------------------------------
write_time(Time) -> write_time(Time, "ERROR REPORT").


%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec (Time, Type) -> void()
%% @end
%% @private
%%--------------------------------------------------------------------
write_time({utc,{{Y,Mo,D},{H,Mi,S}}}, Type) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s UTC ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]);
write_time({{Y,Mo,D},{H,Mi,S}}, Type) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]).
%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec (X) -> X1
%% @end
%% @private
%%--------------------------------------------------------------------
t(X) when is_integer(X) ->
    t1(integer_to_list(X));
t(_) ->
    "".
%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec (X) -> X1
%% @end
%% @private
%%--------------------------------------------------------------------
t1([X]) -> [$0,X];
t1(X)   -> X.

%%--------------------------------------------------------------------
%% @doc
%% This function was taken from the stdlib error_logger_file_h.erl
%% @spec (MonthNum::integer()) -> MonthName::string()
%% @end
%% @private
%%--------------------------------------------------------------------
month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

