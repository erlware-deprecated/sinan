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
%%%  Provides the ability to ask questions of the user and
%%%  get a response.
%%% @end
%%% @copyright 2006-2010
%%%---------------------------------------------------------------------------
-module(sin_talk).

%% API
-export([ask/1,
	 ask/2,
	 ask_default/2,
	 ask_default/3,
	 ask/3,
	 say/1,
	 say/2]).

-export_type([prompt/0,
	      type/0,
	      supported/0]).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Types
%%====================================================================
-type prompt() :: string().
-type type() :: boolean | number | string.
-type supported() :: string() | boolean() | number().

%%====================================================================
%% API
%%====================================================================
%%-------------------------------------------------------------------
%% @doc
%%  Outputs the line to the screen
%% @end
%%-------------------------------------------------------------------
-spec say(string()) -> ok.
say(Say) ->
    io:format(lists:flatten([Say, "~n"])).

-spec say(string(), [term()] | term()) -> ok.
say(Say, Args) when is_list(Args) ->
    io:format(lists:flatten([Say, "~n"]), Args);
say(Say, Args) ->
    io:format(lists:flatten([Say, "~n"]), [Args]).

%%-------------------------------------------------------------------
%% @doc
%%  Asks the user for a response to the specified prompt.
%% @end
%%-------------------------------------------------------------------
-spec ask(prompt()) -> string().
ask(Prompt) ->
    ask_convert(Prompt, fun get_string/1, string, none).

%%-------------------------------------------------------------------
%% @doc
%%  Asks the user for a response to the specified prompt.
%% @end
%%-------------------------------------------------------------------
-spec ask_default(prompt(), string()) -> string().
ask_default(Prompt, Default) ->
    ask_convert(Prompt, fun get_string/1, string, Default).

%%-------------------------------------------------------------------
%% @doc
%%  Asks the user to respond to the prompt. Trys to return the value
%%  in the format specified by 'Type'.
%% @end
%%-------------------------------------------------------------------
-spec ask(prompt(), type()) ->  supported().
ask(Prompt, boolean) ->
    ask_convert(Prompt, fun get_boolean/1, boolean, none);
ask(Prompt, number) ->
    ask_convert(Prompt, fun get_integer/1, number,  none);
ask(Prompt, string) ->
    ask_convert(Prompt, fun get_integer/1, string, none).


%%-------------------------------------------------------------------
%% @doc
%%  Asks the user to respond to the prompt. Trys to return the value
%%  in the format specified by 'Type'.
%% @end
%%-------------------------------------------------------------------
-spec ask_default(prompt(), type(), supported()) ->  supported().
ask_default(Prompt, boolean, Default) ->
    ask_convert(Prompt, fun get_boolean/1, boolean, Default);
ask_default(Prompt, number, Default) ->
    ask_convert(Prompt, fun get_integer/1, number, Default);
ask_default(Prompt, string, Default) ->
    ask_convert(Prompt, fun get_integer/1, string, Default).



%%-------------------------------------------------------------------
%% @doc
%%  Asks the user to respond to the number prompt with a value between
%%  min and max.
%% @end
%%-------------------------------------------------------------------
-spec ask(prompt(), number(), number()) -> number().
ask(Prompt, Min, Max) ->
    Res = ask(Prompt, fun get_integer/1, none),
    case (Res >= Min andalso Res =< Max) of
        true ->
            Res;
        false ->
            say("Your answer must be between ~w and ~w!", [Min, Max]),
            ask(Prompt, Min, Max)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
%%-------------------------------------------------------------------
%% @doc
%%  Actually does the work of asking, checking result and translating
%% result into the requested format.
%% @end
%% @private
%%-------------------------------------------------------------------
-spec ask_convert(prompt(), fun(), type(), supported()) -> supported().
ask_convert(Prompt, TransFun, Type,  Default) ->
    NewPrompt = Prompt ++ case Default of
			      none ->
				  [];
			      Default ->
				  " (" ++ sin_utils:term_to_list(Default) ++ ")"
			  end ++ "> ",
    Data = trim(io:get_line(NewPrompt)),
    Ret = TransFun(Data),
    case Ret of
	no_data ->
	    case Default of
		none ->
		    say("I didn't get that. This ~p kind of question.~n", [Type]),
		    ask_convert(Prompt, TransFun, Type, Default);
		Default ->
		    Default
	    end;
	no_clue ->
            say("I didn't get that. This ~p kind of question.~n", [Type]),
	    ask_convert(Prompt, TransFun, Type, Default);
	_ ->
            Ret
    end.

%%-------------------------------------------------------------------
%% @doc
%%  Trys to translate the result into a boolean
%% @end
%% @private
%%-------------------------------------------------------------------
-spec get_boolean(string()) -> true | false.
get_boolean([]) ->
    no_data;
get_boolean([$T | _]) ->
    true;
get_boolean([$t | _]) ->
    true;
get_boolean("ok") ->
    true;
get_boolean("OK") ->
    true;
get_boolean([$Y | _]) ->
    true;
get_boolean([$y | _]) ->
    true;
get_boolean([$f | _]) ->
    false;
get_boolean([$F | _]) ->
    false;
get_boolean([$n | _]) ->
    false;
get_boolean([$N | _]) ->
    false;
get_boolean(_) ->
    no_clue.

%%-------------------------------------------------------------------
%% @doc
%%  Trys to translate the result into an integer
%% @end
%% @private
%%-------------------------------------------------------------------
-spec get_integer(string()) -> integer().
get_integer([]) ->
    no_data;
get_integer(String) ->
    case (catch list_to_integer(String)) of
        {'Exit', _} ->
            no_clue;
        Integer ->
            Integer
    end.

%%-------------------------------------------------------------------
%% @doc
%%  Solely returns a string give the string. This is so the same
%% translate function can be used across the board
%% @end
%%-------------------------------------------------------------------
-spec get_string(string()) -> string().
get_string([]) ->
    no_data;
get_string(String) ->
    case is_list(String) of
	true ->
	    String;
	false ->
	    no_clue
    end.

%%--------------------------------------------------------------------
%% @spec trim(String::string()) -> NewString::string().
%% @doc
%% Helper function that removes whitespace from both sides of the
%% string.
%% @end
%%--------------------------------------------------------------------
trim(String) ->
    lists:reverse(strip(lists:reverse(strip(String)))).

%%--------------------------------------------------------------------
%% @spec strip(String::string()) -> NewString::string().
%% @doc
%% Helper function that removes whitespace From the front of a string.
%% @end
%%--------------------------------------------------------------------
strip([$   | Cs]) -> strip(Cs);
strip([$\t | Cs]) -> strip(Cs);
strip([$\r | Cs]) -> strip(Cs);
strip([$\n | Cs]) -> strip(Cs);
strip(Cs) -> Cs.

%%%====================================================================
%%% tests
%%%====================================================================
general_test_() ->
    [?_test(42 == get_integer("42")),
     ?_test(500211 == get_integer("500211")),
     ?_test(1234567890 == get_integer("1234567890")),
     ?_test(12345678901234567890 == get_integer("12345678901234567890")),
     ?_test(true == get_boolean("true")),
     ?_test(false == get_boolean("false")),
     ?_test(true == get_boolean("Ok")),
     ?_test(true == get_boolean("ok")),
     ?_test(true == get_boolean("Y")),
     ?_test(true == get_boolean("y")),
     ?_test(false == get_boolean("False")),
     ?_test(false == get_boolean("No")),
     ?_test(false == get_boolean("no"))].
