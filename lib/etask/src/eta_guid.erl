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
%%% @author Caoyuan <dcaoyuan@gmail.com>
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  The module provides a way to generate a GUID in
%%%  accord with http://www.ietf.org/rfc/rfc4122.txt
%%%  it implements a version one generator and a version four generator.
%%%  used and released with permision from caoyuan
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created :  8 Dec 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(eta_guid).


%% API
-export([gen_guid_v1/1, gen_guid_v4/0]).

-define(NUM_CHR_UPPER,
        {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$A,$B,$C,$D,$E,$F,
         $G,$H,$I,$J,$K,$L,$M,$N,$O,$P,$Q,$R,$S,$T,$U,$V,
         $W,$X,$Y,$Z}).

-define(NUM_CHR_LOWER,
        {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f,
         $g,$h,$i,$j,$k,$l,$m,$n,$o,$p,$q,$r,$s,$t,$u,$v,
         $w,$x,$y,$z}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Generates a GUID per http://www.ietf.org/rfc/rfc4122.txt
%% (version 1 generator)
%%--------------------------------------------------------------------
gen_guid_v1(MacAddress) ->
    TimeIn100NanosBeg = calendar:datetime_to_gregorian_seconds({{1582,
                                                                 10, 15}, {0, 0, 0}}) * 10000000,
    {_MegaSecs, _Secs, MicroSecs} = now(),
    TimeIn100NanosNow =
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) *
        10000000 + MicroSecs * 10,
    Time = TimeIn100NanosNow - TimeIn100NanosBeg,
    TimeStr = number_to_based_str(Time, 16, 2),
    TimeStr1 =
        case 15 - length(TimeStr) of
            Rem when Rem > 0 -> lists:duplicate(Rem, $0) ++ TimeStr;
            _                -> TimeStr
        end,
    %% add version number 1,
    TimeHiV = lists:sublist(TimeStr1, 1, 3) ++ "1",
    TimeMid = lists:sublist(TimeStr1, 4, 4),
    TimeLow = lists:sublist(TimeStr1, 8, 8),
    %% multiplexed variant type (2 bits)
    ClockSeqHiV = io_lib:format("~2.16.0b", [(random:uniform(256) - 1)
                                         band 16#3f bor 16#80]),
    ClockSeqLow = io_lib:format("~2.16.0b", [(random:uniform(256) - 1)]),
    Node = MacAddress,
    lists:flatten(TimeLow ++ "-" ++ TimeMid ++ "-" ++ TimeHiV ++ "-"
                  ++ ClockSeqHiV ++ ClockSeqLow ++ "-" ++ Node).

%%--------------------------------------------------------------------
%% @doc
%%  Generates a random GUID per
%%  http://www.ietf.org/rfc/rfc4122.txt (version 4 generator)
%%  e.g. output: 372472a2-d557-4630-bc7d-bae54c934da1
%%  word*2-, word-, (w)ord-, (w)ord-, word*3
%%
%% @spec gen_guid_v4() -> string()
%% @end
%%--------------------------------------------------------------------
gen_guid_v4() ->
    lists:flatten(lists:foldl(
                    fun (I, Acc) ->
                            %% return random number between 0, 255
                            B = random:uniform(256) - 1,
                            %% multiplex version number (4 bits)
                            S = if  I == 7 ->
                                        %% version 4 (random)
                                        %% The last 0 in 2.16.0 means fill with
                                        %% leading 0 if necessay
                                        B1 = B band 16#0f bor 16#40,
                                        io_lib:format("~2.16.0b", [B1]);
                                    %% multiplexed variant type (2 bits)
                                    I == 9 ->
                                        B1 = B band 16#3f bor 16#80,
                                        io_lib:format("~2.16.0b", [B1]);
                                    I == 4; I == 6; I == 8; I == 10 ->
                                        io_lib:format("~2.16.0b-", [B]);
                                    true ->
                                        io_lib:format("~2.16.0b", [B])
                                end,
                            Acc ++ S
                    end, [], lists:seq(1, 16))).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  convert number to alternate string base.
%% @spec number_to_based_str(Number, Base, Digits) -> NewBasedNumber
%% @end
%%--------------------------------------------------------------------
number_to_based_str(Number, Base, Digits) when is_integer(Digits) ->
   Str = number_to_based_str(Number, Base, []),
   case Digits - length(Str) of
       Rem when Rem > 0 ->
           lists:duplicate(Rem, $0) ++ Str;
       _ ->
           Str
   end;
number_to_based_str(Number, Base, _Acc) when Number < Base ->
   [element(Number + 1, ?NUM_CHR_LOWER)];
number_to_based_str(Number, Base,  Acc) ->
   Msd = Number div Base,
   Lsd = Number rem Base,
   case Msd >= Base of
       true  -> number_to_based_str(Msd, Base,
                                    [element(Lsd + 1,
                                             ?NUM_CHR_LOWER)|Acc]);
       false -> [element(Msd + 1, ?NUM_CHR_LOWER) |
                 [element(Lsd + 1,
                          ?NUM_CHR_LOWER)|Acc]]
   end.


