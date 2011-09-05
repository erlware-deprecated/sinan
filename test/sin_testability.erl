%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Test the ability to specify a start directory
%%% @end
%%% Created :  5 Sep 2011 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_testability).

-export([given/2, 'when'/2, then/2, step/2]).

% Step definitions for the sample calculator Addition feature.

given([a, generated, project, in, a, different, location, then, the, 'CWD'],
      _) ->
    io:format("~p~n", [file:get_cwd()]).

'when'([a, build, step, is, run, on, this, project], _) ->
    ok;
'when'([a, start, dir, is, passed, to, the, build], _) ->
    ok.

then([sinan, should, build, the, project, in, the, location,
      specified, by, the, start, dir], _) ->
    ok.

step(_, _) -> undefined.
