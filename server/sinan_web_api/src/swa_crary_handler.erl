%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2007 Erlware
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
%%% @author Eric Merritt <ericmerritt@erlware.org>
%%% @copyright (C) 2008, Eric Merritt
%%% @doc
%%%  Adds the handler interface from crary <-> sinan
%%% @end
%%% Created : 14 Mar 2008 by Eric Merritt <ericmerritt@erlware.org>
%%%-------------------------------------------------------------------
-module(swa_crary_handler).

-include("crary.hrl").
-include("uri.hrl").

%% API
-export([handler/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%  Handle incoming requests for action.
%% @spec (Req) -> ok
%% @end
%%--------------------------------------------------------------------
handler(#crary_req{method = "GET"}) ->
    %% Set up get API
    ok;
handler(Req = #crary_req{method = "POST", uri = #uri{path=Path}}) ->
    Body = crary_body:read_all(Req),
    Args = ktj_decode:decode(Body),
    NewPath = filename:split(Path),
    BuildRef = sinan:gen_build_ref(),
    handle_do_request(NewPath, Req, BuildRef, Args),
    ok;
handler(Req) ->
    crary:not_implemented(Req).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Take the path and various arguments and disptach sinan in the
%%  correct way.
%% @spec handle_do_request(Path, Req, BuildRef, Args) -> ok
%% @end
%%--------------------------------------------------------------------
handle_do_request(["do_task", "build"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:build(BuildRef, Args);
handle_do_request(["do_task", "analyze"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:analyze(BuildRef, Args);
handle_do_request(["do_task", "doc"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:doc(BuildRef, Args);
handle_do_request(["do_task", "shell"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:shell(BuildRef, Args);
handle_do_request(["do_task", "gen"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:gen(BuildRef, Args);
handle_do_request(["do_task", "clean"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:clean(BuildRef, Args);
handle_do_request(["do_task", "help"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:help(BuildRef, Args);
handle_do_request(["do_task", "depends"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:depends(BuildRef, Args);
handle_do_request(["do_task", "test"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:test(BuildRef, Args);
handle_do_request(["do_task", "release"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:release(BuildRef, Args);
handle_do_request(["do_task", "dist"], Req, BuildRef, Args) ->
    sinan:add_build_event_handler(swa_event_handler, [Req, BuildRef]),
    sinan:dist(BuildRef, Args);
handle_do_request(Path, _, _, _) ->
    throw({unknow_task, Path}).

