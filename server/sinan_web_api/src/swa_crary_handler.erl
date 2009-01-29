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
%%%  Adds the handler interface from crary &lt;-&gt; sinan
%%% @end
%%% Created : 14 Mar 2008 by Eric Merritt <ericmerritt@erlware.org>
%%%-------------------------------------------------------------------
-module(swa_crary_handler).

-include("crary.hrl").
-include("uri.hrl").

%% API
-export([handler/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%  Handle incoming requests for action.
%% @spec (Req, Path) -> ok
%% @end
%%--------------------------------------------------------------------
handler(Req = #crary_req{method = "POST"}, Path) ->
    Body = crary_body:read_all(Req),
    {Args, _, _} = ktj_decode:decode(Body),
    %%remove the first element which is always '/'
    [_ | NewPath] = filename:split(Path),
    BuildRef = sinan:gen_build_ref(),
    handle_do_request(NewPath, Req, BuildRef, Args),
    ok;
handler(Req, _) ->
    crary:not_implemented(Req).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Take the path and various arguments and disptach sinan in the
%%  correct way.
%% @spec (Path, Req, BuildRef, Args) -> ok
%% @end
%%--------------------------------------------------------------------
handle_do_request(["do_task", "build"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:build(BuildRef, Args);
handle_do_request(["do_task", "analyze"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:analyze(BuildRef, Args);
handle_do_request(["do_task", "doc"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:doc(BuildRef, Args);
handle_do_request(["do_task", "shell"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:shell(BuildRef, Args);
handle_do_request(["do_task", "gen"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:gen(BuildRef, Args);
handle_do_request(["do_task", "clean"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:clean(BuildRef, Args);
handle_do_request(["do_task", "help"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:help(BuildRef, Args);
handle_do_request(["do_task", "version"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:version(BuildRef, Args);
handle_do_request(["do_task", "depends"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:depends(BuildRef, Args);
handle_do_request(["do_task", "test"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:test(BuildRef, Args);
handle_do_request(["do_task", "release"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:release(BuildRef, Args);
handle_do_request(["do_task", "dist"], Req, BuildRef, Args) ->
    swa_sup:start_handler(BuildRef, Req),
    sinan:dist(BuildRef, Args);
handle_do_request(["do_task", "shutdown"], Req, _, _) ->
    case application:get_env(sinan, may_shutdown) of
	false ->
	    write_out(Req, "Not allowed to respond to shutdown requests!");
	_ ->
	    write_out(Req, "Shutting down now!"),
	    init:stop()
    end;
handle_do_request(["do_task", Task], Req, _, _) ->
    write_out(Req, "I don't know how to do " ++ Task).

%%--------------------------------------------------------------------
%% @doc
%%  Write out an arbitrary message to the client
%% @spec (Req, Message) -> ok
%% @end
%%--------------------------------------------------------------------
write_out(Req, Message) ->
    Writer = crary_body:new_writer(Req),
    JDesc = {obj, [{type, run_event},
                  {event_type, stop},
                  {desc, list_to_binary(Message)}]},
    Content = ktj_encode:encode(JDesc),
    crary:r(Writer, ok, [{"content-type", "application/json"},
                         {<<"content-length">>,
			  integer_to_list(iolist_size(Content))}]),
    crary_sock:write(Writer, Content),
    crary_sock:done_writing(Writer).
