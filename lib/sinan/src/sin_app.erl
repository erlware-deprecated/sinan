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
%%%  Application for sinan.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created : 14 Mar 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%%
%% @doc
%% This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    register_tasks(),
    case sin_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @spec (State) -> void()
%%
%% @doc
%% This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%   Register all of the sinan tasks with the etask task system. In
%%   this case each task knows how to register itself. Let
%%   the tasking system worry about persistance.
%% @spec register_tasks() -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
register_tasks() ->
    sin_edoc:start(),
    sin_erl_builder:start(),
    sin_shell:start(),
    sin_gen:start(),
    sin_clean:start(),
    sin_help:start(),
    sin_version:start(),
    sin_depends:start(),
    sin_test:start(),
    sin_release_builder:start(),
    sin_dist_builder:start().


