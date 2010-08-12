%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2010 Erlware
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
%%% @copyright (C) 2008-2010 Eric Merritt
%%% @doc
%%%  Strait forward application used to start the sinan interface
%%% @end
%%% Created : 14 Mar 2008 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(swa_app).

-behaviour(application).

%% system defines
-define(DEFAULT_PORT, 8599).


%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%% @end
%%--------------------------------------------------------------------
start(_Type, _StartArgs) ->
    start_crary(),
    case swa_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    Port = get_port(),
    crary:stop({{127,0,0,1}, Port}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Starts crary with the correct port and handler information.
%% @spec start_crary() -> ok
%% @end
%%--------------------------------------------------------------------
start_crary() ->
    Port = get_port(),
    Opts = get_socket_opts(),
    crary:start(
      {{127,0,0,1}, Port},
      fun swa_crary_handler:handler/2,
      [{socket_opts, Opts}]).

%%--------------------------------------------------------------------
%% @doc
%%   Get the port from the system. This looks in two places
%%   for port information.
%%
%%   <ol>
%%    <li>An environmental variable called <strong>SINAN_LISTEN_PORT</strong></li>
%%    <li>An application evn variable called <strong>port</strong></li>
%%  </ol>
%%     If it doesn't find a port in either place it uses its default port.
%%
%%   It is also possible to pass raw socket options in the env variable <strong>socket_opts.</strong> This is useful, for example,
%%   when debugging, to set reuseaddr option so that we can reboot sinan after a crash without having to wait for the socket timeout
%%   or changing the port. For example:
%%
%% `application:set_env(sinan_web_api, socket_opts, [{reuseaddr, true}]),'
%%
%% @spec get_port() -> integer()
%% @end
%%--------------------------------------------------------------------
get_port() ->
    case get_option(port, ?DEFAULT_PORT) of
        Port when is_list(Port) ->
            list_to_integer(Port);
        Port when is_integer(Port) ->
            Port
    end.

get_socket_opts() ->
    get_option(socket_opts, []).

get_option(Option, Default) ->
    case application:get_env(sinan_web_api, Option) of
	undefined ->
	    Default;
	{ok, Value} ->
	    Value
    end.
