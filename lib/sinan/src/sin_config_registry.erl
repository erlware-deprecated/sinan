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
%%%  Provides name/build id registration for build projects.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created : 11 Mar 2007 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_config_registry).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get_by_build_id/1,
         get_canonical/1,
         register_canonical/2,
         register_config/2,
         unregister_config/1,
         unregister_canonical/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {build_ids, canonicals}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%%  Find the pid registered by the build id.
%%
%% @spec (BuildId) -> Pid
%% @end
%%--------------------------------------------------------------------
get_by_build_id(BuildId) ->
    gen_server:call(?SERVER, {get_by_build_id, BuildId}).

%%--------------------------------------------------------------------
%% @doc
%%  get the cononical build id thats going to represent the seed
%%  for other builds.
%%
%% @spec (File) -> Pid
%% @end
%%--------------------------------------------------------------------
get_canonical(File) ->
    gen_server:call(?SERVER, {get_canonical, File}).

%%--------------------------------------------------------------------
%% @doc
%%  register a pid at the name. If the name already exists
%%  associate withthe new pid.
%%
%% @spec register_config(Name, Pid) -> ok
%% @end
%%--------------------------------------------------------------------
register_config(BuildId, Pid) ->
    gen_server:cast(?SERVER, {register, BuildId, Pid}).

%%--------------------------------------------------------------------
%% @doc
%%  Register a new canonical build id.
%%
%% @spec register_canonical(ProjectDir, Pid) -> ok
%% @end
%%--------------------------------------------------------------------
register_canonical(ProjectDir, Pid) ->
    gen_server:cast(?SERVER, {register_canonical, ProjectDir, Pid}).

%%--------------------------------------------------------------------
%% @doc
%%  Remove a registered BuildId/pid pair.
%%
%% @spec (BuildId) -> ok
%% @end
%%--------------------------------------------------------------------
unregister_config(BuildId) ->
    gen_server:cast(?SERVER, {unregister, BuildId}).

%%--------------------------------------------------------------------
%% @doc
%%  Remove a registered File/pid pair.
%%
%% @spec (file) -> ok
%% @end
%%--------------------------------------------------------------------
unregister_canonical(File) ->
    gen_server:cast(?SERVER, {unregister_canonical, File}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{build_ids=dict:new(), canonicals=dict:new()}}.

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%%
%% @end
%%--------------------------------------------------------------------
handle_call({get_by_build_id, BuildId}, _From,
            State = #state{build_ids = BuildIds}) ->
    case catch dict:fetch(BuildId, BuildIds) of
        {'EXIT', _} ->
            {reply, undefined, State};
        Reply ->
            {reply, Reply, State}
    end;
handle_call({get_canonical, File}, _From,
            State = #state{canonicals = Canonicals}) ->
    case catch dict:fetch(File, Canonicals) of
        {'EXIT', _} ->
            {reply, undefined, State};
        Reply ->
            {reply, Reply, State}
    end.


%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%%
%% @end
%%--------------------------------------------------------------------
handle_cast({register, BuildId, Pid},
            State = #state{build_ids = BuildIds}) ->
    {noreply, State#state{build_ids = dict:store(BuildId, Pid, BuildIds)}};
handle_cast({unregister, BuildId}, State = #state{build_ids = BuildIds}) ->
    {noreply, State#state{build_ids = dict:erase(BuildId, BuildIds)}};
handle_cast({register_canonical, ProjectDir, Pid},
            State = #state{canonicals = Canonicals}) ->
    {noreply, State#state{canonicals =
                          dict:store(ProjectDir, Pid, Canonicals)}};
handle_cast({unregister_canonical, ProjectDir},
            State = #state{canonicals = Canonicals}) ->
    {noreply, State#state{canonicals = dict:erase(ProjectDir, Canonicals)}}.


%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%%
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
