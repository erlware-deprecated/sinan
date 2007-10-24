%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
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
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%  Starts the build process as well as the 'config' handler.
%%% @end
%%% Created : 30 Oct 2006 by Eric Merritt <cyberlync@gmail.com>
%%%--------------------------------------------------------------------------
-module(sin_builder).

-behaviour(gen_server).

%% API
-export([start_link/0, main/0, main/1, main/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}.
%% 
%% @doc 
%% Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @spec main() -> ok.
%% 
%% @doc 
%%  Starts the build with the target task undefined.
%% @end
%%--------------------------------------------------------------------
main() ->
    main(undefined, []).

%%--------------------------------------------------------------------
%% @spec main([Task::string(), Args::list()) -> ok.
%% 
%% @doc 
%%  Starts building the task (first element in the argument list) 
%%  with the specified args.
%% @end
%%--------------------------------------------------------------------
main([Task | Args]) when is_list(Task) ->
    main(list_to_atom(Task), Args);

%%--------------------------------------------------------------------
%% @spec main(Task::string()) ->
%% 
%% @doc 
%%  Start building with the task specified as a string (no arguments).
%% @end
%%--------------------------------------------------------------------
main(Task) when is_list(Task) ->
    main(list_to_atom(Task), []);

%%--------------------------------------------------------------------
%% @spec main(Task::atom()) -> ok.
%% 
%% @doc 
%%  Start building with the task specified as an atom (no arguments).
%% @end
%%--------------------------------------------------------------------
main(Task) when is_atom(Task) ->
    main(Task, []).


%%--------------------------------------------------------------------
%% @spec main(Task::string(), Args::list()) -> ok.
%% 
%% @doc 
%%  run the task with the specified arguments.
%% @end
%%--------------------------------------------------------------------
main(Task, Args) when is_atom(Task) ->
    gen_server:cast(?SERVER, {start_run, Task, Args}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}.
%% 
%% @doc 
%% Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call({start_run, Task, Args}, _From, State) ->
    sin_build_sup:start_build(Task, Args),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}.
%% 
%% @doc 
%% Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast({start_run, Task, Args}, State) ->
    sin_build_sup:start_build(Task, Args),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}.
%% 
%% @doc 
%% Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void().
%% 
%% @doc 
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}.
%% 
%% @doc 
%% Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
