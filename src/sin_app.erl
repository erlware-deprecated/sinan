%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2011, Erlware
%%% @doc
%%%  The main sinan startup app
%%% @end
%%%-------------------------------------------------------------------
-module(sin_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([manual_start/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @private
start(_StartType, _StartArgs) ->
    case sin_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%% @private
stop(_State) ->
    ok.

%% @doc start up sinan and all of its dependencies in the correct
%% order
-spec manual_start() -> ok.
manual_start() ->
    lists:foreach(fun(App) ->
			  case application:start(App) of
			      {error, {already_started, App}} ->
				  ok;
			      ok ->
				  ok;
			      Error ->
				  throw(Error)
			  end
		  end,
		  [kernel,
		   stdlib,
                   erlware_commons,
		   compiler,
		   syntax_tools,
		   edoc,
		   ktuo,
		   sasl,
		   ibrowse,
		   eunit,
		   ewlib,
		   ewrepo,
		   xtools,
		   xmerl,
		   mnesia,
		   sgte,
		   parsetools,
		   getopt,
		   crypto,
		   proper,
		   sinan]).



%%%===================================================================
%%% Internal functions
%%%===================================================================
