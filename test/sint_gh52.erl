%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Test the ability to set a home directory and have a top level dir
%%% @end
%%%-------------------------------------------------------------------
-module(sint_gh52).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([a,generated,project], _State, _) ->
    sint_test_project_gen:a_generated_project(erlang:atom_to_list(?MODULE));
given([a,'non-standard',home,directory], {PD, PN}, _) ->
    NewHomeDir = ec_file:mkdtemp(),
    {ok, {PD, PN, NewHomeDir}};
given([a,sinan,config,in,that,home,directory], State={_, _, NewHomeDir}, _) ->
    TopSinConfig = filename:join(NewHomeDir, ".sinan.config"),
    ok = file:write_file(TopSinConfig, config_contents()),
    {ok, State}.

'when'([a,home,directory,is,specified,on,the,command,line], State, _) ->
    %% This is a placeholder/reminder the actually command line is
    %% specified next
    {ok, State};
'when'([a,build,step,is,run,on,this,project],
       {ProjectDir, PN, NewHomeDir}, _) ->
    Ret = sinan:run_sinan(["-u", NewHomeDir, "-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    {_, TrueRet} = Ret,
    {ok, {ProjectDir, PN, TrueRet, NewHomeDir}}.

then([sinan,uses,the,config,in,the,alternate,home,directory],
     State = {_ProjectDir, PN, _TrueRet, _HD}, _) ->
    ModuleName = erlang:list_to_atom(PN ++ "_app"),
    {compile, Options} = proplists:lookup(compile, ModuleName:module_info()),
    {options, Opts} = proplists:lookup(options, Options),
    ?assert(false == lists:member(debug_info, Opts)),
    {ok, State};
then([_,the,app,normally], State = {_, _, BuildState, _}, _) ->
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    {ok, State};
then([have,the,home,directory,correctly,specified,in,the,build,state],
     State = {_, _, BuildState, HomeDir}, _) ->
    ?assertMatch(HomeDir, sin_state:get_value(home_dir, BuildState)),
    {ok, State}.


config_contents() ->
    "{compile_args, []}.".
