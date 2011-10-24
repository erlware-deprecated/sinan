-module(sint_default_help).

-include_lib("eunit/include/eunit.hrl").

-export([given/3, 'when'/3, then/3]).

given([a,generated,project], _State, _) ->
    sint_test_project_gen:a_generated_project().

'when'([a, step, is, run, on, the,
        project, with, an, empty, arglist], {ProjectDir, ProjectName}, _) ->
    Ret = sinan:main(["-s", ProjectDir]),
    ?assertMatch({_, _}, Ret),
    {ok, {ProjectDir, ProjectName, Ret}}.

then([sinan, should, run, normally],
     {ProjectDir, ProjectName, Ret}, _) ->
    ?assertMatch({ok, _}, Ret),
    {ok, BuildState} = Ret,
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    {ok, {ProjectDir, ProjectName, BuildState}};
then([display, a, list, 'of',
      commands, 'and', help, usage],
     State = {_, _, BuildState}, _) ->
    DisplayedCommands = sin_state:get_value(help_displayed, BuildState),
    ?assertMatch({command_list, _}, DisplayedCommands),
    {ok, State}.
