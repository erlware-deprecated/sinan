%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% @author Joe Armstrong
%%  @author Eric Merritt
%%% @doc
%%%  This is a pretty simple topological sort for erlang. It
%%%  was originally written for ermake by Joe Armstrong back in '98.
%%%  --
%%%  -type([{X, X}]) -&gt; {ok, [{X,Y}]} | {cycle, [{X,Y}]}
%%%  topological_sort:pairs(L)
%%%
%%%  A partial order on the set S is a set of pairs {Xi,Xj} such that
%%%  some relation between Xi and Xj is obeyed.
%%%
%%%  A topological sort of a partial order is a sequence of elements
%%%  [X1, X2, X3 ...] such that if whenever {Xi, Xj} is in the partial
%%%  order i &lt; j
%%% @end
%%%-------------------------------------------------------------------
-module(eta_topo).

-include("eunit.hrl").

-export([sort/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Do a topological sort on the list of pairs.
%% @spec (Pairs) -> {ok, L1} | {cycle, Pairs}
%% @end
%%--------------------------------------------------------------------
sort(Pairs) ->
    iterate(Pairs, [], all(Pairs)).

%%====================================================================
%% Internal Functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Iterate over the system.
%% @spec (L1, L2, All) -> {ok, L3}
%% @end
%% @private
%%--------------------------------------------------------------------
iterate([], L, All) ->
    {ok, remove_duplicates(L ++ subtract(All, L))};
iterate(Pairs, L, All) ->
    case subtract(lhs(Pairs), rhs(Pairs)) of
	[]  ->
	    {cycle, Pairs};
	Lhs ->
	    iterate(remove_pairs(Lhs, Pairs), L ++ Lhs, All)
    end.

all(L) ->
    lhs(L) ++ rhs(L).
lhs(L) ->
    lists:map(fun({X,_}) -> X end, L).
rhs(L) ->
    lists:map(fun({_,Y}) -> Y end, L).

%%--------------------------------------------------------------------
%% @doc
%%  all the elements in L1 which are not in L2
%% @spec (L1, L2) -> L3
%% @end
%% @private
%%--------------------------------------------------------------------
subtract(L1, L2) ->
    lists:filter(fun(X) ->
                         not lists:member(X, L2)
                 end, L1).

%%--------------------------------------------------------------------
%% @doc
%%  remove dups from the list.
%% @spec (List1) -> List2
%% @end
%% @private
%%--------------------------------------------------------------------
remove_duplicates([H|T]) ->
  case lists:member(H, T) of
      true  ->
          remove_duplicates(T);
      false ->
          [H|remove_duplicates(T)]
  end;
remove_duplicates([]) ->
    [].

%%-------------------------------------------------------------------
%% @doc
%%   removes all pairs from L2 where the first element
%%   of each pair is a member of L1
%%
%%   L2' L1 = [X] L2 = [{X,Y}].
%% @spec (L1, L2) -> L3
%% @end
%% @private
%%-------------------------------------------------------------------
remove_pairs(L1, L2) ->
    lists:filter(fun({X,_Y}) ->
                         not lists:member(X, L1)
                 end, L2).

%%====================================================================
%% Tests
%%====================================================================
topo_1_test() ->
    Pairs = [{1,2},{2,4},{4,6},{2,10},{4,8},{6,3},{1,3},
             {3,5},{5,8},{7,5},{7,9},{9,4},{9,10}],
    ?assertMatch({ok, [1,7,2,9,4,6,3,5,8,10]},
                 sort(Pairs)).
topo_2_test() ->
    Pairs = [{app2, app1}, {zapp1, app1}, {stdlib, app1},
             {app3, app2}, {kernel, app1}, {kernel, app3},
             {app2, zapp1}, {app3, zapp1}, {zapp2, zapp1}],
    ?assertMatch({ok, [stdlib, kernel, zapp2,
                       app3, app2, zapp1, app1]},
                 sort(Pairs)).

topo_3_test() ->
    Pairs = [{app2, app1}, {app1, app2}, {stdlib, app1}],
    ?assertMatch({cycle, [{app2, app1}, {app1, app2}]},
                 sort(Pairs)).

