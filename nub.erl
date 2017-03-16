-module(nub).
-export([nubf/1, nubl/1]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%
% 2.13 %
%%%%%%%%

% This function returns the list with all repetitions of elements removed.

nubf(L) ->
  lists:reverse(nub(L, [])).

nub([], Acc) ->
  Acc;
nub([X|Xs], Acc) ->
  case lists:member(X, Acc) of
    true -> nub(Xs, Acc);
    false -> nub(Xs, [X|Acc])
  end.

nubl(L) ->
  nub(lists:reverse(L), []).

nub_test() ->
  ?assertEqual([2,4,1,3], nubf([2,4,1,3,3,1])),
  ?assertEqual([2,4,3,1], nubl([2,4,1,3,3,1])).
