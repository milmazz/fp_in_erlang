-module(lists_practice).
-export([product/1, maximum/1, maxt/1]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%
% 2.6 %
%%%%%%%

% Combining list elements: the product of a list
%
% Using the template from the last session, define an Erlang function to give
% the product of a list of numbers. The product of an empty list is usually
% taken to be 1: why?

% Step-by-step evaluation
%
% product([2, 3, 4])
% product([2, 3, 4], 1)
% product([3, 4], 2)
% product([4], 6)
% product([], 24)
% 24
product(N) ->
  product(N, 1).

product([], Acc) ->
  Acc;
product([X|Xs], Acc) ->
  product(Xs, X * Acc).

product_test() ->
  ?assertEqual(24, product([2, 3, 4])).

% Combining list elements: the maximum of a list
%
% Define an Erlang function to give the maximum of a list of numbers.
%
% You might find it helpful to use the function max/2 that gives the maximum of
% two values.
%
% It’s not obvious what should be the value for the maximum of an empty list of
% numbers. You could therefore choose to define maximum on lists with at least
% one element only: to do this you will need to change the base case of the
% template.

% Step-by-step evaluation
%
% maximum([3, 1, 2, 4])
% max(3, maximum([1, 2, 4])
% max(3, max(1, maximum([2, 4])))
% max(3, max(1, max(2, maximum([4|[]]))))
% max(3, max(1, max(2, 4)))
% max(3, max(1, 4))
% max(3, 4)
% 4
maximum([X|[]]) ->
  X;
maximum([X|Xs]) ->
  max(X, maximum(Xs)).

% maxt([3, 1, 2, 4])
% maxt([1, 2, 4], 3)
% maxt([2, 4], max(3, 1))
% maxt([2, 4], 3)
% maxt([4], max(3, 2))
% maxt([4|[]], 3)
% max(3, 4)
% 4
%

maxt([X|[]]) ->
  X;
maxt([X|Xs]) ->
  maxt(Xs, X).

maxt([X|[]], Acc) ->
  max(Acc, X);
maxt([X|Xs], Acc) ->
  maxt(Xs, max(Acc, X)).

maximum_test() ->
  L = [3, 1, 2, 4],
  ?assertEqual(maxt(L), maximum(L)),
  ?assertEqual(2, maxt([2])),
  ?assertException(error, function_clause, maxt([])),
  ?assertException(error, function_clause, maximum([])).
