-module(recursion).
-export([fib/1]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%
% 1.19 %
%%%%%%%%
%
% Fibonacci numbers
%
% Give a recursive definition of the function fib/1 computing
% the Fibonacci numbers, and give a step-by-step evaluation of fib(4).
%
% Step-by-step evaluation
%
% fib(4)
% fib(3) + fib(2)
% (fib(2) + fib(1)) + (fib(1) + fib(0))
% (fib(1) + fib(0) + fib(1)) + (fib(1) + fib(0))
% 3
%
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

fib_test() ->
  ?assertEqual(0, fib(0)),
  ?assertEqual(1, fib(1)),
  ?assertEqual(3, fib(4)),
  ?assertException(error, function_clause, fib(-1)).

% How many pieces?
%
% Define a function pieces so that pieces(N) tells you the maximum number of
% pieces into which you can cut a piece of paper with N straight line cuts.
%
% This principle is called: Lazy caterer's sequence
% See: https://en.wikipedia.org/wiki/Lazy_caterer's_sequence
%
% Step-by-step evaluation
%
% pieces(3)
% 3 + pieces(2)
% 3 + 2 + pieces(1)
% 3 + 2 + 1 + pieces(0)
% 3 + 2 + 1 + 1
% 7
pieces(0) -> 1;
pieces(N) when N > 0 -> N + pieces(N-1).

pieces_test() ->
  ?assertEqual(7, pieces(3)),
  ?assertException(error, function_clause, pieces(-1)).