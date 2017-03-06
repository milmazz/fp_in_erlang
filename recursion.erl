-module(recursion).
-export([fib/1]).
-include_lib("eunit/include/eunit.hrl").

% Fibonacci numbers
%
% Give a recursive definition of the function fib/1 computing
% the Fibonacci numbers, and give a step-by-step evaluation of fib(4).
%
% fib(4)
% fib(3) + fib(2)
% fib(2) + fib(1) + fib(1) + fib(0)
% fib(1) + fib(0) + 1 + 1 + 1
% 1 + 1 + 3
% 5
%
fib(0) -> 1;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

fib_test() ->
  ?assertEqual(1, fib(0)),
  ?assertEqual(1, fib(1)),
  ?assertEqual(5, fib(4)),
  ?assertException(error, function_clause, fib(-1)).

% How many pieces?
%
% Define a function pieces so that pieces(N) tells you the maximum number of
% pieces into which you can cut a piece of paper with N straight line cuts.
%