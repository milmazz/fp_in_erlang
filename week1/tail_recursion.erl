-module(tail_recursion).
-export([fib/1, perfect/1]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%
% 1.21 %
%%%%%%%%
%
% Fibonacci numbers
% The Fibonacci sequence is given by 0, 1, 1, 2, 3, 5, … where subsequent values
% are given by adding the two previous values in the sequence.
%
% Define an efficient Fibonacci function fib/3 using a tail recursion with two
% accumulating parameters that hold the last two Fibonacci numbers. Give a
% step-by-step evaluation of fib(4)
%
% Step-by-step evaluation
%
% fib(4)
% fib(4, 0, 1)
% fib(3, 1, 1)
% fib(2, 1, 2)
% fib(1, 2, 3)
% fib(0, 3, 5)
% 3
%
fib(N) when N > -1 -> fib(N, 0, 1).

fib(0, A, _) -> A;
fib(N, A, B) -> fib(N-1, B, A+B).

fib_test() ->
  ?assertEqual(0, fib(0)),
  ?assertEqual(1, fib(1)),
  ?assertEqual(3, fib(4)),
  ?assertException(error, function_clause, fib(-1)).

% Perfect numbers
% A positive integer is perfect when it is the sum of its divisors,
% e.g. 6=1+2+3, 28=1+2+4+7+14.
%
% Define a function perfect/1 that takes a positive number N and returns a
% boolean which indicates whether or not the number is perfect. You may well
% want to use an accumulating parameter to hold the sum of the divisors “so far”.
%
%
% perfect(6)
% perfect(6, 1, 0)
% perfect(6, 2, 1)
% perfect(6, 3, 3)
% perfect(6, 4, 6)
% perfect(6, 5, 6)
% perfect(6, 6, 6)
% true

perfect(N) when N > 0 -> perfect(N, 1, 0).

perfect(N, N, A) -> N == A;
perfect(N, C, A) when N rem C == 0 -> perfect(N, C+1, A+C);
perfect(N, C, A) -> perfect(N, C+1, A).

perfect_test() ->
  ?assert(perfect(6)),
  ?assertNot(perfect(27)),
  ?assert(perfect(28)),
  ?assertException(error, function_clause, perfect(-1)).