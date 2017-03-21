-module(variables).
-export([maxThree/3, howManyEqual/3]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%
% 1.15 %
%%%%%%%%

% Maximum of three
%
% Give a definition of the function maxThree which takes three integers and
% returns the maximum of the three. You can use the max function, which gives
% the maximum of two numbers, in writing your definition.
%
% maxThree(34,25,36) = 36
%
maxThree(X, Y, Z) ->
  max(max(X, Y), Z).

maxThree_test() ->
  ?assertEqual(36, maxThree(34, 25, 36)).

% How many equal?
%
% Give a definition of the function howManyEqual which takes three integers
% and returns an integer, counting how many of its three arguments are equal,
% so that:
%
% howManyEqual(34,25,36) = 0
% howManyEqual(34,25,34) = 2
% howManyEqual(34,34,34) = 3
%
howManyEqual(_X, _X, _X) -> 3;
howManyEqual(_X, _X, _Y) -> 2;
howManyEqual(_X, _Y, _X) -> 2;
howManyEqual(_Y, _X, _X) -> 2;
howManyEqual(_, _, _) -> 0.

howManyEqual_test() ->
  ?assertEqual(0, howManyEqual(34, 25, 36)),
  ?assertEqual(2, howManyEqual(34, 25, 34)),
  ?assertEqual(3, howManyEqual(34, 34, 34)).

