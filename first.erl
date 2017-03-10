-module(first).
-export([double/1, mult/2, area/3, square/1, treble/1]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%
% 1.9 %
%%%%%%%

% Modifying first.erl
% The module first.erl is available below.

% In a text editor of your choice modify first.erl to include functions to
% square and to treble a value, and test these functions out by calling them
% from the Erlang shell.

mult(X,Y) ->
    X*Y.

double(X) ->
    mult(2,X).

area(A,B,C) ->
    S = (A+B+C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).

square(X) ->
  mult(X, X).

treble(X) ->
  mult(3, X).

square_test() ->
  ?assertEqual(1, first:square(1)),
  ?assertEqual(4, first:square(2)),
  ?assertEqual(9, first:square(3)).

