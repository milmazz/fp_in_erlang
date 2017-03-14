-module(recursive_functions).
-export([double/1, evens/1]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%
% 2.9 %
%%%%%%%

% Constructing lists with recursive functions

% The aim of these exercises is to familiarise you with other ways of defining
% functions over lists in Erlang, in particular the different way that recursive
% functions can construct lists.

% Transforming list elements
% Define an Erlang function double/1 to double the elements of a list of numbers.
%
% Step-by-step evaluation
% double([1, 2, 3])
% [2 | double([2, 3])]
% [2 | [4 | double([3])]]
% [2 | [4 | [6 | double([])]]]
% [2 | [4 | [6 | []]]]
% [2, 4, 6]
%
double([]) ->
    [];
double([X|Xs]) when is_number(X) ->
    [2 * X | double(Xs)].

double_test() ->
    ?assertEqual([], double([])),
    ?assertEqual([2, 4, 6], double([1, 2, 3])).

% Filtering lists
% Define a function evens/1 that extracts the even numbers from a list of integers.
%
% Step-by-step evaluation
%
% evens([1, 2, 3]) -> X: 1, Xs: [2, 3]
% evens([2, 3]) -> X: 2, Xs: [3]
% [2 | evens([3])] -> X: 3, Xs: []
% [2 | evens([])]
% [2 | []]
% [2]
%
evens([]) ->
    [];
evens([X|Xs]) when is_integer(X), X rem 2 == 0 ->
    [X | evens(Xs)];
evens([_X|Xs]) ->
    evens(Xs).

evens_test() ->
    ?assertEqual([], evens([])),
    ?assertEqual([2, 4, 6], evens([1, 2, 3, 4, 5, 6])),
    ?assertEqual([], evens([1, 3, 5])).

% Going further

% If you want to try some other recursions on lists try to define functions to
% give:

% the median of a list of numbers: this is the middle element when the list is
% ordered (if the list is of even length you should average the middle two)

% the modes of a list of numbers: this is a list consisting of the numbers that
% occur most frequently in the list; if there is is just one, this will be a list
% with one element only

% In doing this you might find it useful to think of other functions that you
% could define to help you solve these problems, such as a function to sort a
% list, or to work out how many times a value occurs in a particular list.

% sort([], _callback) ->
%   [];
% sort([X|Y|Zs], callback \\ fun(X, Y) -> X >= Y end) ->
