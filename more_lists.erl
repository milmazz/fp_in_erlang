-module(more_lists).
-export([join/2, concat/1, member/2, halve/1, shunt/2]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%
% 2.18 %
%%%%%%%%

% Joining lists together
%
% Here we consider two of the principal functions over lists. The ++ operator
% joins two lists together, and lists:concat/1 joins a list of lists into a
% single list. For example:
%
% "hel"++"lo" = "hello"
% lists:concat(["goo","d","","by","e"]) = "goodbye"
%
% Write your own definitions of these functions. In the case of ++ you’ll need
% to define a function - say, join/2, as you can’t define your own operators in
% Erlang.
%
% Hint: Think about how you could use join (or ++) in the definition of concat.
%

% Step-by-step evaluation
%
% join([1, 2], [3, 4])
% [1|join([2], [3, 4])]
% [1|[2|join([], [3, 4])]]
% [1|[2|[3,4]]]
% [1,2,3,4]
%
-spec join(list(), list()) -> list().
join([], []) ->
  [];
join([], [_X|_Xs] = R) ->
  R;
join([_X|_Xs] = L, []) ->
  L;
join([X|Xs], R) ->
  [X|join(Xs, R)].

join_test() ->
  ?assertEqual([], join([], [])),
  ?assertEqual([1, 2, 3], join([1, 2, 3], [])),
  ?assertEqual([4, 5], join([], [4, 5])),
  ?assertEqual("hello", join("he", "llo")),
  ?assertEqual([1, 2, 3, 4, 5], join([1, 2, 3], [4, 5])).

% Step-by-step evaluation
%
% concat(["goo","d","","by","e"])
% concat([join("goo", "d") | Rest])
% concat(["good" | Rest])
% concat([join("good", "") | Rest])
% concat(["good" | Rest])
% concat([join("good", "by") | Rest])
% concat(["goodby"|["e"|[]])
% concat([join("goodby", "e") | []])
% concat(["goodbye"|[]])
% "goodbye"
%
-spec concat(list()) -> list().
concat([]) ->
  [];
concat([X|[]]) ->
  X;
concat([X|[Y|Zs]]) ->
  concat([join(X, Y)|Zs]).

concat_test() ->
  ?assertEqual([1], concat([[1]])),
  ?assertEqual([], concat([])),
  ?assertEqual([1,2,3,4,5,6,7,8,9], concat([[1, 2, 3], [4, 5], [6], [7, 8, 9]])),
  ?assertEqual("goodbye", concat(["goo","d","","by","e"])).

% Testing membership
%
% Define a function member/2 that tests whether its first argument is a member
% of its second argument, which is a list. For example:
%
% Step-by-step evaluation
%
% member(0,[2,0,0,1])
% member(0, [2|[0,0,1]])
% member(0, [0, 0, 1])
% true
%
-spec member(term(), list()) -> boolean().
member(_N, []) ->
  false;
member(_X, [_X|_Xs]) ->
  true;
member(N, [_X|Xs]) ->
  member(N, Xs).

member_test() ->
  ?assertEqual(false, member(5, [])),
  ?assertEqual(true, member(2,[2,0,0,1])),
  ?assertEqual(false, member(20,[2,0,0,1])).

% Sorting lists
%
% A list can be sorted in a number of ways, including these algorithms
% described informally:
%
% Merge sort: divide the list into two halves of (approximately) equal length,
% sort them (recursively) and then merge the results.
%
% Quicksort: split the list into two according to whether the items are smaller
% than (or equal to) or larger than the pivot, often taken to be the head
% element of the list; sort the two halves and join the results together.
%
% Insertion sort: sort the tail of the list and then insert the head of the
% list in the correct place.
%
% Try to implement each of these sorting algorithms in Erlang.

halve([]) ->
  [];
halve(L) ->
  halve(L, L).

halve([_H|_T], []) ->
  [];
halve([_H|_T], [_X|[]]) ->
  [];
halve([H|T], [_X|[_Y|Zs]]) ->
  [H|halve(T, Zs)].

halve_test() ->
  ?assertEqual([1, 2, 3], halve([1, 2, 3, 4, 5, 6])),
  ?assertEqual([1, 2, 3], halve([1, 2, 3, 4, 5, 6, 7])).

% Permutations
% A permutation of a list xs consists of the same elements in a (potentially)
% different order. Define a function that gives all the permutations of a list,
% in some order. For example:
%
% perms([]) = [[]]
% perms([1,2,3]) = [[1,2,3],[2,3,1],[3,1,2],[2,1,3],[1,3,2],[3,2,1]]
%


%%%%%%%%%%%
% Helpers %
%%%%%%%%%%%
shunt([], Acc) ->
  Acc;
shunt([X|Xs], Acc) ->
  shunt(Xs, [X|Acc]).

shunt_test() ->
  ?assertEqual([1, 2, 3], shunt([3, 2, 1], [])),
  ?assertEqual([1, 2, 3, 4, 5], shunt([3, 2, 1], [4, 5])).
