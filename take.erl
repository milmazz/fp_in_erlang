-module(take).
-export([take/2, take_t/2]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%
% 2.11 %
%%%%%%%%

% Define a function take that takes the first N elements from a list.

% Step-by-step evaluation (direct)
%
% take(4, "hello")
% "h" ++ take(4, "ello", 1)
% "h" ++ "e" ++ take(4, "llo", 2)
% "h" ++ "e" ++ "l" ++ take(4, "lo", 3)
% "h" ++ "e" ++ "l" ++ "l" ++ take(4, "o", 4)
% "h" ++ "e" ++ "l" ++ "l" ++ []
% "hell"

-spec take(integer(), [T]) -> [T].
take(_, []) ->
  [];
take(0, [_X|_Xs]) ->
  [];
take(N, [X|Xs]) when is_integer(N), N > 0 ->
  [X] ++ take(N, Xs, 1).

take(_N, [], _Acc) ->
  [];
take(N, [_X|_Xs], Acc) when Acc >= N ->
  [];
take(N, [X|Xs], Acc) ->
  [X] ++ take(N, Xs, Acc + 1).

take_test() ->
  ?assertEqual([], take(15, [])),
  ?assertEqual([], take(0, "hello")),
  ?assertEqual("hell", take(4, "hello")),
  ?assertEqual("hello", take(5, "hello")),
  ?assertEqual("hello", take(9, "hello")).

% Step-by-step evaluation (tail)
%
% take_t(4, "hello")
% take_t(4, "ello", 1, "h")
% take_t(4, "llo", 2, "he")
% take_t(4, "lo", 3, "hel")
% take_t(4, "o", 4, "hell")
% hell

-spec take_t(integer(), [T]) -> [T].
take_t(_, []) ->
  [];
take_t(0, [_X|_Xs]) ->
  [];
take_t(N, [X|Xs]) when is_integer(N), N > 0 ->
  take_t(N, Xs, 1, [X]).

take_t(_N, [], _Acc, Res) ->
  Res;
take_t(N, [_X|_Xs], Acc, Res) when Acc >= N ->
  Res;
take_t(N, [X|Xs], Acc, Res) ->
  take_t(N, Xs, Acc + 1, Res ++ [X]).

take_t_test() ->
  ?assertEqual([], take_t(15, [])),
  ?assertEqual([], take_t(0, "hello")),
  ?assertEqual("hell", take_t(4, "hello")),
  ?assertEqual("hello", take_t(5, "hello")),
  ?assertEqual("hello", take_t(9, "hello")).
