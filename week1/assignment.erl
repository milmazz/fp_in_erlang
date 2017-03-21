-module(assignment).
-export([perimeter/1, area/1, enclose/1, bits/1]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%
% 1.24 %
%%%%%%%%

% Shapes
% ------
%
% Define a function perimeter/1 which takes a shape and returns the perimeter
% of the shape.
%
% Choose a suitable representation of triangles, and augment perimeter/1
%
perimeter({rectangle, _, H, W}) ->
  2 * H + 2 * W;
perimeter({circle, _, R}) ->
  2 * math:pi() * R;
perimeter({triangle, A, B, C}) ->
  length(A, B) + length(B, C) + length(C, A).

perimeter_test() ->
  ?assertEqual(22, perimeter({rectangle, {4, 1.5}, 3, 8})),
  ?assertEqual("56.5487", float_to_list(perimeter({circle, {0, 0}, 9}), [{decimals, 4}])),
  ?assertEqual("80.86", float_to_list(perimeter({triangle, {15, 15}, {23, 30}, {50, 25}}), [{decimals, 2}])).

% and area/1 to handle this case too.
area({circle, _, R}) ->
  math:pi() * R * R;
area({rectangle, _, H, W}) ->
  H * W;
% See: http://www.mathopenref.com/coordtrianglearea.html
area({triangle, {Ax, Ay}, {Bx, By}, {Cx, Cy}}) ->
  abs((Ax * (By - Cy) + Bx * (Cy - Ay) + Cx * (Ay - By)) / 2).

area_test() ->
  ?assertEqual(60, area({rectangle, {5, 3}, 6, 10})),
  ?assertEqual("28.27", float_to_list(area({circle, {0, 0}, 3}), [{decimals, 2}])),
  ?assertEqual(222.5, area({triangle, {15, 15}, {23, 30}, {50, 25}})),
  ?assertEqual(170.0, area({triangle, {15, 15}, {23, 30}, {43, 25}})).

% Define a function enclose/1 that takes a shape an returns the smallest
% enclosing rectangle of the shape.
enclose({rectangle, _, _, _} = R) ->
  R;
enclose({circle, C, R}) ->
  {rectangle, C, 2 * R, 2 * R};
enclose({triangle, {Ax, Ay}, {Bx, By}, {Cx, Cy}}) ->
  Xmin = minThree(Ax, Bx, Cx),
  Xmax = maxThree(Ax, Bx, Cx),
  Ymin = minThree(Ay, By, Cy),
  Ymax = maxThree(Ay, By, Cy),
  {rectangle, {avg(Xmin, Xmax), avg(Ymin, Ymax)}, Ymax - Ymin, Xmax - Xmin}.

enclose_test() ->
  ?assertEqual({rectangle, {1, 1}, 2, 2}, enclose({rectangle, {1, 1}, 2, 2})),
  ?assertEqual({rectangle, {0, 0}, 2, 2}, enclose({circle, {0, 0}, 1})),
  ?assertEqual({rectangle, {1.0, 0.5}, 1, 2}, enclose({triangle, {0, 0}, {1, 1}, {2, 0}})).

% Summing the bits
% ----------------
%
% Define a function bits/1 that takes a positive integer N and returns the sum of
% the bits in the binary representation. For example bits(7) is 3 and bits(8) is 1.
%
% See whether you can make both a direct recursive and a tail recursive
% definition.
%
% Step-by-Step evaluation (tail recursive)
%
% bits(7)
% bits(7, 1)
% bits(3, 2)
% bits(1, 3)
% bits(0, 3)
% 3
bits(N) when N > 0 ->
  bits(N, 0).

bits(0, Acc) ->
  Acc;
bits(N, Acc) ->
  {Q, R} = divrem(N, 2),
  bits(Q, R + Acc).

% Step-by-step evaluation
% bits_direct(7)
% 1 + bits_direct(3)
% 1 + 1 + bits_direct(1)
% 1 + 1 + 1 + bits_direct(0)
% 1 + 1 + 1 + 0
% 3
bits_direct(0) ->
  0;
bits_direct(N) when N > 0 ->
  {Q, R} = divrem(N, 2),
  R + bits_direct(Q).

bits_test() ->
  ?assertEqual(3, bits(7)),
  ?assertEqual(1, bits(8)),
  ?assertEqual(3, bits_direct(7)),
  ?assertEqual(1, bits_direct(8)).

%%%%%%%%%%%
% Helpers %
%%%%%%%%%%%

% Pythagoras' Theorem
%
% See: http://www.mathopenref.com/coorddist.html
length({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

divrem(N, M) ->
  {N div M, N rem M}.

avg(A, B) ->
  (A + B) / 2.

% From: 1.15
maxThree(X, Y, Z) ->
  max(max(X, Y), Z).

minThree(X, Y, Z) ->
  min(min(X, Y), Z).