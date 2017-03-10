-module(second).
-export([hyp/2, perimeter/2, area/2]).

%%%%%%%
% 1.9 %
%%%%%%%

% Defining second.erl
%
% Using your text editor of choice, define a new module second.erl to include
% the following functions:
%
% Using the function square from first.erl, define a function that gives the
% size of the hypotenuse of a right-angled triangle given the lengths of the two other sides.
%
% Define functions that give the perimeter and area of a right-angled triangle,
% given the lengths of the two short sides.
%
% Which functions can you re-use in making these definitions? Which existing
% definitions can you modify to give you the answers?

hyp(A, B) ->
  math:sqrt(first:square(A) + first:square(B)).

perimeter(A, B) ->
  A + B + hyp(A, B).

area(A, B) ->
  first:area(A, B, hyp(A, B)).
