-module(palindrome).
-export([palindrome/1]).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%
% 2.15 %
%%%%%%%%

% Define a function palindrome, that returns true or false depending on whether
% the list is a palindrome - the same when read right to left as when read left
% to right.

-spec palindrome(string()) -> boolean().
palindrome(X) ->
  Y = re:replace(string:to_lower(X), "[^[:alnum:]]", "", [global,{return,list}]),
  string:equal(Y, lists:reverse(Y)).

palidrome_test() ->
  ?assertEqual(true, palindrome("Madam I\'m Adam")),
  ?assertEqual(true, palindrome("árrá")),
  ?assertEqual(false, palindrome("Arepa")).

