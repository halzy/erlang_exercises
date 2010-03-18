-module(threeone).
-export([sum/1, sum/2]).

sum(1) ->
	1;
sum(Number) ->
	Number + sum(Number - 1).


sum(Lower, Lower) ->
	Lower;
sum(Lower, Upper) when Lower =< Upper ->
	Upper + sum(Lower, Upper-1).
