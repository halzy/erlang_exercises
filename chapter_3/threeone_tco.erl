-module(threeone_tco).
-export([sum/1, sum/2]).

sum_range(Sum, Lower, Lower) ->
	Sum + Lower;
sum_range(Sum, Lower, Upper) ->
	sum_range(Sum + Upper, Lower, Upper-1).

sum(Number) when Number > 0 ->
	sum_range(0, 1, Number).

sum(Lower, Upper) when Lower =< Upper ->
	sum_range(0, Lower, Upper).

