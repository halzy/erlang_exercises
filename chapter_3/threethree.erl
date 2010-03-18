-module(threethree).
-export([print_count/1, print_even_count/1]).

print_number(Number) ->
	io:format("Number:~p~n", [Number]).

print_count(End, End) ->
	print_number(End);
print_count(Current, End) ->
	print_number(Current),
	print_count(Current+1, End).

print_count(Num) when Num > 0 ->
	print_count(1, Num).


print_even_number(Number) when Number rem 2 == 0 ->
	print_number(Number);
print_even_number(_Number) ->
	ok.

print_even_count(Current, End) when Current >= End ->
	print_even_number(Current);
print_even_count(Current, End) ->
	print_even_number(Current),
	print_even_count(Current+1, End).
	
print_even_count(Num) when Num > 0 ->
	print_even_count(1, Num).

