-module(threethree_ref).
-export([print_count/1, print_even_count/1]).

print_number(Number) ->
	io:format("Number:~p~n", [Number]).

print_even_number(Number) when Number rem 2 == 0 ->
	print_number(Number);
print_even_number(_Number) ->
	ok.

print_count_loop(Print, End, End) ->
	Print(End);
print_count_loop(Print, Current, End) ->
	Print(Current),
	print_count_loop(Print, Current+1, End).

print_count(Num) when Num > 0 ->
	print_count_loop(fun print_number/1, 1, Num).

print_even_count(Num) when Num > 0 ->
	print_count_loop(fun print_even_number/1, 1, Num).

