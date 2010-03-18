-module(exr).
-export([sum/1, sum/2]).
-export([create/1, reverse_create/1]).
-export([print_numbers/1]).

sum(1) -> 1;
sum(Number) ->
	Number + sum(Number-1).

sum(X, X) -> X;
sum(N, M) when N < M ->
	M + sum(N, M-1).

create(X, X) -> [X];
create(X, N) ->
	[X|create(X+1, N)].

create(N) when N > 0 ->
	create(1, N).

reverse_create(1) -> [1];
reverse_create(N) when N > 1->
	[N|reverse_create(N-1)].

print(X) when X rem 2 == 0 ->
	io:format("Number: ~p~n", [X]);
print(_X) ->
	ok.
	
print_numbers(X, X) -> 
	print(X);
print_numbers(X, N) ->
	print(X),
	print_numbers(X+1, N).

print_numbers(N) when N > 0 ->
	print_numbers(1, N).


