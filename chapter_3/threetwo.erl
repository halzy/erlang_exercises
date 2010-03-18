-module(threetwo).
-export([create/1, reverse_create/1]).

create(1, [1|_T]=List) ->
	List;	
create(Num, List) ->
	Next = Num-1,
	create(Next, [Next|List]).
create(Num) when Num > 0 ->
	create(Num, [Num]).

reverse_create_list(Num, [Num|_T]=List) ->
	List;
reverse_create_list(Num, [H|_T]=List) ->
	reverse_create_list(Num, [H+1|List]).
reverse_create(Num) when Num > 0 ->
	reverse_create_list(Num, [1]).

