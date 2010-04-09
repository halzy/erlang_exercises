-module(threefive_ref).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).

reverse(To, []) ->
	To;
reverse(To, [H|T]) ->
	reverse([H|To], T).

reverse(List) -> 
	reverse([], List).


filter(Filtered, [], _Number) ->
	reverse(Filtered);
filter(Filtered, [H|T], Number) when H =< Number ->
	filter([H|Filtered], T, Number);
filter(Filtered, [_H|T], Number) ->
	filter(Filtered, T, Number).


filter(List, Number) ->
	filter([], List, Number).



flattenHelper(Dst, []) ->
	Dst;
flattenHelper(Dst, [[[]|HeadTail]|T]) ->
	flattenHelper(Dst, [HeadTail|T]);
flattenHelper(Dst, [[HeadTop|HeadTail]|T]) ->
	NewList1 = [HeadTail|T],
	NewList2 = [HeadTop|NewList1],
	flattenHelper(Dst, NewList2);
flattenHelper(Dst, [[]|T]) ->
	flattenHelper(Dst, T);
flattenHelper(Dst, [H|T]) ->
	flattenHelper([H|Dst], T);
flattenHelper(Dst, Item) ->
	[Item|Dst].

flatten(Lists) ->
	reverse(flattenHelper([], Lists)).




concatenate(Dst, []) ->
	Dst;
concatenate(Dst, [H|T]) ->
	concatenate([H|Dst], T).

concatenateHelper(Dst, []) ->
	Dst;
concatenateHelper(Dst, [H|T]) ->
	Dst2 = concatenate(Dst, H),
	concatenateHelper(Dst2, T).

concatenate(Lists) ->
	reverse(concatenateHelper([], Lists)).


