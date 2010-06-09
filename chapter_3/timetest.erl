-module(timetest).
-export([quicksortA/1, quicksortB/1, reverseA/1, reverseB/1]).

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
        lists:reverse(concatenateHelper([], Lists)).

quicksort(Pivot, Left, Right, []=_Src) ->
	{Left, Pivot, Right};
quicksort(Pivot, Left, Right, [H|T]=_Src) when H < Pivot ->
	quicksort(Pivot, [H|Left], Right, T);
quicksort(Pivot, Left, Right, [H|T]=_Src) ->
	quicksort(Pivot, Left, [H|Right], T).

quicksortA([]) ->
	[];
quicksortA([H|T]=_List) ->
	{Left, Pivot, Right} = quicksort(H, [], [], T),
	quicksortA(Left) ++ [Pivot] ++ quicksortA(Right).

quicksortB([]) ->
	[];
quicksortB([H|T]=_List) ->
	{Left, Pivot, Right} = quicksort(H, [], [], T),
	concatenate([quicksortB(Left), [Pivot], quicksortB(Right)]).

reverseA(To, []) ->
        To;
reverseA(To, [H|T]) ->
        reverseA(To ++ [H], T).
reverseA(List) ->
        reverseA([], List).

reverseB(To, []) ->
        To;
reverseB(To, [H|T]) ->
        reverseB([H|To], T).
reverseB(List) ->
        reverseB([], List).


