-module(threesix).
-export([quicksort/1, mergesort/1]).

quicksort(Pivot, Left, Right, []=_Src) ->
	{Left, Pivot, Right};
quicksort(Pivot, Left, Right, [H|T]=_Src) when H < Pivot ->
	quicksort(Pivot, [H|Left], Right, T);
quicksort(Pivot, Left, Right, [H|T]=_Src) ->
	quicksort(Pivot, Left, [H|Right], T).

quicksort([]) ->
	[];
quicksort([H|T]=_List) ->
	{Left, Pivot, Right} = quicksort(H, [], [], T),
	quicksort(Left) ++ [Pivot] ++ quicksort(Right).


listsplit(Left, Right, []) ->
	{Left, Right};
listsplit(Left, Right, [H|T]=_Src) ->
	listsplit([H|Right], Left, T).

listjoin([], [], Dst) ->
	lists:reverse(Dst);
listjoin([LH|LT]=_Left, [], Dst) ->
	listjoin([], LT, [LH|Dst]);
listjoin([], [RH|RT]=_Right, Dst) ->
	listjoin([], RT, [RH|Dst]);
listjoin([LH|_LT]=Left, [RH|RT]=_Right, Dst) when RH =< LH ->
	listjoin(Left, RT, [RH|Dst]);
listjoin([LH|LT]=_Left, [RH|_RT]=Right, Dst) when LH =< RH ->
	listjoin(LT, Right, [LH|Dst]).

mergesort(List) ->
	{Left, Right} = listsplit([], [], List),
	LeftSorted = quicksort(Left),
	RightSorted = quicksort(Right),
	_ListJoined = listjoin(LeftSorted, RightSorted, []).

