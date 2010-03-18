-module(boolean).
-export([b_not/1, b_and/2, b_or/2, b_nand/2]).

b_not(false) -> true;
b_not(true) -> false.

b_and(true, true) -> true;
b_and(false, false) -> true;
b_and(_BoolA, _BoolB) -> false.

b_or(true, _Bool) -> true;
b_or(_Bool, true) -> true;
b_or(_BoolA, _BoolB) -> false.

b_nand(BoolA, BoolB) ->
	b_not(b_and(BoolA, BoolB)).

