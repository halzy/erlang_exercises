-module(threeeightyfive).
-export([simulate/1]).

simulator([Value|_Stack], []) ->
	Value;
simulator([Value|Stack], [umin|Instructions]) ->
	UminValue = -1 * Value,
	simulator([UminValue|Stack], Instructions);
simulator([FirstValue|Stack], [sub|Instructions]) ->
	[SecondValue|MoreStack] = Stack,
	simulator([FirstValue - SecondValue|MoreStack], Instructions);
simulator([FirstValue|Stack], [add|Instructions]) ->
	[SecondValue|MoreStack] = Stack,
	simulator([FirstValue + SecondValue|MoreStack], Instructions);
simulator(Stack, [Number|Instructions]) ->
	simulator([Number|Stack], Instructions).

simulator(Instructions) ->
	simulator([], Instructions).

simulate(Results, []) ->
	lists:reverse(Results);
simulate(Results, [Instructions|ArraysOfInstructions]) ->
	Result = simulator(Instructions),
	simulate([Result|Results], ArraysOfInstructions).

simulate(ArraysOfInstructions) ->
	simulate([], ArraysOfInstructions).

