-module(threeeightytwo).
-export([evaluator/1]).

evaluate({plus, LHS, RHS}) ->
	evaluate(LHS) + evaluate(RHS);
evaluate({minus, LHS, RHS}) ->
	evaluate(LHS) - evaluate(RHS);
evaluate({num, Number}) ->
	Number.

evaluator([]) ->
	ok;
evaluator([Expression|List]) ->
	Result = evaluate(Expression),
	io:format("~p = ~p~n", [Result, Expression]),
	evaluator(List).
