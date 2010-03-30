-module(threeeightytwo).
-export([evaluator/1]).

evaluate({plus, LHS, RHS}) ->
	evaluate(LHS) + evaluate(RHS);
evaluate({minus, LHS, RHS}) ->
	evaluate(LHS) - evaluate(RHS);
evaluate({unary_minus, Expression}) ->
	-1 * evaluate(Expression);
evaluate({num, Number}) ->
	Number.

eval(Results, []) ->
	lists:reverse(Results);
eval(Results, [Expression|List]) ->
	Result = evaluate(Expression),
	eval([Result|Results], List).

evaluator(Expressions) ->
	eval([], Expressions).
