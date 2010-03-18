-module(threeeightythree).
-export([print/1]).

addParens(String) ->
	A = string:concat([$(], String),
	string:concat(A, [$)]).

prettyPrintOperator(Op, LHS, RHS) ->
	A = string:concat(prettyPrint(LHS), [Op]),
	B = string:concat(A, prettyPrint(RHS)),
	addParens(B).
	
prettyPrint({plus, LHS, RHS}) ->
	prettyPrintOperator($+, LHS, RHS);
prettyPrint({minus, LHS, RHS}) ->
	prettyPrintOperator($-, LHS, RHS);
prettyPrint({num, Number}) ->
	[Number + 48].

print([]) ->
	ok;
print([Expression|List]) ->
	Result = prettyPrint(Expression),
	io:format("~p = ~p~n", [Result, Expression]),
	print(List).
