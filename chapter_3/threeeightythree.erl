-module(threeeightythree).
-export([print/1]).

addParens(String) ->
	A = string:concat([$(], String),
	string:concat(A, [$)]).

prettyPrintDoubleOperator(Op, LHS, RHS) ->
	A = string:concat(prettyPrint(LHS), [Op]),
	B = string:concat(A, prettyPrint(RHS)),
	addParens(B).

prettyPrintSingleOperator(Op, Expression) ->
	A = string:concat([Op], prettyPrint(Expression)),
	addParens(A).
	
prettyPrint({plus, LHS, RHS}) ->
	prettyPrintDoubleOperator($+, LHS, RHS);
prettyPrint({minus, LHS, RHS}) ->
	prettyPrintDoubleOperator($-, LHS, RHS);
prettyPrint({unary_minus, Expression}) ->
	prettyPrintSingleOperator($~, Expression);
prettyPrint({num, Number}) ->
	[Number + 48].

print([]) ->
	ok;
print([Expression|List]) ->
	Result = prettyPrint(Expression),
	io:format("~p = ~p~n", [Result, Expression]),
	print(List).
