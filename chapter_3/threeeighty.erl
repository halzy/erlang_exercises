-module(threeeighty).
-export([parser/1]).

parseNumber(Letter) ->
	{num, Letter-48}.

parserHelperGetExpression(AST) when is_list(AST) ->
	hd(AST);
parserHelperGetExpression(AST) when is_tuple(AST) ->
	AST.

parserHelperOperator(Op,[HT|AST], ExpressionTail) ->
	[Right|NewTail] = ExpressionTail,
	Right1 = parserHelper(AST, ExpressionTail),
	NewAST = [{Op, parserHelperGetExpression(HT), hd(Right1)}|AST],
	parserHelper(NewAST, NewTail).

% minus - 45
parserHelper(AST, [H|T]) when H == 45 ->
	parserHelperOperator(minus, AST, T);
% plus + 43
parserHelper(AST, [H|T]) when H == 43 ->
	parserHelperOperator(plus, AST, T);
% 0=48 9=57
parserHelper(AST, [H|T]=_Expression) when H >= 48 , H =< 57->
	parserHelper([parseNumber(H)|AST], T);

% (
parserHelper(AST, [H|T]=_Expression) when H == 40 ->
	parserHelper([[]|AST], T);
% )
parserHelper([HT|AST], [H|T]=_Expression) when H == 41 ->
	io:format("~p | ~p~n", [HT, AST]),
	[LastGroup|AST2] = AST,
	parserHelper([[HT|LastGroup]|AST2], T);

%end
parserHelper(AST, []) ->
	{AST, []};
%beginning
parserHelper([], Expression) ->
	parserHelper([[]], Expression).

parse(Expression) ->
	{AST, ExpressionLeft} = parserHelper([], Expression),
	AST.

parser(Expression) ->
	io:format("Input: ~p~n", [Expression]),
	Tuples = parse(Expression),
	io:format("Output: ~p~n", [Tuples]).
