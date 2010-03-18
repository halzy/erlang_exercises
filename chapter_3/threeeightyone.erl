-module(threeeightyone).
-export([parser/1]).

getNextExpressionUntil(Stack, [Until|String], Until) ->
	{Stack, String};
getNextExpressionUntil(Stack, String, Until) ->
	{Expression, ModifiedStack, StringLeft} = getNextExpression(Stack, String),
	getNextExpressionUntil([Expression|ModifiedStack], StringLeft, Until).

getNextOperator(Op, [PreviousExpression|RemainingStack], String) ->
	{NextExpression, ModifiedStack, RemainingString} = getNextExpression(RemainingStack, String),
	{{Op, PreviousExpression, NextExpression}, ModifiedStack, RemainingString}.

getNextExpression(Stack, [$(|String]) ->
	{Expression, [], RemainingString1} = getNextExpression([], String),
	{[NextExpression|_T], RemainingString2} = getNextExpressionUntil([Expression], RemainingString1, $)),
	{NextExpression, Stack, RemainingString2};

getNextExpression(Stack, [$+|String]) ->
	getNextOperator(plus, Stack, String);
getNextExpression(Stack, [$-|String]) ->
	getNextOperator(minus, Stack, String);

% 0=48 9=57
getNextExpression(Stack, [H|String]) when H >= 48 , H =< 57->
	{{num, H-48}, Stack, String};

getNextExpression(Stack, [H|String]) ->
	{H, Stack, String}.

parseString(Stack, []=_String) ->
	lists:reverse(Stack);
parseString(Stack, String) ->
	{Expression, ModifiedStack, StringLeft} = getNextExpression(Stack, String),
	parseString([Expression|ModifiedStack], StringLeft).

parseString(String) ->
	parseString([], String).

parser(String) ->
	io:format("String: ~p~n", [String]),
	Stack = parseString(String),
	io:format("Pretty: ~p~n", [Stack]),
	Stack.
