-module(threeeightyone).
-export([parser/1]).

getNextExpressionUntil(Stack, [Until|String], Until) ->
	{Stack, String};
getNextExpressionUntil(Stack, String, Until) ->
	{Expression, ModifiedStack, StringLeft} = getNextExpression(Stack, String),
	getNextExpressionUntil([Expression|ModifiedStack], StringLeft, Until).

getNextDoubleOperator(Op, [PreviousExpression|RemainingStack], String) ->
	{NextExpression, ModifiedStack, RemainingString} = getNextExpression(RemainingStack, String),
	{{Op, PreviousExpression, NextExpression}, ModifiedStack, RemainingString}.

getNextSingleOperator(Op, Stack, String) ->
	{NextExpression, [], RemainingString} = getNextExpression([], String),
	{{Op, NextExpression}, Stack, RemainingString}.

getNextExpression(Stack, [$(|String]) ->
	{Expression, [], RemainingString1} = getNextExpression([], String),
	{[NextExpression|_T], RemainingString2} = getNextExpressionUntil([Expression], RemainingString1, $)),
	{NextExpression, Stack, RemainingString2};

getNextExpression(Stack, [$+|String]) ->
	getNextDoubleOperator(plus, Stack, String);
getNextExpression(Stack, [$-|String]) ->
	getNextDoubleOperator(minus, Stack, String);
getNextExpression(Stack, [$~|String]) ->
	getNextSingleOperator(unary_minus, Stack, String);
	

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
	parseString(String).
