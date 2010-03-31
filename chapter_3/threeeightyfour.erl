-module(threeeightyfour).
-export([compile/1]).


translateInstruction(unary_minus) -> umin;
translateInstruction(minus) -> sub;
translateInstruction(plus) -> add.

compileExpression(Instructions, []) ->
	Instructions;

compileExpression(Instructions, [{num, Value}|Expressions]) ->
	compileExpression([Value|Instructions], Expressions);

compileExpression(Instructions, [{Operator, Value}|Expressions]) ->
	Op = translateInstruction(Operator),
	OpInstructions = [Op|Instructions],
	ValueExp = [Value|Expressions],
	compileExpression(OpInstructions, ValueExp);

compileExpression(Instructions, [{Operator, LHS, RHS}|Expressions]) ->
	Op = translateInstruction(Operator),
	OpInstructions = [Op|Instructions],
	RightExp = [RHS|Expressions],
	LeftExp = [LHS|RightExp],
	compileExpression(OpInstructions, LeftExp).

compileExpression(Expression) ->
	compileExpression([], [Expression]).

compile(Results, []) ->
	lists:reverse(Results);
compile(Results, [CurrentExpression|Expressions]) ->
	Result = compileExpression(CurrentExpression),
	compile([Result|Results], Expressions).

compile(Expressions) ->
	compile([], Expressions).
