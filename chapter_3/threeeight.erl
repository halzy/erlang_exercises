-module(threeeight).
-export([parser/1]).

parseNumber(Letter) ->
	{num, Letter-48}.

% minus - 45
parserHelper([HT|AST], [H|T]=_Expression) when H == 45 ->
	io:format("~p|~p~n", [HT,AST]),
	Left = hd(HT),
	[Right|ExTail] = T,
	parserHelper([{minus, Left, parseNumber(Right)}|AST], ExTail);
% plus + 43
parserHelper([HT|AST], [H|T]=_Expression) when H == 43 ->
	Left = hd(HT),
	[Right|ExTail] = T,
	parserHelper([{plus, Left, parseNumber(Right)}|AST], ExTail);
% 0=48 9=57
parserHelper([HT|AST], [H|T]=_Expression) when H >= 48 , H =< 57->
	parserHelper([[parseNumber(H)|HT]|AST], T);

% (
parserHelper(AST, [H|T]=_Expression) when H == 40 ->
	parserHelper([[]|AST], T);
% )
parserHelper([HT|AST], [H|T]=_Expression) when H == 41 ->
	[LastGroup|AST2] = AST,
	parserHelper([[HT|LastGroup]|AST2], T);

%end
parserHelper(AST, []) ->
	AST;
%beginning
parserHelper([], Expression) ->
	parserHelper([[]], Expression).

parse(Expression) ->
	parserHelper([], Expression).

parser(Expression) ->
	io:format("Input: ~p~n", [Expression]),
	Tuples = parse(Expression),
	io:format("Output: ~p~n", [Tuples]).
