-module(test).
-export([print/1, count/2]).

print(hello) ->
	io:format("Hello there!~n");

print(goodbye) ->
	io:format("Goodbye! Come again!!~n");

print(something) ->
	io:format("Something here.~n");

print(Value) ->
	io:format("~p~n", [Value]).


count(Value, Value) ->
	io:format("~p~n", [Value]);
count(First, Last) ->
	print(First),
	count(First+1, Last).

