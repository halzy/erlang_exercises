-module(four_one).
-export([start/0,print/1,stop/0]).

-export([loop/0]).

start() ->
	Pid = spawn(four_one,loop,[]),
	register(four_one, Pid),
	ok.

print(Term) ->
	four_one ! {print, Term},
	ok.

stop() ->
	four_one ! stop,
	ok.


loop() ->
	receive
		stop -> ok;
		{print, Term} -> 
			io:format("~p~n", [Term]),
			loop()
	end.

