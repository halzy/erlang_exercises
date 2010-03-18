-module(echo_server).
-export([start/0, print/1, stop/0]).

%% internal callback stuff
-export([loop/0]).

start() ->
	Pid = spawn(?MODULE, loop, []),
	register(echo, Pid),
	{ok, Pid}.

print(Msg) ->
	echo ! {echo, Msg}.

stop() ->
	echo ! stop.

loop() ->
	receive
		{echo, Msg} -> 
			io:format("~p~n", [Msg]),
			loop();
		stop -> 
			ok;
		Other -> 
			throw({unknown_message, Other})
	end.


