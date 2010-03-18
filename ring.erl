-module(ring).
-export([start/3]).

start(M, N, Message) ->
	spawn(fun() -> start_create(M, N, Message) end).

start_create(M, N, Message) ->
	Pid = self(),
	create(Pid, M, N-1, Message).

create(FirstPid, M, 0, Message) ->
	message_loop(FirstPid, M, Message);

create(FirstPid, M, N, Message) ->
	Pid = spawn(fun() -> create(FirstPid, M, N-1, Message) end),
	message_loop(Pid, M, Message).

message_loop(Pid, 0, _Message) ->
	receive_loop(Pid);

message_loop(Pid, M, Message) ->
	Pid ! Message,
	message_loop(Pid, M-1, Message).

receive_loop(Pid) ->
	%%io:format("receive_loop(~p ~p)~n", [self(), Pid]),
	receive
		quit -> 
			Pid ! quit;
		Other ->
			Pid ! Other,
			receive_loop(Pid)
	end.

