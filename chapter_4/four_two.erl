-module(four_two).

-export([start/3, test/0]).
-export([process_loop/0, process_loop/1]).

test() ->
    FirstPid = start(10,4,"This is a message"),
    
    %% [bgh] after 15 second, kill it automatically
    receive
    after
	15000 ->
	    stop(FirstPid)
    end.

stop(Pid) ->
    Pid ! {stop, 0}.

start(M, N, Message) ->
    %% [bgh] start N processes
    Pids = lists:reverse(lists:foldl(
				   fun(_Number,PidList) -> 
					   NewPid = spawn(?MODULE, process_loop, []),
					   [NewPid|PidList]
				   end, 
				   [], 
				   lists:seq(1, N)
				 )),

    %% [bgh] Shift the one off the front, and put it on the end
    NeighborPids = rotate_array(Pids),
    
    %% [bgh] link the N processes together
    link_pids(Pids, NeighborPids),

    [FirstPid|_OtherPids] = Pids,

    %% [bgh] start the message sending by sending M messages to PID
    send_messages_to_pid(M, Message, FirstPid),
    
    %% [bgh] start the processes sending messages
    FirstPid.

rotate_array([]) ->
    [];
rotate_array([First|_]=List) when length(List) =:= 1 ->
    [First];
rotate_array([First|Remainder]) ->
    io:format("~p ~p~n", [First,Remainder]),
    lists:append(Remainder,[First]).


send_messages_to_pid(M, Message, FirstPid) when M > 0 ->
    FirstPid ! {message, Message},
    send_messages_to_pid(M-1, Message, FirstPid);
send_messages_to_pid(_M, _Message, _FirstPid) ->
    ok.

link_pids([], []) ->
    ok;
link_pids([Pid|Pids], [NPid|NeighborPids]) ->
    Pid ! {link, NPid},
    link_pids(Pids, NeighborPids).
    
process_loop() ->
    receive
	{link, Pid} ->
	    process_loop({Pid, 0});
	Other -> 
	    io:format("Ignored ~p~n", [Other]),
	    process_loop()
    end.

process_loop({Pid, Count}) ->
    receive
	{message, Message} ->
	    Pid ! {message, Message},
	    process_loop({Pid, Count+1});
	{stop, CountSum} ->
	    Pid ! {stop, Count+CountSum},
	    io:format("~p is shutting down, saw ~p messages~n", [self(), Count+CountSum]);
	Other ->
	    io:format("Ignored ~p~n", [Other]),
	    process_loop({Pid, Count})
    end.
