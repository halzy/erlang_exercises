-module(my_db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/1, terminate/1]).

-define(SERVER, ?MODULE).

%% [bgh] PUBLIC INTERFACE
 
start() ->
    register(?SERVER, spawn(my_db, init, [[]])),
    ok.

stop() ->
    ?SERVER ! {stop, self()},
    receive 
	{reply, Reply} ->
	    Reply 
    end.

write(Key, Element) ->
    call(?SERVER, {write, Key, Element}).

delete(Key) ->
    call(?SERVER, {delete, Key}).

read(Key) ->
    call(?SERVER, {read, Key}).

match(Element) ->
    call(?SERVER, {match, Element}).

%% [bgh] start/stop interface

init(Args) ->
    loop(Args).

terminate(State) ->
    State.

%% [bgh] server handler functions
handle_msg({write, Key, Element}, Db) ->
    {ok, lists:keystore(Key, 1, Db, {Key, Element})};
handle_msg({delete, Key}, Db) ->
    {ok, lists:keydelete(Key, 1, Db)};
handle_msg({read, Key}, Db) ->
    case lists:keyfind(Key, 1, Db) of
	false ->
	    {{error, instance}, Db};
	{Key, Value} ->
	    {{ok, Value}, Db}
    end;
handle_msg({match, Value}, Db) ->
    {match(Value, Db), Db}.
    
    
match(_Element, [], Keys) ->
        Keys;
match(Element, [{Key,Element}|Db], Keys) ->
        match(Element, Db, [Key|Keys]);
match(Element, [_H|Db], Keys) ->
        match(Element, Db, Keys).

match(Element, Db) ->
        match(Element, Db, []).


%% [bgh] copy/pasted from page 135, erlang programming
call(Name, Msg) ->
    Name ! {request, self(), Msg},
    receive {reply, Reply} -> Reply end.
reply(To, Msg) ->
    To ! {reply, Msg}.
loop(State) ->
    receive
	{request, From, Msg} ->
	    {Reply, NewState} = handle_msg(Msg, State),
	    reply(From, Reply),
	    loop(NewState);
	{stop, From} ->
	    reply(From, terminate(State))
    end.
