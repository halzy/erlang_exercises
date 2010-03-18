-module(db).
-export([
	new/0,
	destroy/1,
	write/2,
	delete/2,
	view/1,
	read/2,
	match/2
	]).

-record(db_entry, {
	key, 		%atom()
	value		%any()
	}).

-spec new() -> pid().
-spec destroy( pid() ) -> 'ok'.
-spec write(#db_entry{}, pid() ) -> 'ok'.
-spec delete(atom(), pid() ) -> 'ok'.
-spec view( pid() ) -> nil() | list(#db_entry{}).
-spec read( atom(), pid() ) -> {error,  atom()} | {ok, any()}.
-spec match(any(), pid() ) -> nil() | list(atom()).
-spec read_key(list(#db_entry{}), atom()) -> {ok, any()} | {error, atom()}.
-spec write_key(list(#db_entry{}), #db_entry{}) -> list(#db_entry{}).
-spec delete_key(list(#db_entry{}), atom()) -> list(#db_entry{}).
-spec match_value(list(#db_entry{}), any()) -> list(atom()).
-spec loop([#db_entry{}]) -> nil().

-spec call(pid(),'delete' | 'destroy' | 'match' | 'read' | 'view' | 'write', any()) -> any().
-spec reply(pid(),'ok' | [#db_entry{}] | {'error',atom()}) -> [].


new() ->
	spawn(fun() -> loop([]) end).

destroy(Db) ->
	call(Db, destroy, {}).

read(Key, Db) ->
	call(Db, read, Key).

match(Value, Db) ->
	call(Db, match, Value).

write(Data, Db) ->
	call(Db, write, Data). 

delete(Key, Db) ->
	call(Db, delete, Key).

view(Db) ->
	call(Db, view, {}).

call(Db, Command, Message) ->
	Pid = self(),
	Db ! {Command, Pid, Message},
	receive
		{reply, Response} -> Response;
		Other -> throw({unknown_message, Other})
	end.

reply(Pid, Message) ->
	Pid ! {reply, Message},
	[].

loop(State) ->
	receive
		{read, Pid, Key} ->
			reply(Pid, read_key(State, Key)),
			loop(State);
		{write, Pid, Data} ->
			State1 = write_key(State, Data),
			reply(Pid, ok),
			loop(State1);
		{match, Pid, Value} ->
			reply(Pid, match_value(State, Value)),
			loop(State);
		{delete, Pid, Key} ->
			State1 = delete_key(State, Key),
			reply(Pid, ok),
			loop(State1);
		{destroy, Pid, _Data} -> 
			reply(Pid, ok),
			[];
		{view, Pid, _Data} -> 
			reply(Pid, State),
			loop(State);
		Other ->
			throw({unknown_message, Other})
	end.

read_key([], Key) -> {error, Key};
read_key([#db_entry{key=Key, value=Data}|_T], Key) -> {ok, Data};
read_key([_H|T], Key) ->
	read_key(T, Key).

delete_key([], _Key) -> [];
delete_key([#db_entry{key=Key,value=_Data}|T], Key) -> delete_key(T, Key);
delete_key([H|T], Key) ->
	[H|delete_key(T, Key)].

write_key(State, #db_entry{key=Key, value=_Value} = Data) ->
	[Data|delete_key(State, Key)].

match_value([], _Value) -> [];
match_value([#db_entry{key=Key, value=Value}|T], Value) -> 
	[Key|match_value(T, Value)];
match_value([_H|T], Value) ->
	match_value(T, Value).

