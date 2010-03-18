-module(threefour_db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() ->
	[].

destroy(_Db) ->
	ok.


delete(_Key, NewDb, []) ->
	NewDb;
delete(Key, NewDb, [{Key, _Element}|T]) ->
	delete(Key, NewDb, T);
delete(Key, NewDb, [H|T]) ->
	delete(Key, [H|NewDb], T).

delete(Key, Db) ->
	delete(Key, [], Db).


write(Key, Element, Db) ->
	NewDb = delete(Key, Db),
	[{Key, Element}|NewDb].


read(_Key, []) ->
	{error, instance};
read(Key, [{Key,Element}|_T]) ->
	{ok, Element};
read(Key, [_H|T]) ->
	read(Key, T).

match(_Element, [], Keys) ->
	Keys;
match(Element, [{Key,Element}|Db], Keys) ->
	match(Element, Db, [Key|Keys]);
match(Element, [_H|Db], Keys) ->
	match(Element, Db, Keys).

match(Element, Db) ->
	match(Element, Db, []).

