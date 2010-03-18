-module(threeseven).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() ->
	[].

destroy(_Db) ->
	ok.

delete(Key, Db) ->
	lists:keydelete(Key, 1, Db).

write(Key, Element, Db) ->
	NewDb = delete(Key, Db),
	[{Key, Element}|NewDb].

read(Key, List) ->
	case lists:keyfind(Key, 1, List) of
		false -> {error, instance};
		{Key, Value} -> {ok, Value}
	end.

match(Element, Db) ->
	Matcher = fun(Elem) -> 
			case Elem of 
				{_Key,Element} -> true; 
				_Other -> false
			end
		end,
	lists:filter(Matcher, Db).
