-module(threeten).
-export([textProcess/2, test/1]).

textProcess(Text, Width) ->
	TokenText = tokenizeText(Text),
	WordLines = makeWordLines(TokenText, Width, Width, []),
	String = wordLinesToString(WordLines),
	io:format("~s", [String]).

wordLinesToString(WordLines) ->
	lists:flatten(wordLinesToString(WordLines, [])).

wordLinesToString([], Strings) ->
	lists:reverse(Strings);
wordLinesToString([[]|WordLines], String) ->
	wordLinesToString(WordLines, String);
wordLinesToString([Line|WordLines], String1) ->
	String2 = wordLineToString(Line, 0, String1),
	wordLinesToString(WordLines, String2).

wordLineToString([], _WordNumber, String) ->
	NewLine = "\n",
	[NewLine|String];
wordLineToString([{Word, _Length}|Line], WordNumber, String) when WordNumber == 0 ->
	wordLineToString(Line, WordNumber+1, [Word|String]);
wordLineToString([{Word1, _Length}|Line], WordNumber, String) ->
	Word2 = [" "|Word1],
	wordLineToString(Line, WordNumber+1, [Word2|String]).

makeWordLines([], _Width, _WidthLeft, [LastLine|Lines]) ->
	ReversedLastLine = lists:reverse(LastLine),
	AllLines = [ReversedLastLine|Lines],
	lists:reverse(AllLines);
makeWordLines([{_Word, Length}=Token|Tokens], Width, WidthLeft, [[]|Lines]) when Length =< WidthLeft ->
	makeWordLines(Tokens, Width, WidthLeft-Length, [[Token]|Lines]);
makeWordLines([{_Word, Length}=Token|Tokens], Width, WidthLeft, [LastLine|Lines]) when Length =< WidthLeft ->
	makeWordLines(Tokens, Width, WidthLeft-Length-1, [[Token|LastLine]|Lines]);
makeWordLines([{_Word, Length}=Token|Tokens], Width, _WidthLeft, [[]|Lines]) when Length > Width ->
	makeWordLines(Tokens, Width, 0, [[Token]|Lines]);
makeWordLines([{_Word, Length}|_Tail]=Tokens, Width, _WidthLeft, Lines) when Length > Width ->
	makeWordLines(Tokens, Width, Width, [[]|Lines]);
makeWordLines(Tokens, Width, _WidthLeft, [LastLine|Lines]) ->
	ReversedLastLine = lists:reverse(LastLine),
	PreviousLines = [ReversedLastLine|Lines],
	makeWordLines(Tokens, Width, Width, [[]|PreviousLines]);
makeWordLines(Tokens, Width, WidthLeft, []) ->
	makeWordLines(Tokens, Width, WidthLeft, [[]]).

tokenizeText(Text) ->
	Tokens = string:tokens(Text, " \t\r\n"),
	measureTokens([], Tokens).

measureTokens(Measured, []) ->
	lists:reverse(Measured);
measureTokens(Measured, [Token|Tokens]) ->
	Length = string:len(Token),
	measureTokens([{Token, Length}|Measured], Tokens).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Loading Test Stubs %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
test(Width) ->
	{ok, Text} = readFileText('threeten.text'),
	textProcess(Text, Width).
	
readFileText(Filename) ->
        case file:open(Filename, read) of
                {ok, IoDevice} ->
                        Words = loadRawTextLines([], IoDevice),
                        file:close(IoDevice),
			{ok, Words};
                {error, Reason} -> {error, Reason}
        end.

loadRawTextLines(Lines, IoDevice) ->
        case file:read_line(IoDevice) of
		{ok, Line} -> loadRawTextLines([Line|Lines], IoDevice);
                eof -> lists:flatten(lists:reverse(Lines))
        end.

