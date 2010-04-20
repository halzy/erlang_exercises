-module(threeten).
-export([textProcess/2, justifyText/2, testProcess/1, testJustify/1]).

textProcess(Text, Width) ->
	TokenText = tokenizeText(Text),
	WordLines = makeWordLines(TokenText, Width, Width, []),
	wordLinesToString(WordLines).

justifyText(Text, Width) ->
	TokenText = tokenizeText(Text),
	WordLines = makeWordLines(TokenText, Width, Width, []),
	justifyWordLines(WordLines, Width).

justifyWordLines(WordLines, Width) ->
	justifyWordLines(WordLines, Width, []).

justifyWordLines([], _Width, JustifiedLines) ->
	lists:flatten(lists:reverse(JustifiedLines));
justifyWordLines([[]|WordLines], Width, JustifiedLines) ->
	justifyWordLines(WordLines, Width, JustifiedLines);
justifyWordLines([Line|WordLines], Width, JustifiedLines) ->
	SpacesPerGap = case length(WordLines) of
		0 -> 0; % the last line isn't justified
		_Other -> spacesPerGap(Line, Width)
	end,
	JustifiedString = lists:reverse(wordLineToString(Line, 0, "", SpacesPerGap, 0)),
	justifyWordLines(WordLines, Width, [JustifiedString|JustifiedLines]).

spacesPerGap([], _Width) ->
	0;
spacesPerGap(Line, Width) ->
	Words = length(Line),
	WordLength = lists:foldr(fun({_Word, Length}, Accum) -> Length + Accum end, 0, Line),
	ExtraSpaces = Width - WordLength - Words + 1,
	Gaps = Words-1,
	if
		Gaps =< 0 -> 0;
		ExtraSpaces =< 0 -> 0;
		ExtraSpaces > 0 -> ExtraSpaces / Gaps
	end.

wordLinesToString(WordLines) ->
	lists:flatten(wordLinesToString(WordLines, [])).

wordLinesToString([], Strings) ->
	lists:reverse(Strings);
wordLinesToString([[]|WordLines], String) ->
	wordLinesToString(WordLines, String);
wordLinesToString([Line|WordLines], String1) ->
	String2 = wordLineToString(Line, 0, String1, 0, 0),
	wordLinesToString(WordLines, String2).

wordLineToString([], _WordNumber, String, _SpacesPerGap, _GapLeftover) ->
	NewLine = "\n",
	[NewLine|String];
wordLineToString([{Word, _Length}|Line], WordNumber, String, SpacesPerGap, _GapLeftover) when WordNumber == 0 ->
	wordLineToString(Line, WordNumber+1, [Word|String], SpacesPerGap, SpacesPerGap);
wordLineToString([{Word1, _Length}|Line], WordNumber1, String, SpacesPerGap, GapLeftover1) ->
	WordNumber2 = WordNumber1+1,				% figure out the current word number
	Spaces = erlang:trunc(GapLeftover1)		,	% drop the decimal places
	GapLeftover2 = GapLeftover1 - Spaces + SpacesPerGap,	% adjust the remaining leftover
	Word2 = [string:copies(" ", Spaces+1)|Word1],
	wordLineToString(Line, WordNumber2, [Word2|String], SpacesPerGap, GapLeftover2).

% end case, out of words
makeWordLines([], _Width, _WidthLeft, [LastLine|Lines]) ->
	ReversedLastLine = lists:reverse(LastLine),
	AllLines = [ReversedLastLine|Lines],
	lists:reverse(AllLines);
% if the line is currently empty, and the word length is =< than the length of the line
makeWordLines([{_Word, Length}=Token|Tokens], Width, WidthLeft, [[]|Lines]) when Length =< Width ->
	makeWordLines(Tokens, Width, WidthLeft-Length, [[Token]|Lines]);
% line with words, and the word length is =< the length left
makeWordLines([{_Word, Length}=Token|Tokens], Width, WidthLeft, [LastLine|Lines]) when Length < WidthLeft ->
	makeWordLines(Tokens, Width, WidthLeft-Length-1, [[Token|LastLine]|Lines]);
% if the line is empty, and the word length is > the width of the line
makeWordLines([{_Word, Length}=Token|Tokens], Width, _WidthLeft, [[]|Lines]) when Length > Width ->
	makeWordLines(Tokens, Width, 0, [[Token]|Lines]);
% if there is no more space on the line, reverse the tokens on the last line and push a new one on the stack
makeWordLines(Tokens, Width, _WidthLeft, [LastLine|Lines]) ->
	ReversedLastLine = lists:reverse(LastLine),
	PreviousLines = [ReversedLastLine|Lines],
	makeWordLines(Tokens, Width, Width, [[]|PreviousLines]);
% initial case where we have no existing lines on the stack
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
	
testProcess(Width) ->
	{ok, Text} = readFileText('threeten.text'),
	io:format("~s~n", [textProcess(Text, Width)]).
testJustify(Width) ->
	{ok, Text} = readFileText('threeten.text'),
	io:format("~s~n", [justifyText(Text, Width)]).
	
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

