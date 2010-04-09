-module(threenine).
-export([index/1]).

index(Filename) ->
	case file:open(Filename, read) of
		{ok, IoDevice} -> 
			indexFile(IoDevice),
			file:close(IoDevice);
		{error, Reason} -> {error, Reason}
	end.

indexFile(IoDevice) ->
	RawDocument = loadRawDocument([], IoDevice),
	Document = loadDocument(RawDocument),
	WordIndex1 = indexWords(Document),
	WordIndex2 = updateWordIndexLineNumbers(WordIndex1),
	prettyPrintWordIndex(WordIndex2),
	ok.

loadRawDocument(Lines, IoDevice) ->
	case file:read_line(IoDevice) of
		{ok, Line} -> loadRawDocument([Line|Lines], IoDevice);
		eof -> lists:reverse(Lines)
	end.

loadDocument(RawDocument) ->
	MakeDoc = fun(Line, {LineNo, Document}) -> 
		Tokens = string:tokens(Line, "\r\n\t {,}->:;[]().|\\=/~\"'+_"),
		{LineNo+1, [{LineNo, Tokens}|Document]}
	end,
	{_LineNumber, Document} = lists:foldl(MakeDoc, {1, []}, RawDocument),
	lists:reverse(Document).

indexWordsFun(Word, {LineNo, WordIndex}) ->
	LowerWord = string:to_lower(Word),
	LineNumbersList = case lists:keyfind(LowerWord, 1, WordIndex) of
		{LowerWord, LineNumbers} -> LineNumbers;
		false -> []
	end,
	UpdatedWordIndex = lists:keystore(LowerWord, 1, WordIndex, {LowerWord, [LineNo|LineNumbersList]}),
	{LineNo, UpdatedWordIndex}.

updateWordIndexLineNumbers(WordIndex) ->
	Update = fun({Word, LineNumbers}, NewWordIndex) ->
		UpdatedLineNumbers = cleanNumbers(LineNumbers, []),
		[{Word, UpdatedLineNumbers}|NewWordIndex]
	end,
	lists:foldr(Update, [], WordIndex).
	
cleanNumbers([], CleanNumbers) ->
	CleanNumbers;
cleanNumbers([Number|LineNumbers], []) ->
	cleanNumbers(LineNumbers, [{Number, Number}]);
cleanNumbers([Number|LineNumbers], [{_Start, Number}|_Tail]=NewNumbers) ->
	cleanNumbers(LineNumbers, NewNumbers);
cleanNumbers([Number|LineNumbers], [{Start, End}|Tail]) when Number == End+1 ->
	cleanNumbers(LineNumbers, [{Start, Number}|Tail]);
cleanNumbers([Number|LineNumbers], NewNumbers) ->
	cleanNumbers(LineNumbers, [{Number, Number}|NewNumbers]).
	
indexWords(Document) ->
	IndexWords = fun({LineNo, Words}, WordIndex) ->
		{LineNo, WordIndex2} = lists:foldr(fun indexWordsFun/2, {LineNo, WordIndex}, Words),
		WordIndex2
	end,
	lists:foldr(IndexWords, [], Document).

makePrintFormat(WordIndex) ->
	MaxLength = lists:foldr(fun({Word,_Lines}, Max) -> erlang:max(string:len(Word), Max) end, 0, WordIndex),
	io_lib:format("~~-~Bs ~~s~~n", [MaxLength]).

prettyPrintWordIndex(WordIndex) ->
	PrintFormat = makePrintFormat(WordIndex),
	PrintWord = fun({Word, LineNumbers}, Format) ->
		PrettyLineNumbers = makePrettyLineNumbers(LineNumbers),
		io:format(list_to_binary(PrintFormat), [Word, PrettyLineNumbers]),
		Format
	end,
	lists:foldr(PrintWord, PrintFormat, lists:reverse(lists:keysort(1,WordIndex))).

getPrettyLineNumbers(Result, []) ->
	Result;
getPrettyLineNumbers(Result, [{Number,Number}|LineNumbers]) ->
	getPrettyLineNumbers([integer_to_list(Number)|Result], LineNumbers);
getPrettyLineNumbers(Result, [{Start,End}|LineNumbers]) ->
	Range1=string:concat(integer_to_list(Start), "-"),
	Range2=string:concat(Range1, integer_to_list(End)),
	getPrettyLineNumbers([Range2|Result], LineNumbers).

makePrettyLineNumbers(LineNumbers) ->
	RangedLineNumbers = getPrettyLineNumbers([], LineNumbers),
	string:join(RangedLineNumbers, ", ").

