functor JoinWithArg(
		type pos
		structure ParserData : PARSER_DATA
		sharing type pos = ParserData.pos
	)
		 = struct end
