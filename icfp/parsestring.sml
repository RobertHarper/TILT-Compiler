
structure ParseString =
struct

    fun parse s = (Parsing.parse Parse.prog (Parsing.transform
																						 Tokens.token (Pos.markstream 
																													 (Stostream.stostream s))))

    fun tokenize s = (Stream.toList (Parsing.transform Tokens.token
																		 (Pos.markstream (Stostream.stostream s))))

end
