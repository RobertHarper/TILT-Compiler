
structure ParseString =
struct

    fun file2string file = 
	let val ins = TextIO.openIn file
	    val string = TextIO.inputAll ins
	    val _ = TextIO.closeIn ins
	in  string
	end
    fun parse s = (Parsing.parse Parse.prog (Parsing.transform
																						 Tokens.token (Pos.markstream 
																													 (Stostream.stostream s))))

    fun tokenize s = (Stream.toList (Parsing.transform Tokens.token
																		 (Pos.markstream (Stostream.stostream s))))

end
