signature LINK_PARSE =
 sig
   val LinkParseDiag : bool ref
   type filepos = SourceMap.charpos -> string * int * int
   type 'a parser = string * string -> (filepos * 'a) option (* unit or interface name, file name *)

   val lexer_initial_position : int

   val parse_topdec : Ast.dec parser
   val parse_topspec: Ast.topspec parser
 end

