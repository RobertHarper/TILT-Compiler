signature LINK_PARSE =
 sig
   val LinkParseDiag : bool ref
   type filepos = SourceMap.charpos -> string * int * int
   type 'a parser = string * string -> (filepos * 'a) option (* unit name, file name *)

   val parse_impl : Ast.dec parser
   val parse_inter : Ast.spec list parser
 end

