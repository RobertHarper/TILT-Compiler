(*$import Ast SourceMap *)

signature LINK_PARSE =
 sig 
   type filepos = SourceMap.charpos -> string * int * int
   type 'a parser = string -> (int * filepos * string list * 'a) option

   val parse_impl : Ast.dec parser
   val parse_inter : Ast.spec list parser
 end

