(*$import FrontEnd SourceMap *)

signature LINK_PARSE =
 sig 
   type filepos = SourceMap.charpos -> string * int * int
   val parse_impl : string -> int * filepos * string list * Ast.dec
   val parse_inter : string -> int * filepos * string list * Ast.spec list
 end

