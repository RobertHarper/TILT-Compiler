(*$import FrontEnd SourceMap *)

signature LINK_PARSE =
 sig 
   exception Parse of FrontEnd.parseResult
   type filepos = SourceMap.charpos -> string * int * int
   val parse_impl : string -> filepos * string list * Ast.dec
   val parse_inter : string -> filepos * string list * Ast.spec list
 end
