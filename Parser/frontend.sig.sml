(*$import Prelude Ast Source *)

signature FRONT_END = 
sig
    datatype parseResult
      = EOF   
      | ERROR 
      | ABORT 
      | PARSE_IMPL of int * string list * Ast.dec
      | PARSE_INTER of int * string list * Ast.spec list

    val parse : Source.inputSource -> parseResult
end (* signature FRONT_END *)
