(* The translation of types from IL back to AST for error messaging. *)
signature TOAST = 
  sig
    structure Il : IL

    val debug : bool ref

    val xty    : Il.context * Il.con -> Ast.ty

  end;