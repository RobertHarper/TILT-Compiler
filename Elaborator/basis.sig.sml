(* Contains the initial basis context to translate in. *)
signature BASIS = 
  sig
    structure Il : IL
    structure Datatype : DATATYPE

    val empty_context : Il.context
    val initial_context : (Il.context * Ast.ty -> Il.con) * (Il.context * Il.con -> Il.exp) 
	-> Il.context * Il.sbnd list

  end;
