(*$import Il Tyvar *)
(* Equality compiler *)
signature EQUAL = 
  sig

    val debug : bool ref
	
    val installHelpers : { add_eq_entry : (Il.context,Il.con,Il.exp) Tyvar.tyvar -> unit } -> unit

    (* Equality at a type or tuple of types arising from a recursive type *)

    val compile : {polyinst_opt : 
		     Il.context * Il.sdecs -> 
		       (Il.sbnd list * Il.sdecs * Il.con list) option,
		   vector_eq : Il.context -> Il.exp * Il.con,
		   bool : (Il.con * Il.exp * Il.exp) option,
		   context : Il.context,
		   con : Il.con} -> (Il.exp * Il.con) option


end
