(*$import Il *)
(* Equality compiler *)
signature EQUAL = 
  sig

    val debug : bool ref
	
    (* Equality at a type or tuple of types arising from a recursive type *)

    val compile : {polyinst_opt : 
		     Il.context * Il.sdecs -> 
		       (Il.sbnd list * Il.sdecs * Il.con list) option,
		   vector_eq : Il.context -> Il.exp * Il.con,
		   context : Il.context,
		   con : Il.con} -> (Il.exp * Il.con) option


end
