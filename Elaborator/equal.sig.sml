(*$import Il *)
(* Equality compiler *)
signature EQUAL = 
  sig

    val debug : bool ref
	
    (* Equality at a type *)
    val compile : {polyinst_opt : 
		     Il.context * Il.sdecs -> 
		       (Il.sbnd list * Il.sdecs * Il.con list) option,
		   vector_eq : Il.context -> Il.exp * Il.con,
		   context : Il.context,
		   con : Il.con} -> Il.exp option

    (* Equality at a recursive type - returns a tuple of functions *)
    val compile_mu : {polyinst_opt :
		        Il.context * Il.sdecs -> 
		          (Il.sbnd list * Il.sdecs * Il.con list) option,
		      vector_eq : Il.context -> Il.exp * Il.con,
		      context : Il.context,
		      confun  : Il.con} -> Il.exp option
 

end
