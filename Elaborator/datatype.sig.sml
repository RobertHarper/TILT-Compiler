(*$import Prelude IL Ast Il *)
(* Datatype compiler and destructurer of compiled modules/signatures. *)
signature DATATYPE = 
  sig
    val debug : bool ref

    val exn_lookup : Il.context -> Ast.path -> 
	{stamp : Il.exp,
	 carried_type : Il.con option} option


    (* Takes a context, procedures for type and equality compilation, and
       a datatype declaration, and returns a module and signature for
       the datatype declaration.  *)
    val compile : {context : Il.context,
		   typecompile : Il.context * Ast.ty -> Il.con,
		   datatycs : Ast.db list,
		   eq_compile : Il.context * Il.con -> (Il.exp * Il.con) option,
		   is_transparent : bool} ->
	                        (Il.sbnd * Il.sdec) list


    (* The datatype module/signature returned will be such that:
        when given a context, a mod/sig lookup method, and a path,
	       if the path looks up to a datatype constructor,
		   constr_lookup will return 
		   (1) the name of the datatype(tycon), 
		   (2) the datatype path,
		   (3) the datatype signature, 
		   (4) and the datatype arm signature 

	As a helper function, the is_const_constr takes the signature 
	   for the datatype arm, and indicates whether it is a 
	   const or non value-carrying constructor. *)

    (* Predicate for identifying constructors and separating value-carrying
       from non-value-carrying ones *)
    val is_constr : Il.context -> Ast.path -> bool
    val is_nonconst_constr : Il.context -> Ast.path -> bool


    val instantiate_datatype_signature : 
            Il.context * Ast.path *
	    (Il.context * Il.sdecs -> 
	     Il.sbnd list * Il.sdecs * Il.con list)
	    -> {instantiated_type : Il.con,
		instantiated_sumtype : Il.con,
		arms : {name : Il.label, arg_type : Il.con option} list,
		expose_exp : Il.exp}
			 
  end;
