(*$import ILCONTEXT Ast *)
(* Datatype compiler and destructurer of compiled modules/signatures. *)
signature DATATYPE = 
  sig
    structure IlContext : ILCONTEXT

    val debug : bool ref

    val exn_lookup : IlContext.Il.context -> Ast.path -> 
	{stamp : IlContext.Il.exp,
	 carried_type : IlContext.Il.con option} option


    (* Takes a context, a procedure for type expression compilation, and
       datatype and withtype specifications, and returns a module and 
       signature for the datatype declaration. *)
    val compile : {transparent : bool,
		   context : IlContext.context,
		   typecompile : IlContext.context * Ast.ty -> IlContext.Il.con,
		   datatycs : Ast.db list,
		   eq_compile : IlContext.context * IlContext.con -> IlContext.exp option,
		   eq_compile_mu : IlContext.context * IlContext.con -> IlContext.exp option} -> 
	                        (IlContext.Il.sbnd * IlContext.Il.sdec) list


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
    val is_constr : IlContext.Il.context -> Ast.path -> bool
    val is_nonconst_constr : IlContext.Il.context -> Ast.path -> bool


    val instantiate_datatype_signature : 
            IlContext.Il.context * Ast.path *
	    (IlContext.Il.context * IlContext.Il.sdecs -> 
	     IlContext.Il.sbnd list * IlContext.Il.sdecs * IlContext.Il.con list)
	    -> {instantiated_type : IlContext.Il.con,
		instantiated_sumtype : IlContext.Il.con,
		instantiated_special_sumtype : IlContext.Il.con list,
		arms : {name : IlContext.Il.label, arg_type : IlContext.Il.con option} list,
		expose_exp : IlContext.Il.exp}
			 
  end;
