(* Datatype compiler and destructurer of compiled modules/signatures. *)
signature DATATYPE = 
  sig
    structure IlContext : ILCONTEXT

    val debug : bool ref

    (* Takes a context, a procedure for type expression compilation, and
      datatype and withtype specifications, and returns a module and 
      signature for the datatype declaration. *)
    val compile : {context : IlContext.context,
		   typecompile : IlContext.context * Ast.ty -> IlContext.Il.con,
		   datatycs : Ast.db list,
		   eq_compile : IlContext.context * IlContext.con -> IlContext.exp option} -> 
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
    val constr_lookup : IlContext.Il.context -> Ast.path -> 
	{name : IlContext.Il.label,
	 datatype_path : IlContext.Il.path,
	 is_const : bool,
	 datatype_sig : IlContext.Il.signat} option 
	

    val exn_lookup : IlContext.Il.context -> Ast.path -> {stamp : IlContext.Il.exp,
						carried_type : IlContext.Il.con option} option



    (* Given the signature for a datatype, we destructure to provide
        the name of the datatype, the abstract type naming the datatype,
	and the arm types *)
    val destructure_datatype_signature : 
                         IlContext.Il.signat -> {name : IlContext.Il.label,
				       var_poly : IlContext.Il.var option,
				       sdecs_poly : IlContext.Il.sdecs option,
				       arm_types : {name : IlContext.Il.label, arg_type : IlContext.Il.con option} list}

    val instantiate_datatype_signature : 
                         IlContext.Il.path * IlContext.Il.signat * IlContext.Il.context * 
			 (IlContext.Il.context * IlContext.Il.sdecs -> IlContext.Il.sbnd list * IlContext.Il.sdecs * IlContext.Il.con list)
			 -> {instantiated_type : IlContext.Il.con,
			     arms : {name : IlContext.Il.label, arg_type : IlContext.Il.con option} list,
(*			     case_exp : IlContext.Il.exp, *)
			     expose_exp : IlContext.Il.exp}
			 
  end;
