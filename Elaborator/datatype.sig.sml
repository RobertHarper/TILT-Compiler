(* Datatype compiler and destructurer of compiled modules/signatures. *)
signature DATATYPE = 
  sig
    structure Il : IL

    val debug : bool ref

    (* Takes a context, a procedure for type expression compilation, and
      datatype and withtype specifications, and returns a module and 
      signature for the datatype declaration. *)
    val compile : {context : Il.context,
		   typecompile : Il.context * Ast.ty -> Il.con,
		   datatycs : Ast.db list,
		   withtycs : Ast.tb list} -> (Il.sbnd * Il.sdec) list


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
    val constr_lookup : Il.context -> Ast.path -> {name : Il.label,
						   datatype_path : Il.path,
						   constr_sig : Il.signat,
						   datatype_sig : Il.signat} option 

    val old_exn_lookup : Il.context -> Ast.path -> {name : Il.label,
						    carried_type : Il.con option} option
    val exn_lookup : Il.context -> Ast.path -> {stamp : Il.exp,
						carried_type : Il.con option} option

    val is_const_constr : Il.signat -> bool


    (* Given the signature for a datatype, we destructure to provide
        the name of the datatype, the abstract type naming the datatype,
	and the arm types *)
    val destructure_datatype_signature : 
                         Il.signat -> {name : Il.label,
				       abstract_type : Il.con,
				       var_poly : Il.var option,
				       sdecs_poly : Il.sdecs option,
				       arm_types : {name : Il.label, arg_type : Il.con option} list}

    val instantiate_datatype_signature : 
                         Il.path * Il.signat * Il.context * 
			 (Il.decs * Il.sdecs -> Il.sbnd list * Il.sdecs * Il.con list)
			 -> {instantiated_type : Il.con,
			     arms : {name : Il.label, arg_type : Il.con option} list,
			     case_exp : Il.exp,
			     expose_exp : Il.exp}
			 
  end;
