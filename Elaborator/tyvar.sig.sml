signature TYVAR = 
  sig

    val debug : bool ref (* the usual debug flag *)

    type '1con tyvar     (* type meta-variable used for type inference *)
    type '1con uocon      (* uninstantiated overloaded type with constraints *)
    type '1con ocon      (* uninstantiated overloaded type with constraints *)


    val fresh_tyvar        : unit   -> '1con tyvar   (* create a new initially unset tyvar *)
    val fresh_named_tyvar  : string -> '1con tyvar   (* create a new initially unset tyvar *)
    val eq_tyvar     : '1con tyvar * '1con tyvar -> bool
(*    val tyvar_getvar : '1con tyvar -> Name.var  (* XXX should this be here *) *)
    val tyvar2string : '1con tyvar -> string    (* for printing purposes only *)

    val tyvar_deref  : '1con tyvar -> '1con option
    val tyvar_set    : '1con tyvar * '1con -> unit
    val tyvar_constrain : '1con tyvar -> unit
    val tyvar_isconstrained : '1con tyvar -> bool
    val tyvar_use_equal : '1con tyvar -> unit
    val tyvar_is_use_equal : '1con tyvar -> bool


    val fresh_uocon  : '1con list -> '1con uocon 
                                              (* create an uninstantiated overloaded type 
                                                 with a list of the possible types *)
    val uocon_inst   : '1con uocon -> '1con ocon
	                                      (* generate an instantiated overloaded type
					         from an uninstantiated one *)

    val ocon2string  : '1con ocon -> string      (* for display purposes only; not an injection *)
    val ocon_deref   : '1con ocon -> '1con tyvar (* return the internal metavariable *)

    (* ocon_constrain takes an instantiated overloaded type and two
          constructor equivalence predicate(the first one side-effecting, the second not)
          that is effectless; the constraints are checked using the non-side-effecting version;
	  the zero-based indices of the remaining constraints that are satisfied is returned;
	  if the number of constraints left is one, it will eagerly use the side-effecting
	  version of the unifier to unify the constraint against the internal variable *)
    val ocon_constrain  : '1con ocon * {hard : '1con * '1con -> bool,
					soft : '1con * '1con -> bool} -> int list

  end
      
