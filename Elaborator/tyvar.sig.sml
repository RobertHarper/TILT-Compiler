signature TYVAR = 
  sig

    val debug : bool ref (* the usual debug flag *)

    type ('decs,'1con) tyvar           (* type meta-variable used for type inference *)
    type stamp

    type '1con con_helpers = {hard : '1con * '1con -> bool,
			      soft : '1con * '1con -> bool}

    (* bool represents hardness *)
    datatype 'a status = FAIL | MAYBE | MATCH of 'a
    type ('decs,'1con,'a) constraint = ('decs,'1con) tyvar * '1con con_helpers * bool -> 'a status
    type ('decs,'1con,'a) uocon   (* uninstantiated overloaded type with constraints *)
    type ('decs,'1con) ocon         (* uninstantiated overloaded type with constraints *)

    val get_stamp           : unit   -> stamp
    val fresh_tyvar         : 'decs  -> ('decs,'1con) tyvar (* create a new initially unset tyvar *)
    val fresh_named_tyvar   : 'decs * string -> ('decs,'1con) tyvar (* create a new initially unset tyvar *)
    val fresh_stamped_tyvar : 'decs * string * stamp -> ('decs,'1con) tyvar
    val tyvar_after         : stamp -> ('decs,'1con) tyvar -> bool (* true if tycar created after stamp *)
    val tyvar_stamp         : ('decs,'1con) tyvar -> stamp
    val stamp2int           : stamp -> int
    val update_stamp        : ('decs,'1con) tyvar * stamp -> unit  (* decreases the stamp, never increases *)
    val stamp_join          : stamp * stamp -> stamp       (* returns the earlier stamp *)
    val eq_tyvar            : ('decs,'1con) tyvar * ('decs,'1con) tyvar -> bool
    val tyvar2string        : ('decs,'1con) tyvar -> string    (* for printing purposes only *)
    val tyvar_deref         : ('decs,'1con) tyvar -> '1con option
    val tyvar_set           : ('decs,'1con) tyvar * '1con -> unit
    val tyvar_constrain     : ('decs,'1con) tyvar -> unit
    val tyvar_isconstrained : ('decs,'1con) tyvar -> bool
    val tyvar_use_equal     : ('decs,'1con) tyvar -> unit
    val tyvar_is_use_equal  : ('decs,'1con) tyvar -> bool
    val tyvar_getdecs       : ('decs,'1con) tyvar -> 'decs
    val tyvar_setdecs       : ('decs,'1con) tyvar * 'decs -> unit

    val fresh_uocon  : ('decs,'1con,'a) constraint list -> ('decs,'1con,'a) uocon 
                                              (* create an uninstantiated overloaded type 
                                                 with a list of the possible types *)
    val uocon_inst   : 'decs * ('decs,'1con,'a) uocon * ('a -> unit) -> ('decs,'1con) ocon
	                                      (* generate an instantiated overloaded type
					         from an uninstantiated one *)

    val ocon2string  : ('decs,'1con) ocon -> string        (* for display purposes only; not an injection *)
    val ocon_deref   : ('decs,'1con) ocon -> ('decs,'1con) tyvar (* return the current internal type *)

    (* ocon_constrain takes an instantiated overloaded type and some helper functions
          the constraints are checked using the non-side-effecting version;
	  the 'a indices returned by the remaining constraints that could be satisfied is returned;
	  if the number of constraints left is one, it will try to eagerly use the side-effecting
	  version of the unifier to unify the constraint against the internal variable;
	  and call the 'a -> unit function *)
    val ocon_constrain  : ('decs,'1con) ocon * '1con con_helpers -> (int * bool) list

  end
      
