(*$import Prelude *)

signature TYVAR = 
  sig

    val debug : bool ref (* the usual debug flag *)

    type stamp

    (* type meta-variable used for type inference *)
    type ('ctxt,'1con) tyvar_info     
    datatype ('ctxt,'1con) tyvar = TYVAR of ('ctxt,'1con) tyvar_info


    (* bool represents hardness *)
    datatype 'a status = FAIL | MAYBE | MATCH of 'a
    type ('ctxt,'1con,'a) constraint = ('ctxt,'1con) tyvar * bool -> 'a status
    type ('ctxt,'1con,'a) uocon   (* uninstantiated overloaded type with constraints *)
    (* uninstantiated overloaded type with constraints *)
    type ('ctxt,'1con) ocon_info
    datatype ('ctxt,'1con) ocon = OCON of ('ctxt,'1con) ocon_info

    val get_stamp           : unit   -> stamp
    val fresh_tyvar         : 'ctxt  -> ('ctxt,'1con) tyvar (* create a new initially unset tyvar *)
    val fresh_named_tyvar   : 'ctxt * string -> ('ctxt,'1con) tyvar (* create a new initially unset tyvar *)
    val fresh_stamped_tyvar : 'ctxt * string * stamp -> ('ctxt,'1con) tyvar
    val tyvar_copy          : ('ctxt,'1con) tyvar -> ('ctxt,'1con) tyvar
    val tyvar_update        : ('ctxt,'1con) tyvar * ('ctxt,'1con) tyvar -> unit
    val tyvar_after         : stamp -> ('ctxt,'1con) tyvar -> bool (* true if tycar created after stamp *)
    val tyvar_stamp         : ('ctxt,'1con) tyvar -> stamp
    val stamp2int           : stamp -> int
    val update_stamp        : ('ctxt,'1con) tyvar * stamp -> unit  (* decreases the stamp, never increases *)
    val stamp_join          : stamp * stamp -> stamp       (* returns the earlier stamp *)
    val eq_tyvar            : ('ctxt,'1con) tyvar * ('ctxt,'1con) tyvar -> bool
    val tyvar2string        : ('ctxt,'1con) tyvar -> string    (* for printing purposes only *)
    val tyvar_deref         : ('ctxt,'1con) tyvar -> '1con option
    val tyvar_set           : ('ctxt,'1con) tyvar * '1con -> unit
    val tyvar_constrain     : ('ctxt,'1con) tyvar -> unit
    val tyvar_isconstrained : ('ctxt,'1con) tyvar -> bool
    val tyvar_use_equal     : ('ctxt,'1con) tyvar -> unit
    val tyvar_is_use_equal  : ('ctxt,'1con) tyvar -> bool
    val tyvar_getctxts      : ('ctxt,'1con) tyvar -> 'ctxt list
    val tyvar_addctxts      : ('ctxt,'1con) tyvar * 'ctxt list -> unit


    val fresh_uocon  : ('ctxt,'1con,'a) constraint list -> ('ctxt,'1con,'a) uocon 
                                              (* create an uninstantiated overloaded type 
                                                 with a list of the possible types *)
    val uocon_inst   : 'ctxt * ('ctxt,'1con,'a) uocon * ('a -> unit) -> ('ctxt,'1con) ocon
	                                      (* generate an instantiated overloaded type
					         from an uninstantiated one *)

    val ocon2string  : ('ctxt,'1con) ocon -> string        (* for display purposes only; not an injection *)
    val ocon_deref   : ('ctxt,'1con) ocon -> ('ctxt,'1con) tyvar (* return the current internal type *)

    (* ocon_constrain takes an instantiated overloaded type and some helper functions
          the constraints are checked using the non-side-effecting version;
	  the 'a indices returned by the remaining constraints that could be satisfied is returned;
	  if the number of constraints left is one, it will try to eagerly use the side-effecting
	  version of the unifier to unify the constraint against the internal variable;
	  and call the 'a -> unit function *)
    val ocon_constrain  : ('ctxt,'1con) ocon -> (int * bool) list

  end
      
