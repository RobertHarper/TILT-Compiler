(*$import Prelude *)

signature TYVAR = 
  sig

    val debug : bool ref (* the usual debug flag *)

    type stamp

    (* type meta-variable used for type inference *)
    type ('ctxt,'con) tyvar_info     
    datatype ('ctxt,'con) tyvar = TYVAR of ('ctxt,'con) tyvar_info


    (* overloaded type with constraints 
       bool in constraint indicates side-effecting unification *)
    type ('ctxt,'con) constraint = (unit -> unit) * (('ctxt,'con) tyvar * bool -> bool)
    type ('ctxt,'con) ocon

    val get_stamp           : unit   -> stamp
    val fresh_tyvar         : 'ctxt  -> ('ctxt,'con) tyvar (* create a new initially unset tyvar *)
    val fresh_named_tyvar   : 'ctxt * string -> ('ctxt,'con) tyvar (* create a new initially unset tyvar *)
    val fresh_stamped_tyvar : 'ctxt * string * stamp -> ('ctxt,'con) tyvar
    val tyvar_copy          : ('ctxt,'con) tyvar -> ('ctxt,'con) tyvar
    val tyvar_update        : ('ctxt,'con) tyvar * ('ctxt,'con) tyvar -> unit
    val tyvar_after         : stamp -> ('ctxt,'con) tyvar -> bool (* true if tycar created after stamp *)
    val tyvar_stamp         : ('ctxt,'con) tyvar -> stamp
    val stamp2int           : stamp -> int
    val update_stamp        : ('ctxt,'con) tyvar * stamp -> unit  (* decreases the stamp, never increases *)
    val stamp_join          : stamp * stamp -> stamp       (* returns the earlier stamp *)
    val eq_tyvar            : ('ctxt,'con) tyvar * ('ctxt,'con) tyvar -> bool
    val tyvar2string        : ('ctxt,'con) tyvar -> string    (* for printing purposes only *)
    val tyvar_deref         : ('ctxt,'con) tyvar -> 'con option
    val tyvar_set           : ('ctxt,'con) tyvar * 'con -> unit
    val tyvar_constrain     : ('ctxt,'con) tyvar -> unit
    val tyvar_isconstrained : ('ctxt,'con) tyvar -> bool
    val tyvar_use_equal     : ('ctxt,'con) tyvar -> unit
    val tyvar_is_use_equal  : ('ctxt,'con) tyvar -> bool
    val tyvar_getctxts      : ('ctxt,'con) tyvar -> 'ctxt list
    val tyvar_addctxts      : ('ctxt,'con) tyvar * 'ctxt list -> unit


    val fresh_ocon  : 'ctxt * ('ctxt,'con) constraint list -> ('ctxt,'con) ocon 
                                              (* create an overloaded type 
                                                 with a list of the possible types 
						 and some action to perform when instantiated *)

    val ocon2string  : ('ctxt,'con) ocon -> string   (* for display purposes only; not an injection *)
    val ocon_deref   : ('ctxt,'con) ocon -> ('ctxt,'con) tyvar (* return the current internal type *)

    (* ocon_constrain takes an overloaded type
          the constraints are checked using the non-side-effecting version;
	  if exactly one constraint is satisfied, the side-effecting unifier is called
	  and the unit -> unit thunk is trigger *)
    val ocon_constrain  : ('ctxt,'con) ocon -> int

  end
      
