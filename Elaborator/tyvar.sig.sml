(*$import Util *)

signature TYVAR = 
  sig

    type stamp

    (* type meta-variable used for type inference *)
    type ('ctxt,'con,'exp) tyvar_info     
    datatype ('ctxt,'con,'exp) tyvar = TYVAR of ('ctxt,'con,'exp) tyvar_info


    (* overloaded type with constraints 
       bool in constraint indicates side-effecting unification *)
    type ('ctxt,'con,'exp) constraint = (unit -> unit) * (('ctxt,'con,'exp) tyvar * bool -> bool)
    type ('ctxt,'con,'exp) ocon

    val new_stamp           : unit   -> stamp
    val fresh_tyvar         : 'ctxt  -> ('ctxt,'con,'exp) tyvar (* create a new initially unset tyvar *)
    val fresh_named_tyvar   : 'ctxt * string -> ('ctxt,'con,'exp) tyvar (* create a new initially unset tyvar *)
    val fresh_stamped_tyvar : 'ctxt * string * stamp -> ('ctxt,'con,'exp) tyvar
    val tyvar_copy          : ('ctxt,'con,'exp) tyvar -> ('ctxt,'con,'exp) tyvar
    val tyvar_update        : ('ctxt,'con,'exp) tyvar * ('ctxt,'con,'exp) tyvar -> unit
    val tyvar_after         : stamp -> ('ctxt,'con,'exp) tyvar -> bool (* true if tycar created after stamp *)
    val tyvar_stamp         : ('ctxt,'con,'exp) tyvar -> stamp
    val stamp2int           : stamp -> int
    val update_stamp        : ('ctxt,'con,'exp) tyvar * stamp -> unit  (* decreases the stamp, never increases *)
    val stamp_join          : stamp * stamp -> stamp       (* returns the earlier stamp *)
    val eq_tyvar            : ('ctxt,'con,'exp) tyvar * ('ctxt,'con,'exp) tyvar -> bool
    val tyvar2string        : ('ctxt,'con,'exp) tyvar -> string    (* for printing purposes only *)
    val tyvar_deref         : ('ctxt,'con,'exp) tyvar -> 'con option
    val tyvar_set           : ('ctxt,'con,'exp) tyvar * 'con -> unit
    val tyvar_reset         : ('ctxt,'con,'exp) tyvar * 'con -> unit
    val tyvar_constrain     : ('ctxt,'con,'exp) tyvar -> unit
    val tyvar_isconstrained : ('ctxt,'con,'exp) tyvar -> bool
    val tyvar_use_equal     : ('ctxt,'con,'exp) tyvar -> unit
    val tyvar_is_use_equal  : ('ctxt,'con,'exp) tyvar -> bool
    val tyvar_eq_hole       : ('ctxt,'con,'exp) tyvar -> 'exp Util.oneshot option
    val tyvar_getctxts      : ('ctxt,'con,'exp) tyvar -> 'ctxt list
    val tyvar_addctxts      : ('ctxt,'con,'exp) tyvar * 'ctxt list -> unit


    val fresh_ocon  : 'ctxt * ('ctxt,'con,'exp) constraint list * int -> ('ctxt,'con,'exp) ocon 
                                              (* create an overloaded type 
                                                 with a list of the possible types, each specifying some
						 action to perform when instantiated, and a default. *)

    val ocon2string  : ('ctxt,'con,'exp) ocon -> string   (* for display purposes only; not an injection *)
    val ocon_deref   : ('ctxt,'con,'exp) ocon -> ('ctxt,'con,'exp) tyvar (* return the current internal type *)

    (* ocon_constrain takes an overloaded type
          the constraints are checked using the non-side-effecting version;
	  if exactly one constraint is satisfied, the side-effecting unifier
	  is called and the unit -> unit thunk is triggered.
       Returns the number of constrains which are satisfied.
       ocon_constrain_default uses the default when its among the constraints which are
       satisfied. *)
    val ocon_constrain         : ('ctxt,'con,'exp) ocon -> int
    val ocon_constrain_default : ('ctxt,'con,'exp) ocon -> int
  end
      
