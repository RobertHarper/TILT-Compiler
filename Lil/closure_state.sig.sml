signature CLOSURESTATE = 
  sig
    type fid = Lil.var


    structure Frees :
      sig
	
	type frees = {vars32 : Name.VarSet.set,
		      vars64 : Name.VarSet.set,
		      cvars  : Name.VarSet.set}
	  
	val empty_frees : frees

	val is_empty : frees -> bool

	val join_frees : frees * frees -> frees 

	val print_frees : frees -> unit

	(*Is the second set of frees contained in the first*)
	val contains_frees : frees * frees -> bool
	val free_var64_add  : frees * Lil.var -> frees
	val free_var32_add  : frees * Lil.var -> frees
	val free_cvar_add   : frees * Lil.var -> frees
      end

    structure State :
      sig
	type state
	val initial_state   : fid -> state
	val add_boundvars32 : state * Lil.var list -> state
	val add_boundvars64 : state * Lil.var list -> state
	val add_boundcvars  : state * Lil.var list -> state
	val add_boundvar32  : state * Lil.var -> state
	val add_boundvar64  : state * Lil.var -> state
	val add_boundcvar   : state * Lil.var -> state
	val add_globalvar32 : state * Lil.var -> state
	val add_globalcvar  : state * Lil.var -> state
	val hide_globalcvar : state * Lil.var -> state

        (* A variable is local if it is bound locally or is a global *)
        val var32_islocal : state * Lil.var -> bool
	val var64_islocal : state * Lil.var -> bool
	val cvar_islocal  : state * Lil.var -> bool
	  
        (* A variable is available if it is local/global or in a closure.
	 * 1) Check if v is in the free list (and hence in the eventual closure)
	 * 2) Check if v is local
	 * If neither applies, it is not available (and hence presumably must be 
	 * made available by adding to the free list
	 *)
        val var32_isavailable : state * Frees.frees * Lil.var -> bool
	val var64_isavailable : state * Frees.frees * Lil.var -> bool
	val cvar_isavailable : state * Frees.frees * Lil.var -> bool
	val is_boundfid : state * Lil.var -> bool

	(* promote_state : state * var list -> state *)
	(* Does the necessary bookkeeping to begin scanning a new function nest 
	 *)
	val promote_state : state * Lil.var list -> state

	(* replaces remove_free *)
	(* restrict_frees_with_state : state * frees -> frees *)
	(* restrict_frees_with_state (s,f) ==> g, where       *)
	(* g is f restricted to those variables that are not available in s *)
	val restrict_frees_with_state : state * Frees.frees -> Frees.frees

	val add_boundfids  : state * Lil.var list -> state
	val add_gboundfids : state * Lil.var list -> state

	val fid_in_nest : state  -> Lil.var -> bool

	val get_curfid : state -> Lil.var * Lil.var list
	val set_curfid : state -> fid * fid list -> state
      end 
      
  end