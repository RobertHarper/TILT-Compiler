structure ClosureState :> CLOSURESTATE = 
  struct
    open Lil

      
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

    val recursive_closures = Stats.ff "LilCloseRecClosures"
    val error = fn s => Util.error "lilclosure_analyze.sml" s
    val chatlev = ref 0       
    fun chat i = !chatlev >= i
      
    type fid = var

    structure Frees =
      struct
	type frees = {vars32 : VarSet.set,
		      vars64 : VarSet.set,
		      cvars : VarSet.set}
	  
	val empty_frees :frees = {vars32 = VarSet.empty,
				  vars64 = VarSet.empty,
				  cvars = VarSet.empty}
	  
	fun is_empty {vars32, vars64,cvars} = 
	  VarSet.isEmpty vars32 andalso
	  VarSet.isEmpty vars64 andalso
	  VarSet.isEmpty cvars

	fun print_frees {vars32,vars64,cvars} = 
	  let 
	    fun pp_list doer l = PpLil.pp_list doer l ("{",",","}",false)
	  in
	    print "\tFree con vars: ";pp_list PpLil.pp_var' (VarSet.listItems cvars);print "\n";
	    print "\tFree 32b vars: ";pp_list PpLil.pp_var' (VarSet.listItems vars32);print "\n";
	    print "\tFree 64b vars: ";pp_list PpLil.pp_var' (VarSet.listItems vars64);print "\n"
	  end

	fun join_frees (({vars32 = f32s, vars64 = f64s,cvars = cvars},
			 {vars32 = f32s',vars64 = f64s',cvars = cvars'}) : frees * frees) : frees = 
	  {vars32 = VarSet.union (f32s,f32s'),
	   vars64 = VarSet.union (f64s,f64s'),
	   cvars = VarSet.union (cvars,cvars')
	   }
	  
    (*Is the second set of frees contained in the first*)
	fun contains_frees (({vars32 = f32s, vars64 = f64s,cvars = cvars},
			     {vars32 = f32s',vars64 = f64s',cvars = cvars'}) : frees * frees)  = 
	  (VarSet.isSubset (f32s',f32s) andalso
	   VarSet.isSubset (f64s',f64s) andalso
	   VarSet.isSubset (cvars',cvars))


	fun free_var32_add ({vars32,vars64,cvars} : frees, evar) =
	  let 
	    val vars32 = VarSet.add(vars32,evar)
	  in  {vars32 = vars32,
	       vars64 = vars64,
	       cvars = cvars}
	  end

	fun free_var64_add ({vars32,vars64,cvars} : frees, evar) =
	  let 
	    val vars64 = VarSet.add(vars64,evar)
	  in  {vars32 = vars32,
	       vars64 = vars64,
	       cvars = cvars}
	  end
	
	fun free_cvar_add ({vars32,vars64,cvars} : frees, cvar) =
	  let 
	    val cvars = VarSet.add(cvars,cvar)
	  in  {vars32 = vars32,
	       vars64 = vars64,
	       cvars = cvars}
	  end

      end
    

    structure State = 
      struct
	  
	(*Tags to indicate variable status.  Currently no globals
	 * A variable marked GLOBAL is scheduled to be placed in the data
	 * segment and hence does not need to be included in the environment.
	 * A variable marked LOCAL is bound in the current function and so does not
	 * need to be included in the environment.
	 * A variable marked SHADOW is bound outside of the current function and is
	 * not global, so it must be included in the environment.
	 *
	 * Upon entering a new function, the state must be copied and all variables marked
	 * LOCAL must be changed to SHADOW.
	 * When we leave a function scope, we remove all variables from the free list
	 * except those marked SHADOW.
	 *)
      
	datatype status = SHADOW | LOCAL | GLOBAL

	datatype state = STATE of {curfid : fid * fid list,  (*The current function and the rest of 
								      * the functions in the current nest*)
				   boundvars32 : status VarMap.map,
				   boundvars64 : status VarMap.map,
				                             (*What 32 and 64 bit term variables are currently
							      * bound, and what is their status (see above)*)
				   boundcvars : status VarMap.map,
				   boundfids : VarSet.set}   (*The set of term variables bound
							      * above which correspond to functions.
							      *)

	local
	  fun add_boundvars (varset,v_list) = 
	    let 
	      fun folder(v,m) = VarMap.insert(m,v,LOCAL)
	      val varset = foldl folder varset v_list
	    in  varset
	    end
	in
	  fun add_boundvars32 (STATE{curfid,boundvars32,boundvars64,boundcvars,boundfids},v_list) = 
	    STATE{curfid = curfid,
		  boundvars32 = add_boundvars(boundvars32,v_list),
		  boundvars64 = boundvars64,
		  boundcvars = boundcvars,
		  boundfids = boundfids}



	  fun add_boundvars64 (STATE{curfid,boundvars32,boundvars64,boundcvars,boundfids},v_list) = 
	    STATE{curfid = curfid,
		  boundvars32 = boundvars32,
		  boundvars64 = add_boundvars(boundvars64,v_list),
		  boundcvars = boundcvars,
		  boundfids = boundfids}
	    

	  fun add_boundcvars (STATE{curfid,boundvars32,boundvars64,boundcvars,boundfids},v_list) = 
	    STATE{curfid = curfid,
		  boundvars32 = boundvars32,
		  boundvars64 = boundvars64,
		  boundcvars = add_boundvars(boundcvars,v_list),
		  boundfids = boundfids}


	  fun add_globalvar32 (STATE{curfid,boundvars32,boundvars64,boundcvars,boundfids},v) = 
	    let 
	      val boundvars32 = VarMap.insert(boundvars32,v,GLOBAL)
	    in  
	      STATE{curfid = curfid,
		    boundvars32 = boundvars32,
		    boundvars64 = boundvars64,
		    boundcvars = boundcvars,
		    boundfids = boundfids}
	    end

	  fun add_globalcvar (STATE{curfid,boundvars32,boundvars64,boundcvars,boundfids},v) = 
	    let 
	      val boundcvars = VarMap.insert(boundcvars,v,GLOBAL)
	    in  
	      STATE{curfid = curfid,
		    boundvars32 = boundvars32,
		    boundvars64 = boundvars64,
		    boundcvars = boundcvars,
		    boundfids = boundfids}
	    end
	  
	  fun add_boundvar32(s,v) = add_boundvars32(s,[v])
	  fun add_boundvar64(s,v) = add_boundvars64(s,[v])
	  fun add_boundcvar(s,v)  = add_boundcvars (s,[v])
	end




	local
	  (*replaces is_boundevar*)
	  fun is_local_varxx bound evar = 
	    (case (VarMap.find(bound,evar)) 
	       of SOME LOCAL => true
		| SOME GLOBAL => true
		| SOME SHADOW => false
		| _ => error ("is_local_varxx: variable " ^
			      (Name.var2string evar) ^ " not bound"))

	  (*replaces evar_isfree *)
	  fun varxx_isavailable (boundvars, freevars,var) = 
	    VarSet.member (freevars,var) orelse
	    is_local_varxx boundvars var

	in
	  (*If true, variable does not need to be in a closure *)
	  fun var32_islocal(STATE{boundvars32,...},evar) = is_local_varxx boundvars32 evar
	  fun var64_islocal(STATE{boundvars64,...},evar) = is_local_varxx boundvars64 evar
	  fun cvar_islocal(STATE{boundcvars,...}, evar) = is_local_varxx boundcvars evar

	(* A variable is available if it is local or in a closure.
	 * 1) Check if v is in the free list (and hence in the eventual closure)
         * 2) Check if v is local
	 * If neither applies, it is not available (and hence presumably must be 
	 * made available by adding to the free list)
	 *)
	  fun var32_isavailable (STATE{boundvars32,...},{vars32,...} : Frees.frees,evar) = 
	    varxx_isavailable (boundvars32,vars32,evar)
	  fun var64_isavailable (STATE{boundvars64,...},{vars64,...} : Frees.frees,evar) = 
	    varxx_isavailable (boundvars64,vars64,evar)
	  fun cvar_isavailable (STATE{boundcvars,...},{cvars,...} : Frees.frees,cvar) = 
	    varxx_isavailable (boundcvars,cvars,cvar)
	end

	fun is_boundfid(STATE{boundfids,...}, f) = VarSet.member(boundfids,f)


	(* fids are bound inside of recursive cluster, 
	 * If we have recursive closures, these should be marked shadow so
	 * that they will be added to the closure.  If wr are regenerating the local
	 * closures, then we mark them as local.
	 *)
	fun add_boundfids' shadow global (STATE{curfid,boundvars32,boundvars64,boundcvars,boundfids},fids) = 
	  let 
	    
	    val boundfids = VarSet.addList(boundfids,fids)
	      
	    fun folder(v,m) = VarMap.insert(m,v,if shadow then SHADOW else if global then GLOBAL else LOCAL)
	    val boundvars32 = foldl folder boundvars32 fids
	    val state = STATE{curfid = curfid,
			      boundvars32 = boundvars32,
			      boundvars64 = boundvars64,
			      boundcvars = boundcvars,
			      boundfids = boundfids}
	  in state
	  end

	val add_boundfids = add_boundfids' false false
	val add_gboundfids = add_boundfids' false true

	(* promote_state : state * (var * var list) -> state *)
	(* Does the necessary bookkeeping to begin scanning a new function nest; takes the state from the *)
	(* point just before the function and turns all the LOCALs into   *)
	(* SHADOWs to reflect the fact that we're entering a new scope.  Comment by joev, 8/2002.    *)
	fun promote_state (STATE{boundvars32,boundvars64,boundcvars,boundfids,curfid},fids)= 
	    let fun promote LOCAL = SHADOW
		  | promote x = x
		val boundvars32 = VarMap.map promote boundvars32
		val boundvars64 = VarMap.map promote boundvars64
		val boundcvars  = VarMap.map promote boundcvars
		val state = STATE{curfid=curfid,
				  boundfids=boundfids,
				  boundvars32 = boundvars32,
				  boundvars64 = boundvars64,
				  boundcvars = boundcvars}
	    in add_boundfids' (!recursive_closures) false (state,fids)
	    end

	fun get_curfid(STATE{curfid,...}) = curfid

	fun fid_in_nest (STATE{curfid=(_,fids),...}) fid = Listops.member_eq (Name.eq_var,fid,fids)

	fun set_curfid(STATE{boundvars32,boundvars64,boundcvars,boundfids,curfid}) (fid : fid * fid list) =
	  STATE{curfid=fid,
		boundfids=boundfids,
		boundvars32 = boundvars32,
		boundvars64 = boundvars64,
		boundcvars = boundcvars} 

	(* replaces remove_free *)
	(* restrict_frees_with_state : state * frees -> frees *)
	(* restrict_frees_with_state (s,f) ==> g, where       *)
	(* g is f restricted to those variables that are not local or global in s *)
	fun restrict_frees_with_state(s as (STATE{boundvars32,boundvars64,boundcvars,...}),
				      {vars32, vars64,cvars}) : Frees.frees =
	  let 
	    fun filter32 v = not (var32_islocal(s,v))
	    fun filter64 v = not (var64_islocal(s,v))
	    fun cfilter  v = not (cvar_islocal(s,v))
	    val vars32 = VarSet.filter filter32 vars32
	    val vars64 = VarSet.filter filter64 vars64
	    val cvars  = VarSet.filter cfilter  cvars
	      
	  in  {vars32 = vars32,
	       vars64 = vars64,
	       cvars = cvars}
	  end

	fun initial_state topfid = STATE{curfid=(topfid,[topfid]),
					 boundfids=VarSet.empty,
					 boundvars32 = VarMap.empty,
					 boundvars64 = VarMap.empty,
					 boundcvars = VarMap.empty}

      end  (* State *)
    
  end