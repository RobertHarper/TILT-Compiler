(* Type variables parameterized over types *)
functor Tyvar()
  : TYVAR =
  struct

    open Listops Name Util

    val error = fn s => error "tyvar.sml" s
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    datatype '1con tyvar = TYVAR of {name : var,
				     constrained : bool ref,
				     use_equal : bool ref,
				     body : '1con Util.oneshot}
    datatype '1con ocon  = OCON_UNINST of {name : var,
					  body : '1con,
					  var_constraint : (var * '1con list) list}
                         | OCON_INST of {name : var,
					 body : '1con,
					 tyvar_constraint : ('1con tyvar * '1con list) list}

    fun eq_tyvar(TYVAR{name=v1,...},TYVAR{name=v2,...}) = eq_var(v1,v2)
    fun tyvar_getvar(TYVAR{name,...}) = name
    fun tyvar_deref(TYVAR{body,...}) = oneshot_deref body
    fun tyvar_set(TYVAR{body,...},c) = (case (oneshot_deref body) of
					  NONE => oneshot_set(body,c)
					|(SOME _)  => error "cannot set tyvar more than once")
    fun tyvar_constrain(TYVAR{constrained,...}) = constrained := true
    fun tyvar_isconstrained(TYVAR{constrained,body,...}) = (!constrained orelse 
							    (case (oneshot_deref body) of
							       NONE => false
							     | SOME _ => true))
    fun tyvar_use_equal(TYVAR{use_equal,...}) = use_equal := true
    fun tyvar_is_use_equal(TYVAR{use_equal,...}) = !use_equal

    fun fresh_tyvar s = TYVAR{name = fresh_named_var "tv", 
			      constrained = ref false, 
			      use_equal = ref false,
			      body = oneshot()}
    fun fresh_var_tyvar v = TYVAR{name = v,
				  constrained = ref false, 
				  use_equal = ref false,
				  body = oneshot()}

    fun fresh_ocon (body, var_constraint) = OCON_UNINST{name=fresh_var(),
							body = body,
							var_constraint = var_constraint}
    fun ocon_getvar(OCON_INST{name,...}) = name
      | ocon_getvar(OCON_UNINST{name,...}) = name
    fun ocon_is_inst(OCON_UNINST _) = false
      | ocon_is_inst(OCON_INST _) = true
    fun ocon_deref(OCON_UNINST _) = error "ocon_deref: ocon uninstantiated"
      | ocon_deref(OCON_INST {body,...}) = body
    fun ocon_inst(OCON_INST _) _ = error "ocon_inst: ocon already instantiated"
      | ocon_inst(OCON_UNINST {name,body,var_constraint}) subster =
      let 
	val var_list = map #1 var_constraint
	val constr_list = map #2 var_constraint
	val tyvar_list = map (fn v => fresh_var_tyvar v) var_list
	val newbody = subster(body,zip var_list tyvar_list)
      in OCON_INST{name=name,
		   body=newbody,
		   tyvar_constraint = zip tyvar_list constr_list}
      end
    fun ocon_check(OCON_UNINST _, _) = error "ocon_check: ocon uninstantiated"
      | ocon_check(OCON_INST{name,body,tyvar_constraint}, eq_con) = 
      let fun help pos c [] = NONE
	    | help pos c (con::rest) = if eq_con(c,con) then (SOME pos)
				       else help (pos+1) c rest
	fun loop [] acc = SOME(rev acc)
	  | loop ((tv,cons)::rest) acc = (case tyvar_deref tv of
					    NONE => error "ocon_check: uninstantiated tyvar"
					  | SOME c => (case (help 0 c cons) of
							 NONE => NONE
						       | SOME pos => loop rest (pos::acc)))
      in loop tyvar_constraint []
      end

  end
