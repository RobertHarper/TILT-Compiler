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
    datatype '1con uocon  = UOCON of {constraints : '1con list}
    datatype '1con ocon = OCON of {constraints : (int option ref * '1con) list,
				   name : var,
				   body : '1con tyvar}
	
    fun fresh_named_tyvar s = TYVAR{name = fresh_named_var s, 
			      constrained = ref false, 
			      use_equal = ref false,
			      body = oneshot()}
    fun fresh_tyvar () = fresh_named_tyvar "tv"

    fun eq_tyvar(TYVAR{name=v1,...},TYVAR{name=v2,...}) = eq_var(v1,v2)
    fun tyvar2string(TYVAR{name=v,...}) = var2string v
    fun tyvar_getvar(TYVAR{name,...}) = name
    fun tyvar_deref(TYVAR{body,...}) = oneshot_deref body
    fun tyvar_set(TYVAR{body,...},c) = (case (oneshot_deref body) of
					  NONE => oneshot_set(body,c)
					|(SOME _)  => error "cannot set tyvar more than once")
    fun tyvar_constrain(TYVAR{constrained,...}) = constrained := true
    fun tyvar_isconstrained(TYVAR{constrained,body,...}) = !constrained 

    fun tyvar_use_equal(TYVAR{use_equal,...}) = use_equal := true
    fun tyvar_is_use_equal(TYVAR{use_equal,...}) = !use_equal



    fun fresh_uocon c = UOCON{constraints = c}
    fun uocon_inst (UOCON{constraints}) = OCON{constraints = mapcount (fn (i,c) => (ref (SOME i),c)) constraints,
					       name = fresh_named_var "ocon",
					       body = fresh_named_tyvar "ocon"}
    fun ocon2string (OCON{name,...}) = var2string name
    fun ocon_deref (OCON{body,...}) = body


    fun ocon_satisfy(OCON{constraints,name,body},soft_eqcon) : int = 
	let 
	    fun help c (ref NONE,_) = ()
	      | help c (r,con) = if (soft_eqcon(c,con))
				     then ()
				 else r := NONE
	    fun count [] = 0
	      | count ((ref (SOME _),_)::rest) = 1 + count rest
	      | count (_::rest) = count rest
	in (case tyvar_deref body of
		NONE => ()
	      | SOME c => app (help c) constraints);
	    count constraints
	end

    fun ocon_constrain(OCON{constraints,name,body},{hard,soft}) : int list = 
	let 
	    fun constrain (r as (ref (SOME i)), ct) = 
		(case (tyvar_deref body) of
		     NONE => SOME(i,ct)
		   | SOME c => if (soft(c,ct))
				    then SOME(i,ct)
				else (r := NONE; NONE))
	      | constrain _ = NONE
	    val left = List.mapPartial constrain constraints
	    val _ = case left of
		[(i,ct)] => (case tyvar_deref body of
				NONE => tyvar_set(body,ct)
			      | SOME c => if (hard(c,ct))
					       then ()
					   else error "softeq true but hardeq failed!!!")
	      | _ => ()
	in map #1 left
	end

  end
