(* Type variables parameterized over types *)
functor Tyvar()
  : TYVAR =
  struct

    open Listops Name Util

    val error = fn s => error "tyvar.sml" s
    val debug = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()

    val stamp = ref 0
    fun inc() = let val res = !stamp
		    val _ = stamp := (res + 1)
		in res
		end
    type stamp = int

    datatype ('ctxt,'1con) tyvar = TYVAR of {stampref : int ref,
					     name : var,
					     constrained : bool ref,
					     use_equal : bool ref,
					     body : '1con Util.oneshot,
					     ctxts : 'ctxt list ref}
    type '1con con_helpers = {hard : '1con * '1con -> bool,
			      soft : '1con * '1con -> bool}
    datatype 'a status = FAIL | MAYBE | MATCH of 'a
    type ('ctxt,'1con,'a) constraint = ('ctxt,'1con) tyvar * '1con con_helpers * bool -> 'a status (* bool represents hardness *)
	
    datatype ('ctxt,'1con,'a) uocon  = UOCON of {constraints : ('ctxt,'1con,'a) constraint list}
    datatype ('ctxt,'1con) ocon = OCON of {constrainer : '1con con_helpers -> (int * bool) list,
					   name : var,
					   body : ('ctxt,'1con) tyvar}
    fun get_stamp() = inc()
    fun stamp2int stamp = (stamp : int)
    fun stamp_join(a : stamp,b) = if (a<b) then a else b
    fun fresh_stamped_tyvar (ctxts,name,stamp) = TYVAR{stampref = ref stamp,
						      name = fresh_named_var name, 
						      constrained = ref false, 
						      use_equal = ref false,
						      body = oneshot(),
						      ctxts = ref [ctxts]}
    fun fresh_named_tyvar (ctxts,s) = fresh_stamped_tyvar(ctxts,s,inc())
    fun fresh_tyvar ctxts = fresh_named_tyvar (ctxts,"tv")
    fun tyvar_after stamp (TYVAR{stampref,...}) = stamp < !stampref
    fun tyvar_stamp (TYVAR{stampref,...}) = !stampref
    fun update_stamp (TYVAR{stampref,...},stamp) = let val curval = !stampref
						   in if (curval > stamp) 
							  then stampref := stamp else ()
						   end
					       (* (print "set_stamp setting stampref = !";
						    print (Int.toString (!stampref));
						    print " and stamp = ";
						    print (Int.toString stamp);
						    print "\n";
						 *)

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
    fun tyvar_getctxts(TYVAR{ctxts,...}) = !ctxts
    fun tyvar_addctxts(TYVAR{ctxts,...},ctxts') = ctxts := (ctxts' @ (!ctxts))


    fun fresh_uocon c = UOCON{constraints = c}
    fun ocon2string (OCON{name,...}) = var2string name
    fun ocon_deref (OCON{body,...}) = body

    fun uocon_inst (ctxts : 'ctxt,
		    UOCON{constraints} : ('ctxt,'1con,'a) uocon, thunk : 'a -> unit) : ('ctxt,'1con) ocon = 
	let
	    val body = fresh_named_tyvar (ctxts,"ocon")
	    val name = fresh_named_var "ocon"
	    val constraints = ref(mapcount (fn (i,c) => (i,c)) constraints)
	    fun constrainer (helpers as {hard : '1con * '1con -> bool,
					 soft : '1con * '1con -> bool}) : (int * bool) list =
		let
		    fun filter(i,c) = (case (c(body,helpers,false)) of
					   (MAYBE | MATCH _) => SOME(i,c)
					 | FAIL => NONE)
		    val constraints' = List.mapPartial filter (!constraints)
		    val _ = (constraints := constraints')
		    val result =
			(case constraints' of
			     [(i,c)] => (case (c(body,helpers,true)) of
					      FAIL => error "softeq succeeded but hardeq failed!!!"
					    | MAYBE => [(i,false)]
					    | MATCH thunk_arg => (thunk thunk_arg;
								  [(i,true)]))
			   | _ => map (fn (i,c) => (i,false)) constraints')
		in result
		end
	in
	    OCON{constrainer = constrainer,
		 name = name,
		 body = body}
	end

    fun ocon_constrain(OCON{constrainer,...},helpers) = constrainer helpers

  end
