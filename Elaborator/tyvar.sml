(*$import TYVAR Listops Name Util List Stats Int32 *)

(* Type variables parameterized over types *)
structure Tyvar :> TYVAR =
  struct

    open Listops Name Util

    val error = fn s => error "tyvar.sml" s
    val debug = Stats.ff("TyvarDebug")
    fun debugdo t = if (!debug) then (t(); ()) else ()

    val stamp = ref 0
    fun inc() = let val res = !stamp
		    val _ = stamp := (res + 1)
		in res
		end
    type stamp = int

    type ('ctxt,'1con) tyvar_info = {stampref : int,
				     name : var,
				     constrained : bool,
				     use_equal : bool,
				     body : '1con Util.oneshot,
				     ctxts : 'ctxt list} ref
    datatype ('ctxt,'1con) tyvar = TYVAR of ('ctxt,'1con) tyvar_info

    datatype 'a status = FAIL | MAYBE | MATCH of 'a
    type ('ctxt,'1con,'a) constraint = ('ctxt,'1con) tyvar * bool -> 'a status (* bool represents hardness *)
	
    datatype ('ctxt,'1con,'a) uocon  = UOCON of {constraints : ('ctxt,'1con,'a) constraint list}
    type ('ctxt,'1con) ocon_info = {constrainer : unit -> (int * bool) list,
				    name : var,
				    body : ('ctxt,'1con) tyvar}
    datatype ('ctxt,'1con) ocon = OCON of ('ctxt,'1con) ocon_info

    fun get_stamp() = inc()
    fun stamp2int stamp = (stamp : int)
    fun stamp_join(a : stamp,b) = if (a<b) then a else b
    fun tyvar_copy(TYVAR (ref {stampref, name, constrained, use_equal, body, ctxts})) =
	TYVAR(ref{stampref = stampref, name = name, constrained = constrained,
		  use_equal = use_equal, body = (case oneshot_deref body of
						     SOME _ => body
						   | _ => oneshot()), ctxts = ctxts})
    fun tyvar_update(TYVAR r, TYVAR(ref info)) = r := info
    fun fresh_stamped_tyvar (ctxts,name,stamp) = TYVAR(ref{stampref = stamp,
							   name = fresh_named_var name, 
							   constrained = false, 
							   use_equal = false,
							   body = oneshot(),
							   ctxts = [ctxts]})
    fun fresh_named_tyvar (ctxts,s) = fresh_stamped_tyvar(ctxts,s,inc())
    fun fresh_tyvar ctxts = fresh_named_tyvar (ctxts,"tv")
    fun tyvar_after stamp (TYVAR(ref {stampref,...})) = stamp < stampref
    fun tyvar_stamp (TYVAR(ref {stampref,...})) = stampref
    fun update_stamp (TYVAR(r as ref {stampref,name,constrained,use_equal,body,ctxts}),
		      stamp) = 
	let val stampref = Int.min(stampref,stamp)
	in  r := {stampref = stampref,
		  name=name,
		  constrained=constrained,
		  use_equal = use_equal,
		  body=body,
		  ctxts=ctxts}
	end

    fun eq_tyvar(TYVAR(ref{name=v1,...}),TYVAR(ref{name=v2,...})) = eq_var(v1,v2)
    fun tyvar_getvar(TYVAR(ref {name,...})) = name
    fun tyvar2string tv = var2string(tyvar_getvar tv)
    fun tyvar_deref(TYVAR(ref {body,...})) = oneshot_deref body
    fun tyvar_set(TYVAR(ref {body,...}),c) = 
		 (case (oneshot_deref body) of
		      NONE => oneshot_set(body,c)
		    |(SOME _)  => error "cannot set tyvar more than once")
    fun tyvar_isconstrained(TYVAR(ref {constrained,...})) = constrained 
    fun tyvar_constrain(TYVAR(r as ref ({stampref,name,constrained,use_equal,body,ctxts}))) =
	  r := {stampref = stampref,
		name=name,
		constrained=true,
		use_equal=use_equal,
		body=body,
		ctxts=ctxts}

    fun tyvar_is_use_equal(TYVAR(ref{use_equal,...})) = use_equal
    fun tyvar_use_equal(TYVAR(r as ref ({stampref,name,constrained,use_equal,body,ctxts}))) =
	  r := {stampref = stampref,
		  name=name,
		  constrained=constrained,
		  use_equal=true,
		  body=body,
		  ctxts=ctxts}
    fun tyvar_getctxts(TYVAR(ref {ctxts,...})) = ctxts
    fun tyvar_addctxts(TYVAR(r as ref ({stampref,name,constrained,use_equal,body,ctxts})), 
		       ctxts') =
	  r := {stampref = stampref,
		name=name,
		constrained=constrained,
		use_equal=use_equal,
		body=body,
		ctxts=(ctxts' @ ctxts)}


    fun fresh_uocon c = UOCON{constraints = c}
    fun ocon2string (OCON{name,...}) = var2string name
    fun ocon_deref (OCON{body,...}) = body

    fun uocon_inst (ctxts : 'ctxt,
		    UOCON{constraints} : ('ctxt,'1con,'a) uocon, thunk : 'a -> unit) : ('ctxt,'1con) ocon = 
	let
	    val body = fresh_named_tyvar (ctxts,"ocon")
	    val name = fresh_named_var "ocon"
	    val constraints = ref(mapcount (fn (i,c) => (i,c)) constraints)
	    fun constrainer () : (int * bool) list =
		let
		    fun filter(i,c) = (case (c(body,false)) of
					   MAYBE => SOME(i,c)
					 | MATCH _ => SOME(i,c)
					 | FAIL => NONE)
		    val constraints' = List.mapPartial filter (!constraints)
		    val _ = (constraints := constraints')
		    val result =
			(case constraints' of
			     [(i,c)] => (case (c(body,true)) of
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

    fun ocon_constrain(OCON{constrainer,...}) = constrainer() 

  end
