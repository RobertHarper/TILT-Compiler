(* XXXX thread-unsafe: new_stamp *)

(* Type variables parameterized over types *)
structure Tyvar :> TYVAR =
  struct

    open Listops Name Util

    val error = fn s => error "tyvar.sml" s
    val debug = Stats.ff("TyvarDebug")

    type stamp = int
    local
	val stamp = ref 0
    in
	fun new_stamp() = (stamp := (!stamp) + 1; !stamp)
    end

    (* Eq is NONE if we have not been marked for equality by the user or by type inference.
     * Eq is SOME full_oneshot if we have been marked for equality and set to a constructor c
     *   which admits equality.  (The oneshot is filled early by the unification routines.)
     * Eq is SOME empty_oneshot if we have been marked for equality.
     *)
    type ('ctxt,'con,'exp) tyvar_info = {stampref : int,
					 name : var,
					 constrained : bool,
					 eq : 'exp oneshot option,
					 body : 'con option ref,
					 ctxts : 'ctxt list} ref

    datatype ('ctxt,'con,'exp) tyvar = TYVAR of ('ctxt,'con,'exp) tyvar_info

    (* input bool represents hardness *)
    type ('ctxt,'con,'exp) constraint = (unit -> unit) * (('ctxt,'con,'exp) tyvar * bool -> bool)

    datatype ('ctxt,'con,'exp) ocon = OCON of {constraints : ('ctxt,'con,'exp) constraint list ref,
					       default : ('ctxt,'con,'exp) constraint,
					       name : var,
					       body : ('ctxt,'con,'exp) tyvar}

    fun stamp2int stamp = (stamp : int)
    fun stamp_join(a : stamp,b) = if (a<b) then a else b
    fun tyvar_copy(TYVAR (ref {stampref, name, constrained, eq, body, ctxts})) =
	TYVAR(ref{stampref = stampref, name = name, constrained = constrained,
		  eq = eq, body = (case !body of
				       SOME _ => body
				     | _ => ref NONE), ctxts = ctxts})
    fun tyvar_update(TYVAR r, TYVAR(ref info)) = r := info
    fun fresh_stamped_tyvar (ctxts,name,stamp) = TYVAR(ref{stampref = stamp,
							   name = fresh_named_var name,
							   constrained = false,
							   eq = NONE,
							   body = ref NONE,
							   ctxts = [ctxts]})
    val tyvar_counter = ref (Stats.counter "Elab-NumTyvars")
    fun fresh_named_tyvar (ctxts,s) = ((!tyvar_counter)(); fresh_stamped_tyvar(ctxts,s,new_stamp()))
    fun fresh_tyvar ctxts = fresh_named_tyvar (ctxts,"tv")
    fun tyvar_after stamp (TYVAR(ref {stampref,...})) = stamp < stampref
    fun tyvar_stamp (TYVAR(ref {stampref,...})) = stampref
    fun update_stamp (TYVAR(r as ref {stampref,name,constrained,eq,body,ctxts}),
		      stamp) =
	let val stampref = Int.min(stampref,stamp)
	in  r := {stampref = stampref,
		  name=name,
		  constrained=constrained,
		  eq = eq,
		  body=body,
		  ctxts=ctxts}
	end

    fun eq_tyvar(TYVAR(ref{name=v1,...}),TYVAR(ref{name=v2,...})) = eq_var(v1,v2)
    fun tyvar_getvar(TYVAR(ref {name,...})) = name
    fun tyvar2string tv = var2string(tyvar_getvar tv)
    fun tyvar_deref(TYVAR(ref {body,...})) = !body
    fun tyvar_set(TYVAR(ref {body,...}),c) =
	          (case !body of
		      NONE => body := (SOME c)
		    | SOME _  => error "cannot set tyvar more than once")
    fun tyvar_reset(TYVAR(ref {body,...}),c) = body := SOME c
    fun tyvar_isconstrained(TYVAR(ref {constrained,...})) = constrained
    fun tyvar_constrain(TYVAR(r as ref ({stampref,name,constrained,eq,body,ctxts}))) =
	  r := {stampref = stampref,
		name=name,
		constrained=true,
		eq=eq,
		body=body,
		ctxts=ctxts}

    fun tyvar_is_use_equal(TYVAR(ref{eq,...})) = isSome eq
    fun tyvar_use_equal(TYVAR(r as ref ({stampref,name,constrained,eq,body,ctxts}))) =
	(case eq
	   of NONE => r := {stampref = stampref,
			    name=name,
			    constrained=constrained,
			    eq=SOME (oneshot()),
			    body=body,
			    ctxts=ctxts}
	    | SOME _ => ())
    fun tyvar_eq_hole(TYVAR(ref{eq,...})) = eq
    fun tyvar_getctxts(TYVAR(ref {ctxts,...})) = ctxts
    fun tyvar_addctxts(TYVAR(r as ref ({stampref,name,constrained,eq,body,ctxts})),
		       ctxts') =
	  r := {stampref = stampref,
		name=name,
		constrained=constrained,
		eq=eq,
		body=body,
		ctxts=(ctxts' @ ctxts)}


    fun ocon2string (OCON{name,...}) = var2string name
    fun ocon_deref (OCON{body,...}) = body

    fun fresh_ocon (ctxt,constraints,default) =
	let val name = fresh_named_var "ocon"
	    val body = fresh_named_tyvar (ctxt,"ocon")
	    val _ = tyvar_constrain body
	in  OCON{constraints = ref constraints,
		 default = List.nth (constraints,default),
		 name = name,
		 body = body}
	end

    fun ocon_constrain (OCON{constraints, body, ...}) =
	let
	    fun filter(thunk,unify) = if (unify(body,false))
					  then SOME(thunk,unify)
				      else NONE
	    val constraints' = List.mapPartial filter (!constraints)
	    val _ = constraints := constraints'
	    val _ = (case constraints' of
			 [(thunk,unify)] => (unify(body,true); thunk())
		       | _ => ())
	in  length constraints'
	end

    fun ocon_constrain_default (ocon as OCON {constraints, default=default as (thunk,unify), body, ...}) =
	(case ocon_constrain ocon
	   of 0 => 0
	    | 1 => 1
	    | n => if unify(body,false)
		       then (constraints := [default]; unify(body,true); thunk(); 1)
		   else n)

  end
