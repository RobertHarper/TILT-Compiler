(*$import Prelude TopLevel Util Name Nil Prim TraceInfo Sequence List Array Int TilWord32 NilUtil Ppnil NilSubst Normalize TOCLOSURE Stats Listops Bool NilDefs NilRename NilContext *)

(* XXX Are argument traces being properly closure converted? *)

(* Closure conversion is accomplished in two phases.  
   The first phase scans the program for free type and term variables of 
     all type and term functions and also notes whether a function escapes or not.
     All this information is statefully accumulated in a table; the table is
     indexed by function names, so function variables must be globally unique.
   The second phase then rewrites the functions into codes and closures.  
     Thus, before closure conversion, there are Fixopen_b and App_e(Open,...).
     After closure conversion, there are Fixcode_b, Fixclosure_b, 
     App_e(Closure,...), and App_e(Code,...).
*)

(* The closure conversion performed here is essentially that described in the typed closure 
 conversion paper, but there are a couple of technical issues that affect the implementation.


 Issue: Non-Closed Code

 In principle, closure conversion is supposed to produce closed code for functions, so
 that the code can be hoisted to the top level.  We, however, do not hoist code after
 closure conversion, and the code we produce is not closed.  In particular, any 
 variables that stand for things that do not need to be present at run time are allowed
 to remain free in post-closure-conversion code.  Such variables include:
      - Constructor variables that only appear in classifiers, never in types-as-data.
      - Variables of coercion type that only appear in application position.
 (That's probably all.)

 It is important that the closure converter and reifier agree on what must be around at 
 run time.


 Issue: Shadowing

 Suppose FV(E) = {y}.  It would be nice if we could just write the code of
      fun f x = E
 as 
      fun fcode (x,env) = let y = #y(env) in E end
 Unfortunately, since we do not hoist code to the top level, this creates shadowing
 (as there obviously must already have been a binding for y).  To avoid this, the pass that
 scans for free variables also generates a fresh derived name for each one it sees.  
 Thus in the previous example, our table of function identifiers ends up looking like
   [ (f,[(y,y')]) ]  (where y' is a fresh name)
 and we write the code as
   fun fcode (x,env) = let y' = #y(env) in E[y'/y] end
   
 Note that in general, because functions can be nested, each variable needs a separate 
 derived name for every function in which it occurs free in order to preserve the
 no-shadowing invariant.


 Issue: Direct Calls

 It is sometimes not strictly necessary to create and use closures for a function.  This
 closure converter attempts to include some facilities for direct function calls, but
 does not use them very much.  The current strategy is to generate direct calls for self calls
 and indirect calls for all others.  So
 fun f x = g x
 and g x = g x
 translates to
 reccode f' (x,e) = e.g x
 andcode g' (x,e) = g' x
 recclos f = {f',{g}}
 andclos g = {g',{}}

 If we were using a more aggressive strategy for doing direct calls, the free-variable
 computation would have to pay attention.  In particular, if a function being called
 directly has any free variables, then the caller must pass the values of those free 
 variables to the function; thus they must be considered to occur free in the calling
 function as well.  To ensure that the final result of the free variable computation 
 has this "closedness" property, the initial pass records, for each function, all the
 other functions to which it makes direct calls.  There is code below for a second 
 mini-phase to propagate free variables from callees to callers until a fixed point is
 reached; the call to this phase is commented out, since it has no effect under our
 current strategy; it would be correct to leave it in, but a minor waste of time.

 To handle direct calls, the function application cases of both the free-variable pass
 and the rewriting pass recognize the special case where the function being called
 happens to be the current function.  Adding support for other direct calls would
 probably be a simple matter of adding more such special cases, although the rewriting 
 pass would have to do some extra work to assemble environments for the callee.



Some Factoids:

  - The closure converter DOES seem to support Typecase_c and Typecase_e; I do not
  know whether this support is correct or not.

  - At the moment, closure conversion introduces Typeof's.  Look in fun_rewrite.

  - Closure conversion DOES require correct trace annotations and constructor
  phase annotations.  Trace annotations are used to identify wide (i.e., float)
  values that must be boxed when building environments; constructor phase matters as
  described above ("Non-Closed Code").

  - Closure conversion DOES NOT SEEM TO require code in A-normal form; all it cares
  about is that functions have unique names; see above.


 joev, 8/2002
*)



structure ToClosure
    :> TOCLOSURE =
struct


    val do_single_venv = Stats.tt("Closure_TermCompress")
    val closure_print_free = Stats.ff "closure_print_free"
    (* If omit_coercions is true, coercion values that only appear in *)
    (* application position will be omitted from closures.  This will *)
    (* result in one more way that code might not be closed, but will *)
    (* save some space.                                               *)
    (* This flag is also consulted by the routine in nilutil that finds *)
    (* free variables in exception handlers.                            *)
    val omit_coercions = Stats.tt ("closure_omit_coercions")

    val typeof_count = Stats.counter "CC_typeofs"

    open Nil
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet

    val error = fn s => Util.error "toclosure.sml" s
    val debug = Stats.ff("ToclosureDebug")

    structure FidSet = VarSet

    structure FidMap = VarMap

    type fid = var

    fun chat s = if (!debug) then print s else ()

    (* -------------- types and values manipulating free variables ------------- *)
    (* lists are kept in reverse order; map allow fast lookup/membership  *)

    (*Data structure returned by the variable computation pass.  Contains the
     * set of free variables of the expression under consideration, with
     * each variable mapped to a fresh variable that will be used to name 
     * the corresponding projection from the environment in such a way as to maintain 
     * the no shadowing invariant
     *)
    type frees = {free_cvars : var VarMap.map,
		  free_evars : (var * niltrace (* con option*)) VarMap.map}

    val empty_frees = {free_cvars = VarMap.empty,
		       free_evars = VarMap.empty}

    (* varmapUnion : ('a VarMap.map * 'a VarMap.map) -> bool * ('a VarMap.map) *)
    (* (That's a manually inferred type.) *)
    (* PRE: M and N agree on the intersection of their domains. *)
    (* POST: varmapUnion (M,N) ==> (b,P), where                 *)
    (*       P = M union N                                      *)
    (*       b is true iff N is a subset of M                   *)
    (*                (equivalently, iff P = M)                 *)
    (*                                  - joev, 8/2002          *)
    fun varmapUnion(oldmap,newmap) = 
	let fun folder(var,item,acc as (changed,curmap)) =
	    case (VarMap.find(curmap,var)) of
		  SOME _ => acc
		| NONE => (false, VarMap.insert(curmap,var,item))
	in  VarMap.foldli folder (true,oldmap) newmap
	end

    (* join_free' : frees * frees -> bool * frees *)
    (* PRE: Maps in F agree with corresponding ones in G on the intersections of their domains. *)
    (* POST: join_free' (F,G) ==> (b,H), where                                                  *)
    (*       H = F union G, and                                                                 *)
    (*       b = true iff H = F, i.e. iff G is contained in F.                                  *)
    (* Redocumented by joev 8/2002.  Original comment follows.                                  *)
    (* returns a bool indicating whether the result is same as the first fv set;
      adds to the first free, in order, all the items only found in the second *)
    fun join_free'({free_evars = e1, free_cvars = c1} : frees,
		   {free_evars = e2, free_cvars = c2} : frees) = 
	let val (same_e,e3) = varmapUnion(e1,e2)
	    val (same_c,c3) = varmapUnion(c1,c2)
	in  (same_c andalso same_e, 
		{free_evars = e3,
		 free_cvars = c3})
	end

    (* join_free : frees * frees -> frees *)
    fun join_free args : frees = #2(join_free' args)


    fun show_free({free_evars, free_cvars} : frees) =
	(print "free_evars are: "; 
	 VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print "; ")) free_evars; print "\n";
	 print "free_cvars are: "; 
	 VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print "; ")) free_cvars; print "\n")


    (* c.l1.l2.l3 -> (c, [l1,l2,l3]) *)
    fun extract_cpath c = 
	let fun loop acc (Proj_c(c,l)) = loop (l::acc) c
	      | loop acc c = (c,acc)
	in loop [] c
	end


    (* -------- code to perform the initial free-variable computation ---------------- *)
    local
      
      (*Tags to indicate variable status.  A variable marked GLOBAL in the state 
       * is a global that does not need to be in the environment.
       * A variable marked LOCAL is bound in the current founction and so does not
       * need to be included in the environment.
       * A variable marked SHADOW is bound outside of the current function and is
       * not global, so it must be included in the environment.
       * Upon entering a new function, the state must be copied and all variables marked
       * LOCAL must be changed to SHADOW.
       * When we leave a function scope, we remove all variables from the free list
       * except those marked SHADOW.
       *)
      
	datatype expentry = GLOBALe 
	                  | SHADOWe of niltrace (** con option*) (* Available when of function type *)
			  | LOCALe  of niltrace (** con option*)
	datatype conentry = GLOBALc 
	                  | SHADOWc
			  | LOCALc

	datatype state = STATE of {curfid : fid * fid list,  (*The current function and the rest of 
							      * the functions in the current nest*)
				   is_top : bool,
				   boundevars : expentry VarMap.map,
				   boundcvars : conentry VarMap.map,
				                             (*What term and type variables are currently
							      * bound, and what is their status (see above)*)
				   boundfids : FidSet.set}   (*The set of type and term variables bound
							      * above which correspond to functions.
							      *)
	type funentry = {static : {fid : fid, 
				   fidtype_var : var,
				   code_var : fid,
				   unpack_var : fid,
				   cenv_var : var,
				   venv_var : var},
			 escape : bool,               (*Escape in the sense that we will generate a closure
						       * for it.  This may happen even for "non-escaping"
						       * functions in the current strategy, since we 
						       * only generate direct calls for self-calls
						       *)
			 callee : (fid * state) list,  (*Functions which are called directly.  Currently
							* will only ever contain self *)
			 frees : frees}


	val empty_table = VarMap.empty : (funentry ref) VarMap.map
	val fids = ref empty_table
	fun is_fid f = (case (VarMap.find(!fids,f)) of NONE => false | _ => true)


	fun get_static caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_static")
				   | SOME (ref{static,...}) => static)


	fun add_gboundevar (STATE{is_top,curfid,boundevars,boundcvars,boundfids},v,c) = 
	    let val boundevars' = VarMap.insert(boundevars,v,GLOBALe)
	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars',
		      boundcvars = boundcvars,
		      boundfids = boundfids}
	    end
	fun add_gboundcvar (STATE{is_top,curfid,boundevars,boundcvars,boundfids},v,k) = 
	    let val boundcvars' = VarMap.insert(boundcvars,v,GLOBALc)
	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars,
		      boundcvars = boundcvars',
		      boundfids = boundfids}
	    end

	fun add_boundevars' shadow (STATE{is_top,curfid,boundevars,boundcvars,boundfids},v_tr_list) = 
	    let fun folder((v,tr),m) = 
		let val entry = if shadow then SHADOWe tr
				else (if is_top then GLOBALe
				      else LOCALe tr)
		in  VarMap.insert(m,v,entry)
		end
		val boundevars' = foldl folder boundevars v_tr_list
	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars',
		      boundcvars = boundcvars,
		      boundfids = boundfids}
	    end
	val add_boundevars = add_boundevars' false

	fun add_boundcvars (STATE{is_top,curfid,boundevars,boundcvars,boundfids},vlist) =
	    let fun folder (v,boundcvars) =
		   let val entry = if is_top 
				     then GLOBALc
			           else LOCALc
		   in  VarMap.insert(boundcvars,v,entry)
		   end
		val boundcvars' =  foldl folder boundcvars vlist

	    in  STATE{is_top = is_top,
		      curfid = curfid,
		      boundevars = boundevars,
		      boundcvars = boundcvars',
		      boundfids = boundfids}
	    end


	fun add_boundfids shadow (STATE{is_top,curfid,boundevars,boundcvars,boundfids},fid_types) = 
	    let val state = STATE{curfid = curfid,
				  is_top = is_top,
				  boundevars = boundevars,
				  boundcvars = boundcvars,
				  boundfids = VarSet.addList(boundfids,map #1 fid_types)}
		fun mapper(fid,t) = 
		    let val {fidtype_var,...} = get_static fid
		    in  (fidtype_var, (fid, TraceKnown TraceInfo.Trace))
		    end
		val (vlist,fid_tr_types) = Listops.unzip(map mapper fid_types)
		val state = add_boundcvars (state,vlist)
	    in  add_boundevars' shadow (state,fid_tr_types)
	    end

	fun add_boundevar(s,v,tr) = add_boundevars(s,[(v,tr)])
	fun add_boundcvar(s,v) = add_boundcvars (s,[v])
	    
	(*Is it available?*)
	fun is_boundevar(STATE{boundevars,...},evar) = 
	    (case (VarMap.find(boundevars,evar)) of
		 SOME GLOBALe  => true
	       | SOME (LOCALe _) => true
	       | _ => false)

	fun is_boundcvar(STATE{boundcvars,...},cvar) = 
	    (case (VarMap.find(boundcvars,cvar)) of
		 SOME GLOBALc => true
	       | SOME LOCALc => true
	       | _ => false)

	fun is_boundfid(STATE{boundfids,...}, f) = VarSet.member(boundfids,f)

	(*First free occurrence*)
	(* (See the usage of these functions in e_find_fv' and c_find_fv', variable cases.  joev, 8/2002) *)
	fun cvar_isfree (STATE{boundcvars,boundevars,...},{free_cvars,...}:frees,cvar) = 
	    if isSome (VarMap.find(free_cvars,cvar)) then false
	    else (case (VarMap.find(boundcvars,cvar)) of
			  SOME GLOBALc => false
			| SOME LOCALc  => false
			| SOME SHADOWc => true
			| NONE => error ("cvar_isfree: variable " ^
					 (Name.var2string cvar) ^ " not bound"))


	fun evar_isfree (STATE{boundcvars,boundevars,...},{free_evars,...}:frees,evar) = 
	    if isSome(VarMap.find(free_evars,evar)) then NONE
	    else 
		(case (VarMap.find(boundevars,evar)) of
		     SOME GLOBALe => NONE
		   | SOME (LOCALe _) => NONE
		   | SOME (SHADOWe x(*(c,f)*)) => SOME x (*(c,f)*)
		   | NONE => error ("evar_isfree: variable " ^
				    (Name.var2string evar) ^ " not bound"))
			    


	fun free_cvar_add ({free_cvars,free_evars}:frees, cvar) =
	    let val free_cvars =
		    case VarMap.find(free_cvars,cvar) of
			SOME _ => error "free_cvar given variable already in free set"
		      | _ => VarMap.insert(free_cvars, cvar, Name.derived_var cvar)
            in   {free_evars = free_evars,
	          free_cvars = free_cvars}
	    end

	fun free_evar_add ({free_cvars,free_evars} : frees, evar, tr(*, c*)) =
	    let val free_evars =
		  (case (VarMap.find(free_evars,evar)) of
		       SOME _ => error "free_evar_add given variable already in free set"
		     | NONE => VarMap.insert(free_evars,evar,(Name.derived_var evar,tr(*,c*))))
	    in  {free_evars = free_evars,
		 free_cvars = free_cvars}
	    end


	fun empty_fun escape new_fid : funentry = 
	    let val name = Name.var2name new_fid
	    in   {static = {fid = new_fid,
		       fidtype_var = Name.fresh_named_var(name ^ "_type"),
		       code_var    = Name.fresh_named_var(name ^ "_code"),
		       unpack_var  = Name.fresh_named_var(name ^ "_unpack"),
		       cenv_var    = Name.fresh_named_var(name ^ "_cEnv"),
		       venv_var    = Name.fresh_named_var(name ^ "_eEnv")},
	          escape = escape,
	          callee = [],
	          frees = empty_frees}
	    end
	val global_escapes = ref (VarSet.empty : Name.VarSet.set)




    fun show_state(STATE{boundevars, boundcvars, ...} : state) =
	let fun show_eentry GLOBALe = print "GLOBALe"
	      | show_eentry (LOCALe _) = print "LOCALe"
	      | show_eentry (SHADOWe _) = print "SHADOWe"
	    fun show_centry GLOBALc = print "GLOABLc"
	      | show_centry LOCALc = print "LOCALc"
	      | show_centry SHADOWc = print "SHADOWc"
	in
	    (print "boundevars are: "; 
	     VarMap.appi (fn (v,e) => (print "  "; Ppnil.pp_var v; 
				       print " "; show_eentry e; print "\n")) boundevars; print "\n";
	     print "boundcvars are: "; 
	     VarMap.appi (fn (v,e) => (print "  "; Ppnil.pp_var v; 
				       print " "; show_centry e; print "\n")) boundcvars; print "\n")
	end


    (* copy_state : state * (var * var list) -> state *)
    (* Does the necessary bookkeeping to begin scanning a new function; takes the state from the *)
    (* point just before the function and the new value for cur_fid; turns all the LOCALs into   *)
    (* SHADOWs to reflect the fact that we're entering a new scope.  Comment by joev, 8/2002.    *)
	fun copy_state (STATE{boundevars,boundcvars,boundfids,curfid,...}) (fid : var * var list) = 
	    let fun epromote (LOCALe cf) = SHADOWe cf
		  | epromote x = x
		fun cpromote LOCALc = SHADOWc 
		  | cpromote x = x
		val boundevars' = VarMap.map epromote boundevars
		val boundcvars' = VarMap.map cpromote boundcvars
	    in  STATE{is_top = false,
		      curfid=fid,
		      boundfids=boundfids,
		      boundevars = boundevars',
		      boundcvars = boundcvars'}
	    end
	
	fun get_curfid(STATE{curfid,...}) = curfid

    (* remove_free : state * frees -> frees *)
    (* remove_free (s,f) ==> g, where       *)
    (* g is f restricted to those variables that are *not* "bound" in s; i.e., *)
    (* g is just those vars from f that are not "available" at the program point *)
    (* corresponding to s.                                                       *)
    (* (Is this name misleading?)        Commented by joev, 8/2002               *)
    fun remove_free(s as (STATE{boundevars,boundcvars,...}),
		    {free_cvars, free_evars}) : frees =
	let 
(*
	    val _ = (print "\n\nremove_free...\n";
		     print "\nfreeevars has ";
		     VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print ", ")) freeevars;
		     print "\nboundevars has ";
		     VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print ", ")) boundevars;
		     print "\n")
*)

	    fun cfilter (v,_) = not (is_boundcvar(s,v))
	    fun efilter (v,_) = not (is_boundevar(s,v))
	    val free_evars' = VarMap.filteri efilter free_evars
	    val free_cvars' = VarMap.filteri cfilter free_cvars
(*
	    val _ = (print "\nfreeevars' has ";
		     VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print ", ")) freeevars';
		     print "\n")
*)
	in  {free_cvars = free_cvars',
	     free_evars = free_evars'}
	end

    in  val get_static = get_static
	val is_fid = is_fid
	type state = state
	type funentry = funentry
	val copy_state = copy_state
	val show_state = show_state
	val free_cvar_add = free_cvar_add
	val free_evar_add = free_evar_add
	val cvar_isfree = cvar_isfree
	val evar_isfree = evar_isfree
	val is_boundfid = is_boundfid
	val add_gboundevar = add_gboundevar
	val add_gboundcvar = add_gboundcvar
	val add_boundevar = add_boundevar
	val add_boundcvar = add_boundcvar
	val add_boundevars = add_boundevars
	val add_boundcvars = add_boundcvars
	val add_boundfids = add_boundfids
	val get_curfid = get_curfid
	val remove_free = remove_free
	val is_boundevar = is_boundevar
	val is_boundcvar = is_boundcvar

    (* ----- Misc. imports ----- *)

	val insert_con = NilContext.insert_con
	val insert_kind = NilContext.insert_kind
	val insert_label = NilContext.insert_label

	val float64 = NilDefs.ftype64


    (* ----- Global data structure to keep track of function information ------------ *)

	fun reset_table escapes = (fids := empty_table;
				   global_escapes := VarSet.addList(VarSet.empty,escapes))
	fun get_fids() = VarMap.foldli (fn (fid,_,acc) => VarSet.add(acc,fid)) VarSet.empty (!fids)
	fun add_fun new_fid = 
	    (let val escape = VarSet.member(!global_escapes,new_fid)
	     in  fids := (VarMap.insert(!fids,new_fid,ref (empty_fun escape new_fid)))
	     end)
	    
	fun get_callee caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_callee")
				   | SOME (r as (ref{callee,...})) => callee)
	fun get_frees  caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_frees")
				   | SOME (r as (ref{frees,...})) => frees)

	fun get_escape caller = (case (VarMap.find(!fids,caller)) of
				     NONE => error ((Name.var2string caller) ^ "fid not found for get_escape")
				   | SOME (ref{escape,...}) => escape)


	fun initial_state topfid = let val _ = add_fun topfid
				   in   STATE{is_top = true,
					      curfid=(topfid,[topfid]),
					      boundfids=VarSet.empty,
					      boundevars = VarMap.empty,
					      boundcvars = VarMap.empty}
				   end

	fun add_escape esc_fid = 
	    (case (VarMap.find(!fids,esc_fid)) of
		 NONE => error ((Name.var2string esc_fid) ^ "fid not found for add_escape")
	       | SOME (r as (ref{static,escape,callee,frees})) =>
		     r := {static = static,
			   escape = true,
			   callee = callee,
			   frees = frees})
	fun add_callee (caller,callee_fid,state) = 
	    (case (VarMap.find(!fids,caller)) of
		 NONE => error ((Name.var2string caller) ^ "fid not found for add_callee")
	       | SOME (r as (ref{static,escape,callee,frees})) =>
		     r := {static = static,
			   escape = escape,
			   callee = (callee_fid,state)::callee,
			   frees = frees})
	fun add_frees (fid,f) =
	  let
(*	    val _ = (print "\naddfun variable ";Ppnil.pp_var fid;
		     print "with frees ";
		     show_free f;
		     print "\n")
	*)      
	  in
	    (case (VarMap.find(!fids,fid)) of
		 NONE => error ((Name.var2string fid) ^ "fid not found for add_frees")
	       | SOME (r as (ref{static,escape,callee,frees : frees})) =>
		     r := {static = static,
			   escape = escape,
			   callee = callee,
			   frees = join_free(frees,f) : frees})
	  end
	fun augment_frees (fid,f) =
	    (case (VarMap.find(!fids,fid)) of
		 NONE => error ((Name.var2string fid) ^ "fid not found for aug_frees")
	       | SOME (r as (ref{static,escape,callee,frees})) =>
		     let (* val _ = (print "augment_frees called on fid = ";
				  Ppnil.pp_var fid; print "\nwith\n";
				  print "evars are: "; 
				  VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print " ")) evars; print "\n";
				  print "cvars are: "; 
				  VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print " ")) cvars; 
				  print "\n")  *)
			 val (same,frees) = join_free'(frees,f)
		     in  if same 
			     then false
			 else 
			     (r := {static = static,
				    escape = escape,
				    callee = callee,
				    frees = frees}; true)
		     end)
    end




(* ----------------------------------------------------------------------------
   Free variable computation.
   ---------------------------------------------------------------------------- *)
    fun bnd_find_fv (bnd, (frees : frees, state : state)) : frees * state =
      (case bnd of
	 Con_b(Compiletime,cbnd) => let val (_, state) = cbnd_find_fv(cbnd,(frees,state))
				    in  (frees, state)
				    end
       | Con_b(Runtime,cbnd) => cbnd_find_fv(cbnd,(frees,state))
       | Exp_b(v,tr,e) => 
         let 
	   val f = trace_find_fv(state,frees) tr
	   val f = e_find_fv (state,f) e
	   val _ = if (!debug)
		     then (print "add_boundevar ";
			   Ppnil.pp_var v; print "\n")
		   else ()
	 in (f, add_boundevar(state,v,tr))
	 end
       | Fixopen_b var_fun_set =>
	 let (*val (outer_curfid,_) = get_curfid state  XXX Not used?  -Leaf *)
	   fun do_arm fids_types ((v,c),Function{tFormals,eFormals,fFormals,body,...}) =
	     let (*val outer_state = add_boundfids false (state,fids_types)   XXX Not used?  -Leaf*)
	       val fids = map #1 fids_types
	       val local_state = copy_state state (v,fids)
	       val local_state = add_boundfids true (local_state,fids_types)
	       val (f, s) = (empty_frees, local_state)
	       val _ = if (!debug)
			 then (print "state before vkfolder to ";
			       Ppnil.pp_var v; print "\n";
			       show_state s; print "\n")
		       else ()
	       val s = convlist_process (tFormals, s)
	       val _ = if (!debug)
			 then (print "the following (after vkfolder) frees to ";
			       Ppnil.pp_var v; print "\n";
			       show_free f; print "\n")
		       else ()
	       val (f,s) = vtrlist_find_fv (eFormals, (f, s))
		 
	       val _ = if (!debug)
			 then (print "the following (after vcfolder) frees to ";
			       Ppnil.pp_var v; print "\n";
			       show_free f; print "\n")
		       else ()
	       val f = e_find_fv (s,f) body
	       val _ = if (!debug)
			 then (print "the following (after body) frees to ";
			       Ppnil.pp_var v; print "\n";
			       show_free f; print "\n")
		       else ()
			 
	       fun efolder(v,(_,tr(*,copt*)),f) = free_evar_add(f,v,tr)
		 (*let 
		   val self = Listops.member_eq(Name.eq_var,v,
						#2(get_curfid s))
		 in  (case (self,copt) of
			(false, _) => free_evar_add(f,v,tr,copt)
		      | (true, SOME _) => free_evar_add(f,v,tr,copt)
		      | _ => error "no optional type for function")
		 end*)
	       fun cfolder(v,_,f) = free_cvar_add(f,v)

	       (* Generate a new copy of our free variables, with fresh derived names, *)
	       (* to store as the free vars of this function.  The f we just computed will *)
	       (* be returned, and counted among the free vars of the next level out. joev *)
	       val {free_evars,free_cvars} = f
	       val lf = VarMap.foldli efolder empty_frees free_evars
	       val lf = VarMap.foldli cfolder lf free_cvars	 
               (* The following is mysterious, and seems unnecessary.
		  Perhaps now that I have commented it out, someone will discover
		  why it was here.                joev
	       val f = {free_evars = free_evars,
			free_cvars = #free_cvars lf}  (* XXX same as free_cvars?  This whole bit there is just checking invariant?  -Leaf*)
	                                              (* NO!  Creates new derived vars! *)
               *)
	       (* Record the free variables of the function we've just processed. *)
	       val _ = add_frees(v,lf)
	     in  f
	     end
	   
	   val var_arm_list = Sequence.toList var_fun_set
	   val local_fids_types = map #1 var_arm_list
	   val fids = map #1 local_fids_types
	   val _ = app add_fun fids
	   val free = foldl (fn (a,f) => let val f' = do_arm local_fids_types a
					 in  join_free(f,f')
					 end) frees var_arm_list
	   val (s,f) = (add_boundfids false (state, local_fids_types), free) 
	 in  (remove_free(s,f), s)
	 end
       | Fixcode_b _ => error "there can't be codes while closure-converting"
       | Fixclosure_b _ => error "there can't be closures while closure-converting")

	

    and trace_find_fv (state : state, frees : frees) trace : frees =
	(case trace of
	     TraceUnknown => frees
	   | TraceKnown (TraceInfo.Compute(v,_)) => c_find_fv (state, frees) (Var_c v)
	   | TraceKnown _ => frees
	   | TraceCompute v => c_find_fv (state, frees) (Var_c v))

    and e_find_fv arg exp : frees =
	let val res = e_find_fv' arg exp
	    handle e => (print "exp_find_fv called on\n";
			 Ppnil.pp_exp exp;
			 print "\n\n"; raise e)
	in  res 
	end

    and switch_find_fv (state : state, frees : frees) switch : frees =
	(case switch of
	     Intsw_e{arg,arms,default,result_type,...} =>
		 let val frees = e_find_fv (state,frees) arg
		     val frees = t_find_fv (state,frees) result_type
		     val frees = foldl (fn ((_,e),f) => e_find_fv (state,f) e) frees arms 
		     val frees = (case default of
				      NONE => frees
				    | SOME e => e_find_fv (state,frees) e)
		 in  frees
		 end
	   | Sumsw_e{sumtype,arg,bound,arms,default,result_type,...} => 
		 let val frees = e_find_fv (state,frees) arg
		     val frees = t_find_fv (state,frees) sumtype
		     val frees = t_find_fv (state,frees) result_type
		     val frees = (case default of
				      NONE => frees
				    | SOME e => e_find_fv (state,frees) e)
		     fun folder((w,tr,e),f) =
			let val state = add_boundevar(state,bound,tr)    (*XXX Don't need to traverse tr as well?  -Leaf *)
			                                                      (*Invariant: This is always Trace *)
			in  e_find_fv (state,f) e
			end
		     val frees = foldl folder frees arms
		 in  frees
		 end
	   | Exncase_e{arg,bound,arms,default,result_type,...} => 
		 let val frees = e_find_fv (state,frees) arg
		     val frees = t_find_fv (state,frees) result_type
		     val frees = (case default of
				      NONE => frees
				    | SOME e => e_find_fv (state,frees) e)
		     val frees = foldl (fn ((e1,tr,e2),f) => 
					let val f = e_find_fv (state,f) e1
					    val f = trace_find_fv(state,f) tr
					    val state = add_boundevar(state,bound,tr)
					    val f = e_find_fv (state,f) e2
					in  f
					end) frees arms 
		 in  frees
		 end
	   | Typecase_e {arg,arms,default,result_type} =>
		 let val frees = t_find_fv(state,frees) result_type
		     val frees = c_find_fv(state,frees) arg
		     val frees = e_find_fv(state,frees) default
		     fun folder ((pc,vklist,e),frees) = 
			 let val (frees,state) = vklist_find_fv (vklist, (frees,state))
			 in  e_find_fv (state,frees) e
			 end
		 in  foldl folder frees arms
		 end)

    and e_find_fv' (state : state, frees : frees) exp : frees =
	(if (!debug)
	     then (print "exp_find_fv called on\n";
		   Ppnil.pp_exp exp; print "\n\n")
	 else ();
	 case exp of
	     Var_e v => 
	       (* Because of the special case handling for App_e, we know this variable is not a function *)
	       (* being called directly.  Thus, if it is a function, then it is escaping in the sense that *)
	       (* we need a closure for it.             Comment by joev, 8/2002                            *)
		 (if (is_boundfid(state,v)) then add_escape v else ();
		  (case (evar_isfree(state,frees,v)) of
		         (* If we get NONE here, then either v is "available" or we already knew it occurred free. *)
			   NONE => frees
			 (* If we get SOME, then we have to record the free occurrence; when we rewrite this, we    *)
			 (* will also introduce an occurrence of the trace annotation of v, so check its FV's also. *)
			 (*        -joev                                                                            *)
			 | SOME tr(*,copt)*) => let val frees = free_evar_add(frees,v,tr(*,copt*))
					     in  trace_find_fv(state, frees) tr
					     end))
	   | Prim_e (p,trlist,clist,elist) =>
		 let fun tfold(t,f) = t_find_fv (state,f) t
		     fun cfold(c,f) = c_find_fv (state,f) c
		     fun efold(e,f) = e_find_fv (state,f) e
		     fun trfold(tr,f) = trace_find_fv(state,f) tr

		     val frees = if (NilDefs.allprim_uses_carg p)
				     then foldl cfold frees clist
				 else foldl tfold frees clist
		     val frees = foldl efold frees elist
		     val frees = foldl trfold frees trlist
		 in  frees
		 end
	   | Const_e v => 
		 (case v of
		      (Prim.int _)  => frees
		    | (Prim.uint _) => frees
		    | (Prim.float _) => frees
		    | Prim.array (c,array) =>
			  let fun folder(e,f) = e_find_fv (state,f) e
			      val f = Array.foldl folder frees array
			  in  t_find_fv (state,f) c
			  end
		    | Prim.vector (c,array) => 
			  let fun folder(e,f) = e_find_fv (state,f) e
			      val f = Array.foldl folder frees array
			  in  t_find_fv (state,f) c
			  end
		    | Prim.refcell (ref e) => e_find_fv(state,frees) e
		    | Prim.tag (_,c) => t_find_fv (state,frees) c)
	   | Let_e (_,bnds,e) => (* treat as though a sequential let *)
		      let val (bndfree,state') = foldl bnd_find_fv (frees,state) bnds
		      in  e_find_fv (state',bndfree) e
		      end
	   | Switch_e switch => switch_find_fv (state,frees) switch

	   | App_e (_, e, clist, elist, eflist) =>
		 let val f = (case e of
				      Var_e v => if (is_fid v andalso 
						     Name.eq_var(v,#1(get_curfid state)))  (* XXX Why only if current function?  -Leaf*)
                                                                         (* Because we are only tracking callees of direct calls, which only 
                                                                            happen for self-calls?  -joe, 8/2002 *)
						     then (
(*							   print "***** adding callee ";
							   Ppnil.pp_var v; print " to ";
							   Ppnil.pp_var (get_curfid state); print "\n";
*)
							   add_callee(#1(get_curfid state),v,
								      state);
							   frees)
						 else (e_find_fv (state,frees) e)
				    | _ => (e_find_fv (state,frees) e))
		     val f = foldl (fn (c,f) => c_find_fv (state,f) c) f clist
		     val f = foldl (fn (e,f) => e_find_fv (state,f) e) f elist
		     val f = foldl (fn (e,f) => e_find_fv (state,f) e) f eflist
		 in  f
		 end
	   | ExternApp_e (e, elist) =>
		 let val f = e_find_fv (state,frees) e
		     val f = foldl (fn (e,f) => e_find_fv (state,f) e) f elist
		 in  f
		 end
	   | Raise_e (e,c) => e_find_fv (state, t_find_fv (state,frees) c) e
	   | Handle_e {body,bound,handler,result_type} =>
		 let val f = e_find_fv (state,frees) body
		     val f = e_find_fv (add_boundevar(state,bound,TraceKnown TraceInfo.Trace), f) handler
		     val f = t_find_fv (state,f) result_type
		 in  f
		 end
	   | Fold_e (vars,from,to) =>
	     let
		 val state = foldl (fn (v,s) => add_boundcvar (s,v)) state vars
		 val f = t_find_fv (state,frees) from
		 val f = t_find_fv (state,f) to
	     in f
	     end
	   | Unfold_e (vars,from,to) =>
	     let 
		 val state = foldl (fn (v,s) => add_boundcvar (s,v)) state vars
		 val f = t_find_fv (state,frees) from
		 val f = t_find_fv (state,f) to
	     in f
	     end
	   | Coerce_e (coercion,cargs,exp) =>
	     let
		 val f = 
		   if !omit_coercions then
		     case coercion of 
		       (Var_e v) => frees 
		     | _ => e_find_fv (state,frees) coercion
		   else
		     e_find_fv (state,frees) coercion
		 (* The cargs are ignored -- they have to be, since the reifier *)
		 (* may have already decided not to make their free variables   *)
		 (* available.                                                  *)
		 val f = e_find_fv (state,f) exp
	     in f
	     end)



	and c_find_fv (state : state, frees : frees) con : frees =
	    let 
		val _ = if (!debug)
			    then (print "c_find_fv called on\n";
				  Ppnil.pp_con con; print "\n\n")
			else ()
		val res = c_find_fv' (state,frees) con 
		handle e => 
		  (print "c_find_fv called on\n";
		   Ppnil.pp_con con; print "\n and with state = ";
		   show_state state; print "\n\n"; raise e)
	    in res
	    end	 

	and t_find_fv (state : state, frees : frees) con : frees = frees


	and k_find_fv (state : state,frees : frees) kind : frees =
	    let val res = k_find_fv' (state,frees) kind 
		handle e => 
		  (print "k_find_fv called on\n";
		   Ppnil.pp_kind kind; print "\n and with state = ";
		   show_state state; print "\n\n"; raise e)
	    in res
	    end

    (* Used for processing constructor variable parameter lists for functions *)

    and convlist_process (convlist, state) : state =
	let fun folder(v,s) = 
	    add_boundcvar (s, v)
	in  foldl folder state convlist
	end

    (* The following three functions ([vk|vtrt|vopttrt]list_find_fv) are used for *)
    (* processing argument lists, so they add the variables they see to the state as *)
    (* LOCAL.(or possibly GLOBAL if appropriate?) as well as adding anything that    *)
    (* occurs free to fs.     joev, 8/2002.                                          *)

    and vklist_find_fv (vklist,fs) =
	let fun folder((v,k),(f,s)) = 
	    (k_find_fv (s,f) k, add_boundcvar(s,v))
	in  foldl folder fs vklist
	end

    and vtrtlist_find_fv (vtrclist,fs) =
	let fun folder((v,tr,c),(f,s)) = 
	    (trace_find_fv (s, t_find_fv (s,f) c) tr,
	     add_boundevar(s,v,tr))
	in  foldl folder fs vtrclist
	end

    and vtrlist_find_fv (vtrlist,fs) =
	let fun folder((v,tr),(f,s)) = 
	    (trace_find_fv (s, f) tr,
	     add_boundevar(s,v,tr))
	in  foldl folder fs vtrlist
	end

    and vopttrtlist_find_fv (vclist,fs) =
	let fun folder((vopt,tr,c),(f,s)) = 
	    (t_find_fv (s,f) c, 
	     (case vopt of 
		  NONE => s
		| SOME v => add_boundevar(s,v,tr)))
	in  foldl folder fs vclist
	end

    and cbnd_find_fv (cbnd,(f,s)) : frees * state =
	(case cbnd of
	     Con_cb(v,c) => let val f' = c_find_fv (s,f) c
				val state = add_boundcvar(s,v)
			    in (f',state)
			    end
	   | (Code_cb(v,vklist,c)) => error "Code function in supposedly unclosed code" (* (f,s) *)
	   | (Open_cb(v,vklist,c)) =>
			    let val _ = add_fun v
				val ls = copy_state s (v,[v])
				val (f,ls) = vklist_find_fv (vklist,(f,ls))
				val f' = c_find_fv (ls,f) c
				val _ = add_frees(v,f')
			    in  (remove_free(s,f'), add_boundcvar(s,v))
			    end)
			
    and c_find_fv' (state : state, frees : frees) con : frees =
	     (case con of
		 Prim_c (pc,clist) => foldl (fn (c,f)=> (c_find_fv (state,f) c)) frees clist
	       | Mu_c (_,vcset) =>
		     let val vclist = Sequence.toList vcset
			 (* we need to alpha-vary since reductions may lead to duplication
			    of bound variables despite A-normal linearization *)
			 val (vclist) = NilUtil.alpha_mu (fn v => is_boundcvar(state,v)) (vclist)
			 val state' = add_boundcvars(state,map #1 vclist)
		     in  foldl (fn ((v,c),f) => c_find_fv (state',f) c) frees (Sequence.toList vcset)
		     end
	       (* the types of some primitives like integer equality are Code arrow *)
               (*  (So?  -joev, 8/2002)  *)
	       | AllArrow_c{tFormals,eFormals,fFormals,body_type,...} =>
		     let val (f,s) = vklist_find_fv (tFormals,(frees,state))
			 val f = foldl (fn (c, f) => c_find_fv (s,f) c) f eFormals
		     in  c_find_fv (s,f) body_type
		     end
	       | ExternArrow_c(clist,c) =>
		     let fun cfolder (c,(f,s)) = (c_find_fv (s,f) c,s)
			 val (f,s) = foldl cfolder (frees,state) clist
		     in  c_find_fv (s,f) c
		     end
	       | Var_c v => 
		     if (cvar_isfree(state,frees,v))
			then free_cvar_add(frees,v)
		     else frees
	       | Coercion_c {vars,from,to} =>
		       let 
			 val state = foldl (fn (v,s) => add_boundcvar(s,v)) state vars
			 val f = c_find_fv (state,frees) from
		       in c_find_fv (state,f) to
		       end
	       | Let_c(_,cbnds,c) => 
		     let 
			 val (f,s) = foldl cbnd_find_fv (frees,state) cbnds
			 val f' = c_find_fv (s,f) c
		     in  f'
		     end
	       | Crecord_c lclist => foldl (fn ((_,c),f) => c_find_fv (state,f) c) frees lclist
	       | Proj_c (c,l) => c_find_fv (state,frees) c
	       | Closure_c (c1,c2) => c_find_fv (state, c_find_fv (state,frees) c1) c2
	       | App_c(c,clist) => let val f = c_find_fv (state,frees) c
				   in foldl (fn (c,f) => c_find_fv (state,f) c) f clist
				   end)
		 
	and k_find_fv' (state,frees) kind : frees =
	    (case kind of
	    Type_k => frees
	  | SingleType_k c => (c_find_fv (state,frees) c)
	  | Single_k c => (c_find_fv (state,frees) c)
	  | (Record_k lv_kind_seq) =>
		 let fun folder (((l,v),k),(f,s)) = (k_find_fv (s,f) k, add_boundcvar(s,v))
		 in  #1(Sequence.foldl folder (frees,state) lv_kind_seq)
		 end
	  | (Arrow_k (_,vklist,k)) =>
		let val (f,s) = vklist_find_fv (vklist,(frees,state))
		in  k_find_fv (s,f) k
		end)



   (* ------- compute the final free-variable list by closing over the callgraph ------ *)

    (* I have commented out the call to this phase from close_mod (at the bottom of this file) *)
    (* because it has no effect under our current strategy; however, since we might want to play *)
    (* around with this later I'm leaving the code here.              joev, 8/2002               *)

   local
     (* close_fun : fid * (fid VarSet.set) -> fid VarSet.set *)
     (* (That's a manually inferred type.)                   *)
     (* For each function fid directly called by function curfid, statefully adds the free variables *)
     (* of fid to those of curfid.  If this actually changes the free variables of curfid, returns   *)
     (* (nextset union {curfid}); else, returns nextset unchanged.         joev                      *)
       fun close_fun (curfid,nextset) = 
	   let 
	     val callees = get_callee curfid
	     val callee_fvs = foldl (fn ((fid,s),fv) => let val f = get_frees fid
							    val f = remove_free(s,f)
							in  join_free(f, fv)
							end) empty_frees callees 
	       val changed = augment_frees(curfid,callee_fvs)
	   in if changed
		  then VarSet.add(nextset,curfid)
	      else nextset
	   end
   in  fun close_funs workset = 
       let  val _ = if (!debug)
			then (print "before close_funs\n";
			      VarSet.app (fn fid => (print ("fid = ");
						     Ppnil.pp_var fid; print " --   callees are "; 
						     app (fn (fid,s) => (print "  ";
									 Ppnil.pp_var fid; print " -> ";
									 show_state s;
									 print "\n"))
						     (get_callee fid);
						     print "\n";
						     show_free(get_frees fid); print "\n\n")) (get_fids());
			      print "\n\n")
		    else ()
	    fun loop() = 
		let 
		  (* Propagates free variables one step from callees to callers, returning a set of *)
		  (* all the fids whose free variables have changed.  (When this set is empty, we have *)
                  (* finished the transitive closure.)               joev, 8/2002                      *)
		    val nextset = (VarSet.foldl close_fun VarSet.empty workset)
		in  if (VarSet.isEmpty nextset)
			then ()
		    else loop()  (* note that we must start with the whole set again *)
		end
	    val _ = loop()

            (* The rest of close_funs just prints debugging info. *)
	    val _ = if (!debug)
			then (print "after all close_funs\n";
			      VarSet.app (fn fid => (print ("fid = ");
						     Ppnil.pp_var fid; print "\n"; 
						     show_free(get_frees fid))) (get_fids());
			      print "\n\n")
		    else ()
	    val _ = if !closure_print_free 
			then VarSet.app 
			    (fn fid => 
			     let val {free_evars,free_cvars} = get_frees fid
			     in  (print ("fid = ");
				  Ppnil.pp_var fid; print "  (#free-cvars , #free-evars) =   ";
				  print (Int.toString (VarMap.numItems free_cvars));
				  print ", ";
				  print (Int.toString (VarMap.numItems free_evars));
				  print "\n")
			     end) (get_fids())
		    else ()
       in  ()
       end
   
   end



   (* The comment below is out of date and wrong.  We do not seem to do unpack functions anymore. *)
   (* joev, 8/2002.                                                                               *)

   (* ------- rewrite each term-level binding to a list of bindings ---------------------
      In general, each function is re-written to one or more functions
      and zero or more closures.  No closures are needed and generated
      if the function does not escape.

      ----- Function bindings are rewritten as follows.
      
      f == Function(vargs, fargs) = body --->
      
      [fcode == Pfunction(cargs @ cfv, vargs @ vfv, fargs) = body
       funpack == Pfunction(cargs @ [cenv], vargs @ [venv], fargs) = body
       f == Closure(funpack,cenv,venv)]

       ------------------------------------------------------------------------ *)

   fun cproj cvar i = Proj_c(Var_c cvar, NilUtil.generate_tuple_label(i+1))
   fun eproj (evar,rectype) i = Prim_e(NilPrimOp(select (NilUtil.generate_tuple_label(i+1))),[],
				       [],[Var_e evar])


   local 
       datatype state = STATE of {curfid : var list}
   in
       fun copy_state (STATE{curfid},fid) = STATE{curfid = fid::curfid}
       fun show_state(STATE{curfid}) = (print "state = "; Ppnil.pp_var (hd curfid); print "\n")
       fun new_state curfid = STATE{curfid = [curfid]}
       fun current_fid (STATE{curfid}) = hd curfid
       fun current_fids (STATE{curfid}) = curfid
       type state = state
   end

   (* If there is one free variable, then don't bother tupling it up.
      Notice that we must do this consistently at the definition and call sites. 
      If there are no free variables, use empty record as the free environment.
      In either case, we get to not have the unpack function. 

      We perform this optimization only if both the constructor AND term level
      free variables are zero or one in number.
    *)

   (* fun_rewrite *)
   (* vars is the list of names of all the functions defined in the same recursive nest as the current one. *)
   (* Comments by joev, 8/2002. *)
   fun fun_rewrite D state vars ((v,c),Function{effect,recursive,
						tFormals=tvars,eFormals=evars,fFormals=fvars,body}) =
       let 
	   val _ = if (!debug)
		       then (print "\n***fun_rewrite v = "; Ppnil.pp_var v; print "\n")
		   else ()

	   (*We maintain the old context*)
	   val arr = Normalize.strip_arrow_norm D c
	      
	   val {openness,tFormals,eFormals=etypes,body_type,fFormals=numf,...} = NilUtil.rename_arrow (arr, tvars)

	   val eFormals = Listops.zip evars etypes
	   val fFormals = map (fn v => (v,float64)) fvars
	   val D = foldl (fn ((v,k),D) => insert_kind(D,v, k)) D tFormals
	   val D = foldl (fn (((v,_),c),D) => insert_con(D,v, c)) D eFormals

	   val D = foldl (fn ((v,c),D) => insert_con(D,v,c)) D fFormals

	   val {code_var, fidtype_var, unpack_var, cenv_var, venv_var, ...} = get_static v
	   val {free_cvars,free_evars=pc_free} = get_frees v

	   (* vkl_free is a list of tuples (v,v',k,l), where: *)
	   (*      v is a constructor variable that appears free in the function (?) *)
	   (*      v' is a fresh variable that will refer to the projection from the constructor env *)
	   (*           containing v (?)                                                       *)
	   (*      k is a kind, namely the singleton of v.                                           *)
	   (*      l is the label of the component of the constructor environment holding v. (?)     *)
	   val (vkl_free, D) = VarMap.foldli (fn (v,v',(acc,D)) => let val k = Single_k(Var_c v)
					   val l = Name.internal_label(Name.var2string v)
					   val D = NilContext.insert_kind (D, v', k)
				         in  ((v,v',k,l)::acc, D)
				         end) ([],D) free_cvars

	   (* Rewrite the kinds in vkl_free.  (The variables in the singletons will probably become projections.) *)
	   val vkl_free = map (fn (v,v',k,l) => (v,v',k_rewrite state k,l)) vkl_free

	   (* After this, pc_free will be a list of tuples of the form (v,v',tr,l,c), where *)
	   (*      v is a value variable (?)                                                *)
	   (*      v' is the variable that will refer to the projection from the environment corrsp. to v. (?) *)
	   (*      tr is the trace annotation for v' (?)                                                       *)
	   (*      l is the label of the component of the constructor environment holding v. (?)               *)
	   (*      c is the type of v', which currently is just Typeof(v).  Presumably this is about to change (?) *)
	   val (pc_free, D) = let val temp = VarMap.listItemsi pc_free
				  fun folder((v,(v',tr(*,copt*))), D) = 
				      let val l = Name.internal_label(Name.var2string v)
					  val c = NilContext.find_con (D, v)
					  val D = NilContext.insert_con (D, v', c)
				      in ((v, v', tr, l, c), D)
				      end
			      in  Listops.foldl_acc folder D temp
			      end

	   val free_vars = map #1 pc_free

	   (* is_recur will be true iff the name of one of the other functions in the next occurs inside the current one. *)
	   val is_recur = not(null (Listops.list_inter_eq(Name.eq_var, vars, free_vars)))

	   val _ = if (!debug)
		       then (print "fun_rewrite v = "; Ppnil.pp_var v;
			     print "\nvkl_free are "; 
			     app (fn (v,_,_,_) => (Ppnil.pp_var v; print ", ")) vkl_free;
			     print "\nvc_free are "; 
			     app (fn (v,_,_,_,_) => (Ppnil.pp_var v; print ", ")) pc_free;
			     print "\n")
		   else ()

	   val num_vkl_free = length vkl_free
	   val num_pc_free = length pc_free
	   val is_empty = num_pc_free = 0

	   (* A list of bindings projecting constructor variables from the environment. *)
	   val code_cbnds = map (fn (_,v',_,l) => Con_b(Runtime, (Con_cb(v',Proj_c(Var_c cenv_var, l))))) vkl_free

	   (* The constructor environment itself *)
	   val cenv = Crecord_c(map (fn (v,_,_,l) => (l,c_rewrite state (Var_c v))) vkl_free)

	   (* A substitution to replace constructor variables with the appropriate projections. *)
	   (* (Is the name "internal" strange? -joev)                                           *)
	   val internal_subst = foldl (fn ((v,_,_,l),s) => 
				       NilSubst.C.sim_add s (v,Proj_c(Var_c cenv_var, l)))
	                         (NilSubst.C.empty()) vkl_free


	   (* Now we can put together the type of the closure.*)
	   val (vklist_cl, clist_cl, codebody_tipe, closure_tipe) =
	       (*if doConTrans then
		   let
		       val vklist_cl = vklist_rewrite state tFormals
		       val clist_cl = map (c_rewrite state) etypes
		       val codebody_tipe = c_rewrite state body_type
		       val closure_tipe = AllArrow_c{openness=Closure, effect=effect,
						     tFormals=vklist_cl,
						     eFormals=clist_cl,
						     fFormals=numf,
						     body_type=codebody_tipe}
		   in
		       (vklist_cl, clist_cl, codebody_tipe, closure_tipe)
		   end
	       else*)
		   (tFormals, etypes, body_type, c)

	   (* We will reuse parts of this type below, so we must rename 
	    * the bound variables if we wish to maintain the renaming
	    * invariant
	    *)
	   val closure_tipe = NilRename.renameCon closure_tipe

	   (* Should codebody_tipe be called something else now?  -joev *)
	   val codebody_tipe = NilSubst.substConInCon internal_subst codebody_tipe


	   val inner_state = copy_state(state,v)

           fun is_float (TraceKnown TraceInfo.Notrace_Real) = true
	     | is_float _ = false
	   fun box e = Prim_e(NilPrimOp(box_float Prim.F64), [],[], [e])
	   fun unbox e = Prim_e(NilPrimOp(unbox_float Prim.F64), [],[], [e])
	   val trace_pointer = TraceKnown TraceInfo.Trace
	   val trace_float = TraceKnown TraceInfo.Notrace_Real

	   (* Here we create the value environment. *)
	   (*      venv is the expression to create the environment. *)
	   (*      code_bnds go at the beginning of the code to extract the environment's contents. *)
	   (*      venv_tr is the environment's trace annotation.    *)
	   (*      venv_type_var is a variable with which to name the environment's type. *)
	   (*      venv_type is the the type of the environment.              *)
	   val (venv, code_bnds, venv_tr,venv_type_var,venv_type) = 
	      (case (!do_single_venv, pc_free) of
		 (* The special case where we are flattening an environment that has just one thing. *)
		(true, [(v,v',tr,_,t)]) => let (* Here the environment is just the one variable...   *)
					       val venv = e_rewrite D state (Var_e v)
					       (* ...unless it's a float and must be boxed. *)
					       val (venv,bnd,tr,t) = 
						 if is_float tr then
						   (box venv,
						    Exp_b(v', tr, unbox(Var_e venv_var)),
						    trace_pointer,NilDefs.boxfloat_con)
						 else
						   (venv,
						    Exp_b(v',tr,Var_e venv_var),
						    tr,t)
					       val t_name = Name.fresh_named_var "singlevenvtype"
					   in  (venv, [bnd], tr, t_name,t)
					   end
	      | _ => let  val labels = map #4 pc_free
			  (* mapper is called on each free variable, and returns a tuple (field,bndopt,tr,typ,codebnds), where: *)
			  (*      field is the value that will go into the environment *)
			  (*      bndopt is (optionally) a bnd that must happen before the environment is created *)
			  (*      tr is the trace annotation of the projected value (inside the code)             *)
			  (*      typ is the type of field.                                                       *)
			  (*      codebnds are bnds that happen at the beginning of the function, to extract the value. *)
			  fun mapper (v,v',tr,l,t) =
			      let val env_e = e_rewrite D state (Var_e v)
				  val code_e = Prim_e(NilPrimOp(select l), [],[],
						 [Var_e venv_var])
				  val code_tr = trace_rewrite inner_state tr
				  val (env_e,env_tr,env_t) =
				      if (is_float tr) 
					  then (box env_e, trace_pointer, NilDefs.boxfloat_con)
				      else (env_e, trace_rewrite state tr, t)
				  val code_bnds = if (is_float tr)
						      then let val v'' = Name.derived_var v'
							   in  [Exp_b(v'',trace_pointer,code_e),
								Exp_b(v',trace_float,unbox (Var_e v''))]
							   end
						  else [Exp_b(v',code_tr,code_e)]
			      in  (case env_e of 
				       Var_e _ => (env_e, NONE, env_tr, env_t, code_bnds)
				     | _ => let val v'' = Name.derived_var v
					    in  (Var_e v'', SOME(Exp_b(v'',env_tr,env_e)), env_tr, env_t, code_bnds)
					    end)
			      end
			  val fields_bndopts_trs_types_codebnds = map mapper pc_free
			  val (fields,bndopts,trs,types,codebnds) = Listops.unzip5 fields_bndopts_trs_types_codebnds
			  val bnds = List.mapPartial (fn x => x) bndopts
			  val code_bnds = Listops.flatten codebnds
			  val venv_type = Prim_c(Record_c labels, types)
			  val venv_type_name = Name.fresh_named_var "venvtype"
			  val venv =  
			    (case labels
			       of [] => Prim_e(NilPrimOp(record []),[],[], [])
				| _  => 
				 let
				   val venv_tag_name = Name.fresh_named_var "venvtag"
				   val venv_tag_bnd = Exp_b(venv_tag_name,TraceKnown TraceInfo.Notrace_Int,
							    Prim_e(NilPrimOp mk_record_gctag,trs,[Var_c venv_type_name],[]))
				 in NilUtil.makeLetE Sequential (venv_tag_bnd::bnds) 
				             (Prim_e(NilPrimOp(record labels),[],[], (Var_e venv_tag_name)::fields))
				 end)
		     in  (venv, code_bnds, TraceKnown TraceInfo.Trace, venv_type_name,venv_type)
		     end)

	   (* Now we can rewrite the formal parameters and build the code of the function. *)
	   val vklist_code = (cenv_var,Single_k cenv) :: vklist_cl
	   val c_mapper =
	       (*if doConTrans then
		   (fn c => c_rewrite state (NilSubst.substConInCon internal_subst c))
	       else*)
		   (fn c => NilSubst.substConInCon internal_subst c)

	   val code_type = AllArrow_c {effect = effect, openness = Code,
				       tFormals = vklist_code,
				       eFormals = Var_c venv_type_var :: (map c_mapper etypes), fFormals = numf,
				       body_type = codebody_tipe}
	   val D = insert_con(D, code_var, code_type)
	   val code_body = e_rewrite D inner_state body

	   fun vtr_mapper (v,tr) = 
	       let val tr = trace_rewrite state (NilSubst.substConInTrace internal_subst tr)
	       in  (v, tr)
	       end
	   val vtrlist_code = map vtr_mapper ((venv_var,venv_tr) :: (map #1 eFormals))
	   val code_fun = Function{effect=effect,recursive=recursive,
				   tFormals=map #1 vklist_code,
				   eFormals=vtrlist_code,
				   fFormals=fvars,
				   body = NilUtil.makeLetE Sequential (code_cbnds @ code_bnds) code_body}

	   
	   val closure = {code = code_var,
			  cenv = cenv,
			  venv = venv}
       in if get_escape v then (SOME(fidtype_var,closure_tipe),
			  (venv_type_var,venv_type),
			  ((code_var,code_type),code_fun),
			  SOME (is_recur,((v,closure_tipe),closure)))
	  else (NONE,(venv_type_var,venv_type),((code_var,code_type),code_fun),NONE)
       end

   and vklist_rewrite state vklist = map (fn (v,k) => (v,k_rewrite state k)) vklist

   and bnd_rewrite D state bnd : (bnd list * NilContext.context) =
       (case bnd of
	  (Con_b(p,cb)) => let val cbnds = cbnd_rewrite state cb
			       val D = foldl (fn (cb, D) =>
					      let
						  val (v,c) = NilUtil.extractCbnd cb
					      in
						  NilContext.insert_equation(D,v,c)
					      end) D cbnds
		   	   in  (map (fn cb => Con_b(p,cb)) cbnds,D)
			   end
	| (Exp_b(v,niltrace,e)) =>
			   let
			       val e' = e_rewrite D state e
			   in
			       ([Exp_b(v, trace_rewrite state niltrace, e')],
				NilContext.insert_con(D,v, Normalize.type_of (D, e')))
			   end
         | (Fixclosure_b _) => error "there can't be closures while closure-converting"
	 | (Fixcode_b _) => error "there can't be codes while closure-converting"
	 | (Fixopen_b var_fun_set) => 
	       let 
		   val var_fun_list = Sequence.toList var_fun_set
		   val var_fun_list = map (fn ((v, c), f) => ((v, c_rewrite state c), f)) var_fun_list
		   val (vars,D) = Listops.foldl_acc (fn (((v,c),_),D) => (v,NilContext.insert_con(D,v,c))) D var_fun_list

		   val (closure_types,venv_types,pfun_list,closures) = 
		     Listops.unzip4 (map (fun_rewrite D state vars) var_fun_list)

		   (* Create bnds to give names to the types of the closures and of the value environments. *)
		   val pfun_typebnds = 
		     (List.mapPartial (fn (SOME (v,c)) => SOME (Con_b(Compiletime,Con_cb(v,c))) | NONE => NONE) closure_types)
		     @ (map (fn (v,c) => (Con_b(Compiletime,Con_cb(v,c)))) venv_types)

		   (* A bnd for the code of all the functions in the nest. *)
		   val pfun_bnd = Fixcode_b (Sequence.fromList pfun_list)

		   fun make_fix (is_recur,[]) = []
		     | make_fix (is_recur,ls) = [Fixclosure_b(is_recur,Sequence.fromList ls)]

		   (* Separate out the recursive closures from the non-recursive ones. *)
		   fun closure_loop recur (group,separate) [] = 
		          (make_fix(false,separate)) @ (make_fix(recur,group))
		     | closure_loop recur (group,separate) (clos::rest) = 
			  (case clos 
			     of SOME (r,cl) =>
			       closure_loop (recur orelse r)
			       (if r then (cl::group,separate) else (group,cl::separate)) rest
			      | NONE => closure_loop recur (group,separate) rest)

		   val closure_bnd_list = closure_loop false ([],[]) closures

	       (* Now put all of these bnds together. *)
	       in  (pfun_typebnds @ (pfun_bnd :: closure_bnd_list),D)
	       end)

   and trace_rewrite state trace : niltrace = 
       let fun help c = 
	   let val c = c_rewrite state c
	       val (Var_c v, labs) = extract_cpath c
	   in  case labs of
	       [] => TraceCompute v
	     | _ => TraceKnown(TraceInfo.Compute(v,labs))
	   end
       in (case trace of
	       TraceUnknown => trace
	     | TraceKnown (TraceInfo.Compute(v,labs)) => help(NilDefs.path2con(v,labs))
	     | TraceKnown _ => trace
	     | TraceCompute v => help(Var_c v))
       end
		     

   and e_rewrite D state arg_exp : exp =
       let 
(*
	   val _ = (print "  e_rewrite called on = \n";
		    pp_exp arg_exp; print "\n")
*)


	   (* there are no function definitions within e_rewrite so we use same state *)
	   val e_recur = e_rewrite D state
	   val c_rewrite = c_rewrite state
       in
       (case arg_exp of
	    Var_e v =>
	       let val fid = current_fid state
                   val {free_evars,...} = get_frees fid
               in  case VarMap.find(free_evars,v) of
			NONE => arg_exp
		      | SOME(v,_(*,_*)) => Var_e v
	       end
	  | Prim_e(NilPrimOp np,trlist,clist,elist) => 
	       let val np = (case np of
				 make_vararg(_,e) => make_vararg(Closure,e)
			       | make_onearg(_,e) => make_onearg(Closure,e)
			       | _ => np)
	       in  Prim_e(NilPrimOp np,map (trace_rewrite state) trlist,map c_rewrite clist, map e_recur elist)
	       end
	  | Prim_e(p,trlist,clist,elist) => 
	       Prim_e(p,map (trace_rewrite state) trlist,map c_rewrite clist, map e_recur elist)
	  | Const_e v => arg_exp
	  | Switch_e switch => 
		Switch_e(case switch of 
		     Intsw_e{size,arg,arms,default,result_type} => 
			 Intsw_e{size = size, 
				 arg = e_recur arg,
				 arms = Listops.map_second e_recur arms,
				 default = Util.mapopt e_recur default,
				 result_type=c_rewrite result_type}
		   | Sumsw_e{sumtype,arg,bound,arms,default,result_type} => 
	              let 
			val (true,sumtype') = Normalize.reduce_hnf (D,sumtype)
			fun stype w = NilUtil.convert_sum_to_special (sumtype',w)
		      in
			   Sumsw_e{sumtype=c_rewrite sumtype, 
				   arg=e_recur arg,
				   bound = bound,
				   arms = map (fn (w,tr,e) => 
					       (w, trace_rewrite state tr,
						e_rewrite (NilContext.insert_con(D,bound, stype w)) state e)) arms,
				   default=Util.mapopt e_recur default,
				   result_type=c_rewrite result_type}
		      end
		   | Exncase_e{arg,arms,bound,default,result_type} => 
			   let
			     fun itype (D, e) = 
			       let val (true,(Prim_c (Exntag_c,[con]))) = Normalize.reduce_hnf (D, Normalize.type_of (D, e))
			       in con
			       end
			     fun mapper (e,tr,f) = 
			       (e_recur e,
				trace_rewrite state tr,
				e_rewrite (NilContext.insert_con(D,bound,itype (D,e))) state f)
			   in
			     Exncase_e{arg=e_recur arg,
				       bound=bound,
				       arms=map mapper arms,
				       default=Util.mapopt e_recur default,
				       result_type=c_rewrite result_type}
			   end
		   | Typecase_e {arg,arms,default,result_type} =>
			     Typecase_e{arg=c_rewrite arg,
					default=e_recur default,
					result_type=c_rewrite result_type,
					arms=map (fn (pc,vklist,e) =>
						  (pc, vklist_rewrite state vklist, e_rewrite (NilContext.insert_kind_list(D,vklist)) state e)) arms})
			 
	  | Let_e(letsort,bnds,e) => let val (bnds_list,D) =
		                             Listops.foldl_acc (fn (bnd,D) => (bnd_rewrite D state bnd)) D bnds
				     in Let_e(letsort, List.concat bnds_list, e_rewrite D state e)
				     end
	  | App_e (Closure, _,_,_,_) => error "App_e(Closure,...) during closure conversion (rewrite phase)"
	  | ExternApp_e (e, elist) => 
		let val elist' = map e_recur  elist
		    val e' = e_recur e
		in  ExternApp_e(e', elist')
		end
	  | App_e (Code, e, clist, elist, eflist) => 
		let val clist' = map c_rewrite  clist
		    val elist' = map e_recur  elist
		    val eflist' = map e_recur eflist
		    val e' = e_recur e
		in  App_e(Code, e', clist', elist', eflist')
		end
	  | App_e (Open, e, clist, elist, eflist) => 
		(* Catch the special case that this is the current function calling itself. *)
		(* In that case, generate a direct (Code) call, passing the current environments. *)
		(*  -joev                                                                         *)
		let val clist' = map c_rewrite  clist
		    val elist' = map e_recur  elist
		    val eflist' = map e_recur eflist
		    fun docall (cv,{free_cvars, free_evars} : frees) = 
			let val vk_free = map #1 (VarMap.listItemsi free_cvars)
			    val vc_free = VarMap.listItemsi free_evars
			    val {free_evars,...} = get_frees(current_fid state)
			    val {cenv_var,venv_var,...} = get_static (current_fid state)
			    val clist'' = (Var_c cenv_var) :: clist'
			    val elist'' = (Var_e venv_var) :: elist'
			in App_e(Code, Var_e cv, clist'', elist'', eflist')
			end
		    fun default() = App_e(Closure,e_recur e, clist', elist', eflist') 
		in  (case e of
			 Var_e v => if (Name.eq_var(v,current_fid state))
					then let val {code_var,...} = get_static v
					     in  docall(code_var,get_frees v)
					     end
				    else default()
		       | _ => default())
		end
	  | Raise_e (e,c) => Raise_e(e_recur e, c_rewrite c)
	  | Handle_e {body,bound,handler,result_type} => 
		Handle_e{body = e_recur body, bound = bound,
			 handler = e_rewrite (insert_con(D,bound,Prim_c(Exn_c,[]))) state handler,
			 result_type = c_rewrite result_type}
	  | Fold_e (vars,from,to) => Fold_e (vars,c_rewrite from,c_rewrite to)
	  | Unfold_e (vars,from,to) => Unfold_e (vars,c_rewrite from,c_rewrite to)
	  | Coerce_e (coercion,cargs,exp) => 
	    Coerce_e (e_recur coercion, map c_rewrite cargs, e_recur exp)
	    )
       end

   (* We don't use the context for closure converting constructors 
    * and kinds, so we don't bother to pass it in.
    *)
   and cbnd_rewrite state (Con_cb(v,c)) : conbnd list = 
	    [Con_cb(v,c_rewrite state c)]
     | cbnd_rewrite state (Open_cb(v,vklist,c)) =
         let val _ = if (!debug)
			 then (print "  cbnd_rewrite v = "; Ppnil.pp_var v; print "\n")
		     else ()
	     val (code_var, cenv_var, vkl_free, cbnds, subst) = 
		 if (is_fid v)
		     then let val {code_var, cenv_var, ...} = get_static v
			      val {free_cvars,...} = get_frees v    (* free_evars must be empty *)
			      fun folder ((v,v'),subst) = 
				let val k = Single_k(Var_c v)
				    val l = Name.internal_label(Name.var2string v)
				    val c = Proj_c(Var_c cenv_var, l)
				in  (((v,k,l), Con_cb(v',c)), NilSubst.C.sim_add subst (v,c))
				end
			      val (temp, subst) = Listops.foldl_acc folder (NilSubst.C.empty()) 
							(VarMap.listItemsi free_cvars)
			      val (vkl_free, cbnds) = Listops.unzip temp
			  in  (code_var, cenv_var, vkl_free, cbnds, subst)
			  end
		 else  (Name.fresh_named_var "unclosed_open_cb",
			Name.fresh_named_var "unclosed_open_cb_cenv",
			[], [], NilSubst.C.empty())


	     val vklist = vklist_rewrite state vklist
	     val vkl_free = map (fn (v,k,l) => (v,k_rewrite state k,l)) vkl_free
	     val vkl_free_kind = Record_k(Sequence.fromList(map
							    (fn (v,k,l) => ((l, Name.derived_var v), k))
							    vkl_free))

	     val vklist' = (cenv_var, vkl_free_kind) :: vklist 
	     val inner_state = if (is_fid v) then (copy_state(state ,v)) else state
	     val body    = c_rewrite inner_state c
	     val code_cb = Code_cb(code_var, vklist',Let_c(Sequential,cbnds,body))
	     val con_env = Crecord_c(map (fn (v,_,l) => (l,c_rewrite state (Var_c v))) vkl_free)
	     val closure_cb = Con_cb(v,Closure_c (Var_c code_var, con_env))
	 in  [code_cb, closure_cb]
	 end			
     | cbnd_rewrite state (Code_cb _) = error "found Code_cb during closure-conversion"

   (* We don't use the context for closure converting constructors 
    * and kinds, so we don't bother to pass it in.
    *)
   and c_rewrite state arg_con : con = 
       let val c_rewrite = c_rewrite state
       in
	(case arg_con of
            Prim_c (primcon,clist) => 
		let val primcon = (case primcon of
				       Vararg_c(_, e) => Vararg_c(Closure,e)
				     | _ => primcon)
		in  Prim_c(primcon, map c_rewrite clist)
		end
	  | Mu_c (ir,vc_set) => Mu_c(ir,Sequence.fromList 
					(map (fn (v,c) => (v,c_rewrite c)) (Sequence.toList vc_set)))
	  | Var_c v => 
	       let val fid = current_fid state
                   val {free_cvars,...} = get_frees fid
               in  case VarMap.find(free_cvars,v) of
			NONE => arg_con
		      | SOME v => Var_c v
	       end
	  | AllArrow_c {openness,effect,tFormals,eFormals,fFormals,body_type} =>
		let val tFormals' = map (fn(v,k) => (v,k_rewrite state k)) tFormals
		    val eFormals' = map c_rewrite eFormals
		    val openness' = (case openness of
				   Open => Closure
				 | Code => Code
				 | Closure => (Ppnil.pp_con arg_con; print "\n"; error ("AllArrow_c(Closure,...) " ^ 
						     "during closure-conversion")))
		    val body_type' = c_rewrite body_type
		in  AllArrow_c{openness=openness',effect=effect,
			       tFormals=tFormals',eFormals=eFormals',
			       fFormals=fFormals,body_type=body_type'}
		end
	  | ExternArrow_c (clist,c) => 
		ExternArrow_c(map c_rewrite clist,
			      c_rewrite c)

	  | Let_c (letsort,cbnds,c) => 
		let val cbnds' = List.concat(map (cbnd_rewrite state) cbnds)
		    val c = c_rewrite c
		in  (case (c,cbnds') of
			 (_,[]) => c
		       | (Var_c v, [Con_cb(v',c')]) => if (Name.eq_var(v,v'))
							     then c'
							 else Let_c(letsort,cbnds',c)
		       | _ => Let_c(letsort,cbnds',c))
		end
	  | Coercion_c {vars,from,to} =>
		Coercion_c {vars=vars, 
			    from=c_rewrite from,to=c_rewrite to}
	  | Crecord_c lclist => Crecord_c(map (fn (l,c) => (l,c_rewrite c)) lclist)
	  | Proj_c (c,l) => Proj_c(c_rewrite c, l)
	  | Closure_c (c1,c2) => error "should not encounter Closure_c during closure-conversion"
	  | App_c (c,clist) => App_c(c_rewrite c, map c_rewrite clist))
       end


   (* We don't use the context for closure converting constructors 
    * and kinds, so we don't bother to pass it in.
    *)
  and k_rewrite state arg_kind : kind =
       (case arg_kind of 
	    Type_k => arg_kind
	  | SingleType_k c =>  SingleType_k (c_rewrite state c)
	  | Single_k c =>  Single_k (c_rewrite state c)
	  | (Record_k lvkseq) => let val lvklist = Sequence.toList lvkseq
				     val lvklist = map (fn ((l,v),k) => ((l,v),k_rewrite state k)) lvklist
				 in  Record_k(Sequence.fromList lvklist)
				 end
	  | Arrow_k (Open,vklist,k) => 
		let val vklist' = map (fn (v,k) => (v,k_rewrite state k)) vklist
		    val k' = Arrow_k(Closure, vklist', k_rewrite state k)
		in  k'
		end
	  | Arrow_k _ => error ("cannot encounter arrow_k(Code/Closure,...) " ^ 
				"during closure-conversion"))




   fun close_mod (MODULE{bnds, imports, exports}) = 
       let val _ = reset_table []
	   val top_fid = Name.fresh_named_var "top_fid"
	   val state = initial_state top_fid

	   (* Scan module for free variables *)
	   fun import_folder (ImportValue(l,v,_,c),state) = 
	       let val f = c_find_fv (state,empty_frees) c
	       in  add_gboundevar(state,v,c)
	       end
	     | import_folder (ImportType(l,v,k),state) = 
	       let val f = k_find_fv (state,empty_frees) k
	       in  add_gboundcvar(state,v,k)
	       end
	     | import_folder (ImportBnd (_, cb),state) =
	       let
		   val v =
		       case cb of
			   Con_cb (v, _) => v
			 | Open_cb (v, _, _) => v
			 | Code_cb (v, _, _) => v
	       in
		   add_gboundcvar(state,v,Type_k)
	       end
	   val _ = chat "  Scanning for free variables\n"
	   val state = foldl import_folder state imports
	   val ({free_evars,free_cvars,...},state) = foldl bnd_find_fv (empty_frees,state) bnds
	   fun export_mapper state (ExportValue(l,v)) = e_find_fv (state, empty_frees) (Var_e v)
	     | export_mapper state (ExportType(l,v)) = c_find_fv (state, empty_frees) (Var_c v)
	   val _ = map (export_mapper state) exports

	   val _ = if (!debug)
		       then (print "Done with e_find_fv\n";
			     print "free is empty: ";
			     print (Bool.toString (VarMap.numItems free_evars = 0 andalso
						   VarMap.numItems free_cvars = 0));
			     print "\n")
		   else ()

           (* Perform transitive closure computation *)
	   (* joev: transitive closure is commented out because it is currently trivial:   *)
	   (* all direct calls are self calls, so close_funs only makes one pass and does *)
	   (* not change anything.                                                         *)
	   (*
	   val _ = chat "  Computing transitive closure of close funs\n"
	   val _ = close_funs(get_fids())
	   val _ = if (!debug)
		       then print "close_mod: Done with close_funs\n"
		   else ()
	   *)

	   (* Rewrite module *)
	   val initial_state = new_state top_fid
	   val _ = chat "  Rewriting imports, bindings, exports\n"

	   fun import_folder (ImportValue(l,v,tr,c),D) =
	       let val c = c_rewrite initial_state c
		   val tr = trace_rewrite initial_state tr
		   val D = insert_con(D, v, c)
		   val D = insert_label(D, l, v)
	       in  (ImportValue(l,v,tr,c), D)
	       end
	     | import_folder (ImportType(l,v,k),D) =
	       let val k = k_rewrite initial_state k
		   val D = insert_kind(D, v, k)
		   val D = insert_label(D, l, v)
	       in  (ImportType(l,v,k),D)
	       end
	     | import_folder (ImportBnd (phase, cb),D) =
	       let val cb = List.last (cbnd_rewrite initial_state cb)
		   val (v, k) = NilStatic.kind_of_cbnd (D, cb)
		   val D = insert_kind(D, v, k)
	       in
		   (ImportBnd (phase, cb),D)
	       end
	   val (imports', D) = Listops.foldl_acc import_folder (NilContext.empty()) imports

	   fun folder (bnd,D) = bnd_rewrite D initial_state bnd
	   val (bndsbnds,D) = Listops.foldl_acc folder D bnds
	   val bnds' = List.concat bndsbnds

	   fun export_rewrite (ExportValue(l,v)) = ExportValue(l,v)
	     | export_rewrite (ExportType(l,v)) = ExportType(l,v)

	   val exports' = map export_rewrite exports

	   val _ = reset_table []

       in  MODULE{bnds = bnds', imports = imports', exports = exports'}
       end	   


end



