(*$import NilContext NilUtil Ppnil NilSubst Normalize TOCLOSURE Stats Listops Bool *)

(* Are argument traces being properly closure converted *)

(* Closure conversion is accomplished in two phases.  
   The first phase scans the program for free type and term variables of 
   all type and term functions and also notes whether a function escapes or not.  
   The second phase then rewrites the functions into codes and closures.  
   Thus, before closure conversion, there are Fixopen_b and App_e(Open,...).
   After closure conversion, there are Fixcode_b, Fixclosure_b, 
   App_e(Closure,...), and App_e(Code,...).
*)

structure ToClosure
    :> TOCLOSURE =
struct


    val do_single_venv = Stats.tt("Closure_TermCompress")
    val closure_print_free = Stats.ff "closure_print_free"


    open Util NilUtil Name Nil Ppnil Listops
    structure Nil = Nil

    val error = fn s => error "toclosure.sml" s
    val debug = Stats.ff("ToclosureDebug")

    val float64 = Prim_c(Float_c Prim.F64,[])
    structure FidSet = VarSet
    structure FidMap = VarMap
    type fid = var
    fun chat s = print s

    (* -------------- types and values manipulating free variables ------------- *)
    (* lists are kept in reverse order; map allow fast lookup/membership  *)
    type frees = {free_cvars : var VarMap.map,
		  free_evars : (var * niltrace * con option) VarMap.map}

    val empty_frees = {free_cvars = VarMap.empty,
		       free_evars = VarMap.empty}

    fun varmapUnion(oldmap,newmap) = 
	let fun folder(var,item,acc as (changed,curmap)) =
	    case (VarMap.find(curmap,var)) of
		  SOME _ => acc
		| NONE => (false, VarMap.insert(curmap,var,item))
	in  VarMap.foldli folder (true,oldmap) newmap
	end

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

    fun join_free args : frees = #2(join_free' args)


    fun show_free({free_evars, free_cvars} : frees) =
	(print "free_evars are: "; 
	 VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print "; ")) free_evars; print "\n";
	 print "free_cvars are: "; 
	 VarMap.appi (fn (v,_) => (Ppnil.pp_var v; print "; ")) free_cvars; print "\n")


    (* c.l1.l2.l3 -> (c, [l1,l2,l3]) *)
    fun extract_cpath c = 
	let fun loop acc (Proj_c(c,l)) = loop (l::acc) c
	      | loop acc c = (c,rev acc)
	in loop [] c
	end


    (* -------- code to perform the initial free-variable computation ---------------- *)
    local
	datatype expentry = GLOBALe 
	                  | SHADOWe of niltrace * con option (* Available when of function type *)
			  | LOCALe  of niltrace * con option
	datatype conentry = GLOBALc 
	                  | SHADOWc
			  | LOCALc
	datatype state = STATE of {curfid : fid * fid list,
				   is_top : bool,
				   boundevars : expentry VarMap.map,
				   boundcvars : conentry VarMap.map,
				   boundfids : FidSet.set}
	type funentry = {static : {fid : fid, 
				   fidtype_var : var,
				   code_var : fid,
				   unpack_var : fid,
				   cenv_var : var,
				   venv_var : var},
			 escape : bool,
			 callee : (fid * state) list,
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

	fun add_boundevars' shadow (STATE{is_top,curfid,boundevars,boundcvars,boundfids},v_tr_copt_list) = 
	    let fun folder((v,tr,c),m) = 
		let val entry = if shadow then SHADOWe(tr,c)
				else (if is_top then GLOBALe
				      else LOCALe(tr,c))
		in  VarMap.insert(m,v,entry)
		end
		val boundevars' = foldl folder boundevars v_tr_copt_list
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
		    in  (fidtype_var, (fid, TraceKnown TraceInfo.Trace, SOME(Var_c fidtype_var)))
		    end
		val (vlist,fid_tr_types) = Listops.unzip(map mapper fid_types)
		val state = add_boundcvars (state,vlist)
	    in  add_boundevars' shadow (state,fid_tr_types)
	    end

	fun add_boundevar(s,v,tr,copt) = add_boundevars(s,[(v,tr,copt)])
	fun add_boundcvar(s,v) = add_boundcvars (s,[v])
	    
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


	fun cvar_isfree (STATE{boundcvars,boundevars,...},{free_cvars,...}:frees,cvar) = 
	    if (case (VarMap.find(free_cvars,cvar)) of
		      NONE => false
		    | _ => true)
		then false
	    else (case (VarMap.find(boundcvars,cvar)) of
			  SOME GLOBALc => false
			| SOME LOCALc  => false
			| SOME SHADOWc => true
			| NONE => error ("cvar_isfree: variable " ^
					 (var2string cvar) ^ " not bound"))


	fun evar_isfree (STATE{boundcvars,boundevars,...},{free_evars,...}:frees,evar) = 
	    if (case VarMap.find(free_evars,evar) of
		    NONE => false
		  | _ => true)
		then NONE
	    else 
		(case (VarMap.find(boundevars,evar)) of
		     SOME GLOBALe => NONE
		   | SOME (LOCALe _) => NONE
		   | SOME (SHADOWe (c,f)) => SOME(c,f)
		   | NONE => error ("evar_isfree: variable " ^
				    (var2string evar) ^ " not bound"))
			    


	fun free_cvar_add ({free_cvars,free_evars}:frees, cvar) =
	    let val free_cvars =
		    case VarMap.find(free_cvars,cvar) of
			SOME _ => error "free_cvar given variable already in free set"
		      | _ => VarMap.insert(free_cvars, cvar, derived_var cvar)
            in   {free_evars = free_evars,
	          free_cvars = free_cvars}
	    end

	fun free_evar_add ({free_cvars,free_evars} : frees, evar, tr, c) =
	    let val free_evars =
		  (case (VarMap.find(free_evars,evar)) of
		       SOME _ => error "free_evar_add given variable already in free set"
		     | NONE => VarMap.insert(free_evars,evar,(derived_var evar,tr,c)))
	    in  {free_evars = free_evars,
		 free_cvars = free_cvars}
	    end


	fun empty_fun escape new_fid : funentry = 
	    let val name = var2name new_fid
	    in   {static = {fid = new_fid,
		       fidtype_var = fresh_named_var(name ^ "_type"),
		       code_var = fresh_named_var(name ^ "_code"),
		       unpack_var = fresh_named_var(name ^ "_unpack"),
		       cenv_var = fresh_named_var(name ^ "_cEnv"),
		       venv_var = fresh_named_var(name ^ "_eEnv")},
	          escape = escape,
	          callee = [],
	          frees = empty_frees}
	    end
	val global_escapes = ref (VarSet.empty : Name.VarSet.set)




    fun show_state(STATE{boundevars, boundcvars, ...} : state) =
	let fun show_eentry GLOBALe = print "GLOABLe"
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
	    (case (VarMap.find(!fids,fid)) of
		 NONE => error ((Name.var2string fid) ^ "fid not found for add_frees")
	       | SOME (r as (ref{static,escape,callee,frees : frees})) =>
		     r := {static = static,
			   escape = escape,
			   callee = callee,
			   frees = join_free(frees,f) : frees})

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
	let
	    fun do_fix frees var_arm_set do_arm =
		let val var_arm_list = Sequence.toList var_arm_set
		    val fids = map #1 var_arm_list
		    val _ = app add_fun fids
		    val local_fids_types = map (fn (v,pf) => 
						(v,NilUtil.function_type Open pf)) 
			var_arm_list
		    val free = (foldl (fn (a,f) => let val f' = do_arm local_fids_types a
						   in  join_free(f,f')
						   end)
				frees (Sequence.toList var_arm_set))
		in  (add_boundfids false (state, local_fids_types), free)
		end
	in
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
		     in (f, add_boundevar(state,v,tr,NONE))
		     end
	       | Fixopen_b var_fun_set =>
		   let val (outer_curfid,_) = get_curfid state
		       fun do_arm fids_types (v,Function{tFormals,eFormals,fFormals,body,
							 body_type,...}) =
			   let val outer_state = add_boundfids false (state,fids_types) 
			       val fids = map #1 fids_types
			       val local_state = copy_state state (v,fids)
			       val local_state = add_boundfids true (local_state,fids_types) 
			       val fs = (empty_frees, local_state)
			       val _ = if (!debug)
					   then (print "state before vkfolder to ";
						 Ppnil.pp_var v; print "\n";
						 show_state (#2 fs); print "\n")
				       else ()
			       val fs = vklist_find_fv (tFormals,fs)
			       val _ = if (!debug)
					   then (print "the following (after vkfolder) frees to ";
						 Ppnil.pp_var v; print "\n";
						 show_free (#1 fs); print "\n")
				       else ()
			       val (f,s) = vtrtlist_find_fv (eFormals, fs)
				   
			       val _ = if (!debug)
					   then (print "the following (after vcfolder) frees to ";
						 Ppnil.pp_var v; print "\n";
						 show_free (#1 fs); print "\n")
				       else ()
			       val f = e_find_fv (s,f) body
			       val _ = if (!debug)
					   then (print "the following (after body) frees to ";
						 Ppnil.pp_var v; print "\n";
						 show_free f; print "\n")
				       else ()
			       val f = t_find_fv (s,f) body_type
			       val _ = if (!debug)
					   then (print "adding the following frees to ";
						 Ppnil.pp_var v; print "\n";
						 show_free f; print "\n")
					     else ()
						 
			       fun efolder(v,(_,tr,copt),f) = 
				   let 
				       val self = Listops.member_eq(eq_var,v,
								    #2(get_curfid (#2 fs)))
				   in  (case (self,copt) of
					  (false, _) => free_evar_add(f,v,tr,copt)
					| (true, SOME _) => free_evar_add(f,v,tr,copt)
					| _ => error "no optional type for function")
				   end
			       fun cfolder(v,_,f) = free_cvar_add(f,v)
			       val {free_evars,free_cvars} = f
			       val lf = VarMap.foldli efolder empty_frees free_evars
			       val lf = VarMap.foldli cfolder lf free_cvars	   
			       val f = {free_evars = free_evars,
					free_cvars = #free_cvars lf}
			       val _ = add_frees(v,lf)
			   in  f
			   end
		       val (s,f) = do_fix frees var_fun_set do_arm
		   in  (remove_free(s,f), s)
		   end
	       | Fixcode_b _ => error "there can't be codes while closure-converting"
	       | Fixclosure_b _ => error "there can't be closures while closure-converting")
	end
	

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
			let val state = add_boundevar(state,bound,tr,NONE)
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
					    val state = add_boundevar(state,bound,tr,NONE)
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
		 (if (is_boundfid(state,v)) then add_escape v else ();
		  (case (evar_isfree(state,frees,v)) of
			   NONE => frees
			 | SOME (tr,copt) => let val frees = free_evar_add(frees,v,tr,copt)
					     in  trace_find_fv(state, frees) tr
					     end))
	   | Prim_e (p,clist,elist) =>
		 let fun tfold(t,f) = t_find_fv (state,f) t
		     fun cfold(c,f) = c_find_fv (state,f) c
		     fun efold(e,f) = e_find_fv (state,f) e
		     val frees = if (NilUtil.allprim_uses_carg p)
				     then foldl cfold frees clist
				 else foldl tfold frees clist
		     val frees = foldl efold frees elist
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
						     eq_var(v,#1(get_curfid state)))
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
		     val f = e_find_fv (add_boundevar(state,bound,TraceKnown TraceInfo.Trace,NONE), f) handler
		     val f = t_find_fv (state,f) result_type
		 in  f
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

    and vklist_find_fv (vklist,fs) =
	let fun folder((v,k),(f,s)) = 
	    (k_find_fv (s,f) k, add_boundcvar(s,v))
	in  foldl folder fs vklist
	end

    and vtrtlist_find_fv (vtrclist,fs) =
	let fun folder((v,tr,c),(f,s)) = 
	    (trace_find_fv (s, t_find_fv (s,f) c) tr,
	     add_boundevar(s,v,tr,NONE))
	in  foldl folder fs vtrclist
	end

    and vopttrtlist_find_fv (vclist,fs) =
	let fun folder((vopt,tr,c),(f,s)) = 
	    (t_find_fv (s,f) c, 
	     (case vopt of 
		  NONE => s
		| SOME v => add_boundevar(s,v,tr,NONE)))
	in  foldl folder fs vclist
	end

    and cbnd_find_fv (cbnd,(f,s)) : frees * state =
	(case cbnd of
	     Con_cb(v,c) => let val f' = c_find_fv (s,f) c
				val state = add_boundcvar(s,v)
			    in (f',state)
			    end
	   | (Code_cb(v,vklist,c)) => (f,s)
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
		 Prim_c(Record_c(labs,SOME vars),clist) =>  
		     let val vtrclist = map2 (fn (v,c) => (v,TraceUnknown,c)) (vars,clist)
			 val (f,s) = vtrtlist_find_fv (vtrclist,(frees,state))
		     in  f
		     end
	       | Prim_c (pc,clist) => foldl (fn (c,f)=> (c_find_fv (state,f) c)) frees clist
	       | Mu_c (_,vcset) =>
		     let val vclist = Sequence.toList vcset
			 (* we need to alpha-vary since reductions may lead to duplication
			    of bound variables despite A-normal linearization *)
			 val (vclist) = alpha_mu (fn v => is_boundcvar(state,v)) (vclist)
			 val state' = add_boundcvars(state,map #1 vclist)
		     in  foldl (fn ((v,c),f) => c_find_fv (state',f) c) frees (Sequence.toList vcset)
		     end
	       (* the types of some primitives like integer equality are Code arrow *)
	       | AllArrow_c{tFormals,eFormals,fFormals,body_type,...} =>
		     let val fs = vklist_find_fv (tFormals,(frees,state))
			 val vopttrt_list = map (fn (vopt,t) => (vopt, TraceUnknown, t)) eFormals
			 val (f,s) = vopttrtlist_find_fv (vopttrt_list,fs)
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
	       | Typecase_c {arg, arms, default, kind} => 
		     let val f = c_find_fv (state,frees) arg
			 val f = c_find_fv (state,f) default
			 val f = k_find_fv (state,f) kind
			 fun armfolder ((pc,vklist,c),f) = 
			     let fun folder((v,k),(f,s)) = (k_find_fv (s,f) k,
							    add_boundcvar(s,v))
				 val (f,_) = foldl folder (f,state) vklist
			     in  c_find_fv (state,f) c
			     end
		     in  foldl armfolder f arms
		     end
	       | Let_c(_,cbnds,c) => 
		     let 
			 val (f,s) = foldl cbnd_find_fv (frees,state) cbnds
			 val f' = c_find_fv (s,f) c
(*			 val f' = remove_free(s,f') *)
		     in  f'
		     end
	       | Crecord_c lclist => foldl (fn ((_,c),f) => c_find_fv (state,f) c) frees lclist
	       | Typeof_c e => frees
	       | Proj_c (c,l) => c_find_fv (state,frees) c
	       | Closure_c (c1,c2) => c_find_fv (state, c_find_fv (state,frees) c1) c2
	       | App_c(c,clist) => let val f = c_find_fv (state,frees) c
				   in foldl (fn (c,f) => c_find_fv (state,f) c) f clist
				   end
	       | Annotate_c (_,c) => c_find_fv (state,frees) c)
		 
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

   local
       fun close_fun (curfid,nextset) = 
	   let val callees = get_callee curfid
(* val _ = VarSet.app (fn x => (print "\n    callee_fvs is: "; 
                                   show_free (get_frees x); print "\n")) callees *)
(* val callee_fvs = VarSet.foldl 
 (fn (f,fv) => join_free(get_frees f, fv)) empty_frees callees *)
	       val callee_fvs = foldl (fn ((fid,s),fv) => let val f = get_frees fid
							       val f = remove_free(s,f)
							   in  join_free(f, fv)
							   end) empty_frees callees 
(* val _ = (print "\n\nfinal callee_fvs is: "; show_free callee_fvs; print "\n") *)
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
		    val nextset = (VarSet.foldl close_fun VarSet.empty workset)
		in  if (VarSet.isEmpty nextset)
			then ()
		    else loop()  (* note that we must start with the whole set again *)
		end
	    val _ = loop()
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





   (* ------- rewrite each term-level binding to a list of bindings ---------------------
      In general, each function is re-written to one or more functions
      and zero or more closures.  No closures are needed and generated
      if the function does not escape.

      ----- Function bindings are rewritten as follows.
      
      f == Function(vargs, fargs) = body --->
      
      [fcode == Pfunction(cargs @ cfv, vargs @ vfv, fargs) = body
       funpack == Pfunction(cargs @ [cenv], vargs @ [venv], fargs) = body
       f == Closure(fcode,cenv,venv)]

       ------------------------------------------------------------------------ *)

   fun cproj cvar i = Proj_c(Var_c cvar, generate_tuple_label(i+1))
   fun eproj (evar,rectype) i = Prim_e(NilPrimOp(select (generate_tuple_label(i+1))),
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

     local fun path_eq((v1,l1),(v2,l2)) = 
		(eq_var(v1,v2) andalso (Listops.eq_list(eq_label,l1,l2)))
	   fun chandle subst (_,c) = 
		let val (base,labels) = extract_cpath c
		in  case base of
		      Var_c v => 
			(case (Listops.assoc_eq(path_eq,(v,labels),subst)) of
				NONE => NOCHANGE
			      | SOME c => CHANGE_NORECURSE c)
		    | _ => NOCHANGE
		end
	fun handlers subst = {exphandler = default_exphandler,
			      conhandler = chandle subst,
			      kindhandler = default_kindhandler,
			      bndhandler = default_bndhandler,
			      cbndhandler = default_cbndhandler}
     in  fun substConPathInCon (subst,c) = NilUtil.con_rewrite (handlers subst) c
         fun substConPathInKind (subst,k) = NilUtil.kind_rewrite (handlers subst) k
     end 
   fun fun_rewrite state vars (v,Function{effect,recursive,isDependent,
					  tFormals,eFormals,fFormals,body,body_type}) =
       let 
	   val _ = if (!debug)
		       then (print "\n***fun_rewrite v = "; Ppnil.pp_var v; print "\n")
		   else ()
	   val {code_var, fidtype_var, unpack_var, cenv_var, venv_var, ...} = get_static v
	   val {free_cvars,free_evars=pc_free} = get_frees v
	   val vkl_free = VarMap.foldli (fn (v,v',acc) => let val k = Single_k(Var_c v)
					   val l = Name.internal_label(Name.var2string v)
				         in  (v,v',k,l)::acc
				         end) [] free_cvars

	   val inner_state = copy_state(state,v)

	   val escape = get_escape v
	   val vkl_free = map (fn (v,v',k,l) => (v,v',k_rewrite state k,l)) vkl_free
	   val pc_free = let val temp = VarMap.listItemsi pc_free
			     fun mapper(v,(v',tr,copt)) = 
				 let val l = Name.internal_label(Name.var2string v)
				 in (v, v', tr, l, 
				     c_rewrite state 
				       (case copt of
					   SOME c => c
				         | _ => Typeof_c (Var_e v)))
				 end
			 in  map mapper temp
			 end

	   val free_vars = map #1 pc_free
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

	   val (internal_subst, code_cbnds, cenv) =
	       let val cbnds = map (fn (_,v',_,l) => Con_b(Runtime, (Con_cb(v',
								Proj_c(Var_c cenv_var, l))))) vkl_free
		   val cenv = Crecord_c(map (fn (v,_,_,l) => (l,c_rewrite state (Var_c v))) vkl_free)
		   val subst = foldl (fn ((v,_,_,l),s) => 
				       NilSubst.C.sim_add s (v,Proj_c(Var_c cenv_var, l)))
				(NilSubst.C.empty()) vkl_free
	       in  (subst, cbnds, cenv)
	       end


 	   val vklist_cl = vklist_rewrite state tFormals
	   val vclist_cl = map (fn (v,_,c) => 
				(if isDependent then SOME v else NONE, c_rewrite state c)) eFormals 
	   val codebody_tipe = c_rewrite state body_type
	   val closure_tipe = AllArrow_c{openness=Closure, effect=effect, isDependent=isDependent,
					 tFormals=vklist_cl,
					 eFormals=vclist_cl,
					 fFormals=TilWord32.fromInt(length fFormals),
					 body_type=codebody_tipe}
	   val codebody_tipe = NilSubst.substConInCon internal_subst codebody_tipe


	   val code_body = e_rewrite inner_state body

           fun is_float (TraceKnown TraceInfo.Notrace_Real) = true
	     | is_float _ = false
	   val boxfloat_type = Prim_c(BoxFloat_c Prim.F64, [])
	   fun box e = Prim_e(NilPrimOp(box_float Prim.F64), [], [e])
	   fun unbox e = Prim_e(NilPrimOp(unbox_float Prim.F64), [], [e])
	   val trace_pointer = TraceKnown TraceInfo.Trace
	   val trace_float = TraceKnown TraceInfo.Notrace_Real

	   val (venv, code_bnds, venv_tr,venv_type) = 
	      (case (!do_single_venv, pc_free) of
		(true, [(v,v',tr,_,t)]) => let val venv = e_rewrite state (Var_e v)
					       val venv = if (is_float tr) then box venv else venv
					       val bnd = if (is_float tr)
							      then Exp_b(v', tr, unbox(Var_e venv_var))
							  else Exp_b(v',tr,Var_e venv_var)
					       val (tr,t) = if (is_float tr) 
								then (trace_pointer,boxfloat_type) 
								else (tr,t)
					   in  (venv, [bnd], tr, t)
					   end
	      | _ => let  val labels = map #4 pc_free
			  fun mapper (v,v',tr,l,t) =
			      let val env_e = e_rewrite state (Var_e v)
				  val code_e = Prim_e(NilPrimOp(select l), [],
						 [Var_e venv_var])
				  val code_tr = trace_rewrite inner_state tr
				  val (env_e,env_tr,env_t) =
				      if (is_float tr) 
					  then (box env_e, trace_pointer, boxfloat_type)
				      else (env_e, trace_rewrite state tr, t)
				  val code_bnds = if (is_float tr)
						      then let val v'' = derived_var v'
							   in  [Exp_b(v'',trace_pointer,code_e),
								Exp_b(v',trace_float,unbox (Var_e v''))]
							   end
						  else [Exp_b(v',code_tr,code_e)]
			      in  (case env_e of 
				       Var_e _ => (env_e, NONE, env_t, code_bnds)
				     | _ => let val v'' = derived_var v
					    in  (Var_e v'', SOME(Exp_b(v'',env_tr,env_e)), env_t, code_bnds)
					    end)
			      end
			  val fields_bndopts_types_codebnds = map mapper pc_free
			  val fields = map #1 fields_bndopts_types_codebnds
			  val bnds = List.mapPartial #2 fields_bndopts_types_codebnds
			  val types = map #3 fields_bndopts_types_codebnds
			  val code_bnds = Listops.flatten(map #4 fields_bndopts_types_codebnds)
			  val venv = makeLetE Sequential bnds 
			      (Prim_e(NilPrimOp(record labels),[], fields))
			  val venv_type = Prim_c(Record_c (labels,NONE), types)
		     in  (venv, code_bnds, TraceKnown TraceInfo.Trace, venv_type)
		     end)


	   fun vtrc_mapper (v,tr,c) = 
	       let val c =  c_rewrite state (NilSubst.substConInCon internal_subst c)
		   val tr = trace_rewrite state (NilSubst.substConInTrace internal_subst tr)
	       in  (v, tr, c)
	       end
	   val vklist_code = (cenv_var,Single_k cenv) :: vklist_cl 
	   val vtrclist_code = map vtrc_mapper ((venv_var,venv_tr,venv_type) :: eFormals)
	   val code_fun = Function{effect=effect,recursive=recursive,isDependent=isDependent,
				   tFormals=vklist_code,
				   eFormals=vtrclist_code,
				   fFormals=fFormals,
				   body = makeLetE Sequential (code_cbnds @ code_bnds) code_body, 
				   body_type=codebody_tipe}

	   val closure = {code = code_var,
			  cenv = cenv,
			  venv = venv,
			  tipe = Var_c fidtype_var}

       in if escape then (is_recur,
			  [(fidtype_var, closure_tipe)],
			  [(code_var,code_fun)],
			  [(v,closure)])
	  else (false,[],[(code_var,code_fun)],[])
       end

   and vklist_rewrite state vklist = map (fn (v,k) => (v,k_rewrite state k)) vklist

   and bnd_rewrite state bnd : bnd list =
       (case bnd of
	  (Con_b(p,cb)) => let val cbnds = cbnd_rewrite state cb
		   	   in  map (fn cb => Con_b(p,cb)) cbnds
			   end
         | (Exp_b(v,niltrace,e)) => [Exp_b(v, trace_rewrite state niltrace, 
						e_rewrite state e)]
         | (Fixclosure_b _) => error "there can't be closures while closure-converting"
	 | (Fixcode_b _) => error "there can't be codes while closure-converting"
	 | (Fixopen_b var_fun_set) => 
	       let 
		   val var_fun_list = Sequence.toList var_fun_set
		   val vars = map #1 var_fun_list
		   val type_pfun_close = map (fun_rewrite state vars) var_fun_list
		   val pfun_type = List.concat(map #2 type_pfun_close)
		   val pfun_typebnds = map (fn (v,c) => (Con_b(Compiletime,Con_cb(v,c)))) pfun_type
		   val pfun_list = List.concat(map #3 type_pfun_close)
		   val pfun_bnd = Fixcode_b (Sequence.fromList pfun_list)
		   fun make_fix (is_recur,[]) = []
		     | make_fix (is_recur,ls) = [Fixclosure_b(is_recur,Sequence.fromList ls)]
		   fun closure_loop recur (group,separate) [] = 
		          (make_fix(false,separate)) @ (make_fix(recur,group))
		     | closure_loop recur (group,separate) ((r,_,_,cl)::rest) = 
			  closure_loop (recur orelse r)
			    (if r then (cl@group,separate) else (group,cl@separate)) rest
		   val closure_bnd_list = closure_loop false ([],[]) type_pfun_close
	       in  pfun_typebnds @ (pfun_bnd :: closure_bnd_list)
	       end)


   and trace_rewrite state trace : niltrace = 
       let fun help c = 
	   let val c = c_rewrite state c
	       val (Var_c v, rev_labs) = extract_cpath c
	       val labs = rev rev_labs
	   in  case labs of
	       [] => TraceCompute v
	     | _ => TraceKnown(TraceInfo.Compute(v,labs))
	   end
       in (case trace of
	       TraceUnknown => trace
	     | TraceKnown (TraceInfo.Compute(v,labs)) => help(path2con(v,labs))
	     | TraceKnown _ => trace
	     | TraceCompute v => help(Var_c v))
       end
		     

   and e_rewrite state arg_exp : exp =
       let 
(*
	   val _ = (print "  e_rewrite called on = \n";
		    pp_exp arg_exp; print "\n")
*)

	   (* there are no function definitions within e_rewrite so we use same state *)
	   val e_rewrite = e_rewrite state
	   val c_rewrite = c_rewrite state

       in
       (case arg_exp of
	    Var_e v =>
	       let val fid = current_fid state
                   val {free_evars,...} = get_frees fid
               in  case VarMap.find(free_evars,v) of
			NONE => arg_exp
		      | SOME(v,_,_) => Var_e v
	       end
	  | Prim_e(NilPrimOp np,clist,elist) => 
	       let val np = (case np of
				 make_vararg(_,e) => make_vararg(Closure,e)
			       | make_onearg(_,e) => make_onearg(Closure,e)
			       | _ => np)
	       in  Prim_e(NilPrimOp np,map c_rewrite clist, map e_rewrite elist)
	       end
	  | Prim_e(p,clist,elist) => 
	       Prim_e(p,map c_rewrite clist, map e_rewrite elist)
	  | Const_e v => arg_exp
	  | Switch_e switch => 
		Switch_e(case switch of 
		     Intsw_e{size,arg,arms,default,result_type} => 
			 Intsw_e{size = size, 
				 arg = e_rewrite arg,
				 arms = map_second e_rewrite arms,
				 default = Util.mapopt e_rewrite default,
				 result_type=c_rewrite result_type}
		   | Sumsw_e{sumtype,arg,bound,arms,default,result_type} => 
			 Sumsw_e{sumtype=c_rewrite sumtype, 
				 arg=e_rewrite arg,
				 bound = bound,
				 arms = map (fn (w,tr,e) => 
						(w, trace_rewrite state tr,
						 e_rewrite e)) arms,
				 default=Util.mapopt e_rewrite default,
				 result_type=c_rewrite result_type}
		   | Exncase_e{arg,arms,bound,default,result_type} => 
			 Exncase_e{arg=e_rewrite arg,
				   bound=bound,
				   arms=map (fn (e,tr,f) => (e_rewrite e,trace_rewrite state tr,e_rewrite f)) arms,
				   default=Util.mapopt e_rewrite default,
				   result_type=c_rewrite result_type}
		   | Typecase_e {arg,arms,default,result_type} =>
			     Typecase_e{arg=c_rewrite arg,
					default=e_rewrite default,
					result_type=c_rewrite result_type,
					arms=map (fn (pc,vklist,e) =>
						  (pc, vklist_rewrite state vklist, e_rewrite e)) arms})
			 
	  | Let_e(letsort,bnds,e) => let val bnds_list = map (bnd_rewrite state) bnds
				     in Let_e(letsort, List.concat bnds_list, e_rewrite e)
				     end
	  | App_e (Closure, _,_,_,_) => error "App_e(Code|Closure,...) during closure conversion"
	  | ExternApp_e (e, elist) => 
		let val elist' = map e_rewrite  elist
		    val e' = e_rewrite e
		in  ExternApp_e(e', elist')
		end
	  | App_e (Code, e, clist, elist, eflist) => 
		let val clist' = map c_rewrite  clist
		    val elist' = map e_rewrite  elist
		    val eflist' = map e_rewrite eflist
		    val e' = e_rewrite e
		in  App_e(Code, e', clist', elist', eflist')
		end
	  | App_e (Open, e, clist, elist, eflist) => 
		let val clist' = map c_rewrite  clist
		    val elist' = map e_rewrite  elist
		    val eflist' = map e_rewrite eflist
		    fun docall (cv,{free_cvars, free_evars} : frees) = 
			let val vk_free = map #1 (VarMap.listItemsi free_cvars)
			    val vc_free = VarMap.listItemsi free_evars
			    val {free_evars,...} = get_frees(current_fid state)
			    val {cenv_var,venv_var,...} = get_static (current_fid state)
			    val clist'' = (Var_c cenv_var) :: clist'
			    val elist'' = (Var_e venv_var) :: elist'
			in App_e(Code, Var_e cv, clist'', elist'', eflist')
			end
		    fun default() = App_e(Closure,e_rewrite e, clist', elist', eflist') 
		in  (case e of
			 Var_e v => if (eq_var(v,current_fid state))
					then let val {code_var,...} = get_static v
					     in  docall(code_var,get_frees v)
					     end
				    else default()
		       | _ => default())
		end
	  | Raise_e (e,c) => Raise_e(e_rewrite e, c_rewrite c)
	  | Handle_e {body,bound,handler,result_type} => 
		Handle_e{body = e_rewrite body, bound = bound,
			 handler = e_rewrite handler,
			 result_type = c_rewrite result_type})
       end

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
			      val (temp, subst) = foldl_acc folder (NilSubst.C.empty()) 
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
							    (fn (v,k,l) => ((l, derived_var v), k))
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
	  | AllArrow_c {openness,effect,isDependent,tFormals,eFormals,fFormals,body_type} =>
		let val tFormals' = map (fn(v,k) => (v,k_rewrite state k)) tFormals
		    val eFormals' = map (fn(v,c) => (v,c_rewrite c)) eFormals
		    val openness' = (case openness of
				   Open => Closure
				 | Code => Code
				 | Closure => error ("AllArrow_c(Closure,...) " ^ 
						     "during closure-conversion"))
		    val body_type' = c_rewrite body_type
		in  AllArrow_c{openness=openness',effect=effect,isDependent=isDependent,
			       tFormals=tFormals',eFormals=eFormals',
			       fFormals=fFormals,body_type=body_type'}
		end
	  | ExternArrow_c (clist,c) => 
		ExternArrow_c(map c_rewrite clist,
			      c_rewrite c)

	  | Let_c (letsort,cbnds,c) => 
		let val cbnds' = List.concat(map (cbnd_rewrite state) cbnds)
		    val c = c_rewrite c
		in  (case (strip_annotate c,cbnds') of
			 (_,[]) => c
		       | (Var_c v, [Con_cb(v',c')]) => if (eq_var(v,v'))
							     then c'
							 else Let_c(letsort,cbnds',c)
		       | _ => Let_c(letsort,cbnds',c))
		end
	  | Typecase_c {arg,arms,default,kind} => 
		Typecase_c{arg = c_rewrite arg,
			   arms = map (fn (pc,vklist,c) => (pc,map (fn (v,k) => (v,k_rewrite state k)) vklist,
							    c_rewrite c)) arms,
			   default = c_rewrite default,
			   kind = k_rewrite state kind}
	  | Crecord_c lclist => Crecord_c(map (fn (l,c) => (l,c_rewrite c)) lclist)
	  | Typeof_c e => Typeof_c(e_rewrite state e)
	  | Proj_c (c,l) => Proj_c(c_rewrite c, l)
	  | Closure_c (c1,c2) => error "should not encounter Closure_c during closure-conversion"
	  | App_c (c,clist) => App_c(c_rewrite c, map c_rewrite clist)
	  | Annotate_c (annot,c) => Annotate_c(annot,c_rewrite c))
       end



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
	   val top_fid = fresh_named_var "top_fid"
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
	   val _ = chat "  Scanning for free variables\n"
	   val state = foldl import_folder state imports
	   val ({free_evars,free_cvars,...},state) = foldl bnd_find_fv (empty_frees,state) bnds
	   fun export_mapper state (ExportValue(l,v)) = e_find_fv (state, empty_frees) (Var_e v)
	     | export_mapper state (ExportType(l,v)) = c_find_fv (state, empty_frees) (Var_c v)
	   val _ = map (export_mapper state) exports

           (* Perform transitive closure computation *)
	   val _ = if (!debug)
		       then (print "Done with e_find_fv\n";
			     print "free is empty: ";
			     print (Bool.toString (VarMap.numItems free_evars = 0 andalso
						   VarMap.numItems free_cvars = 0));
			     print "\n")
		   else ()
	   val _ = chat "  Computing transitive closure of close funs\n"
	   val _ = close_funs(get_fids())
	   val _ = if (!debug)
		       then print "close_mod: Done with close_funs\n"
		   else ()


	   (* Rewrite module *)
	   val initial_state = new_state top_fid
	   val _ = chat "  Rewriting imports, bindings, exports\n"

	   fun import_mapper (ImportValue(l,v,tr,c)) =
	       let val c = c_rewrite initial_state c
		   val tr = trace_rewrite initial_state tr
	       in  ImportValue(l,v,tr,c)
	       end
	     | import_mapper (ImportType(l,v,k)) =
	       let val k = k_rewrite initial_state k
	       in  ImportType(l,v,k)
	       end
	   val imports' = map import_mapper imports

	   fun folder (bnd,acc) = 
	       let val bnds = bnd_rewrite initial_state bnd
	       in  acc @ bnds
	       end
	   val bnds' = foldl folder [] bnds

	   fun export_rewrite (ExportValue(l,v)) = ExportValue(l,v)
	     | export_rewrite (ExportType(l,v)) = ExportType(l,v)
	   val exports' = map export_rewrite exports

	   val _ = reset_table []

       in  MODULE{bnds = bnds', imports = imports', exports = exports'}
       end	   


end



