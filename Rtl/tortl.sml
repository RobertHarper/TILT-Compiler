(*
   Some features of the translator that might not be obvious to the naked eye:

   - The translation proceeds one function at a time.  There is a work list; translating
   each arm of a fixcode binding consists merely of adding the function to the work list.
   Some information about the current function (such as the locations of the arguments and
   return address) is held in global refs (in TortlBase) rather than in the "state"
   argument that is threaded through the translation.

   - The translation is imperative; the function add_instr from TortlBase statefully emits
   an instruction into a data structure (currently a ref list ref) that gets turned into
   an array at the end of translating each function.  Some simple backpatching occurs
   for the arguments of NEEDALLOC and NEEDMUTATE instructions, but mostly the structure of
   the instruction list does not change once it is created.

   - Tortl seems to support Typecase_e (correctness unknown), but not Typecase_c.
   It does not support Ifthenelse_e, but adding rudimentary support for this construct
   would probably not be difficult.  Compilation of switches in general is extremely
   naive, performing linear search in all nontrivial cases.  This could stand to be
   improved.

   - Tortl requires that its input contain no shadowing.  In particular, the mapping from
   variables to "terms" (maintained by add_term and friends in TortlBase) obviously
   cannot allow multiple entries for the same variable.

      joev 8/2002
*)

(*
 toRtl currently relies somewhat on the code being in a-normal form.  In particular,
 it relies on having traceability for almost all terms, which only happens if all
 terms are named.
 -leaf 9/2002
*)

(* (1) This translation relies on the layout of the thread structure which is
       pointed to by the thread pointer.  Check Runtime/thread.h for details.
   (2) Empty records translate to 256, requiring no allocation
   (3) Folds/unfolds translate to 257.
*)

(* A state contains classification information(type/kind),
                    residence information(register/global label),
		    and value information(int/label).
		    Only one of the latter two must be present.
   There is a current state and a global state.
   The latter contains only variables that have to be globally visible,
      This is a subset of variables bound at the top-level (outside any lambda).
      It is a subset because we would like unreferenced top-level variables to reside
      in registers, thus reducing a proliferation of global labels.
      All imported variables are both top-level and global.
      The check for global and top-level variables must also consider exports.
      Currently, we do not make this distinction to avoid a pre-pass.
*)


structure Tortl :> TORTL =
struct

  val diag = ref false
  val debug = Stats.ff("TortlDebug")
  val debug_full_when = Stats.int("TortlDebugFull")
  val _ = debug_full_when := 99999
  val curfun = ref 0
  fun debug_full() = !debug_full_when <= !curfun
  val debug_full_return = ref false
  val debug_full_env = ref false
  val debug_simp = ref false
  val debug_bound = ref false

  val zip = Listops.zip

  val char_con = NilDefs.char_con

  fun msg str = if (!debug) then print str else ()

  (* Module-level declarations *)
  open Nil Rtl TortlBase
  structure TW32 = TilWord32
  structure TW64 = TilWord64

  val w2i = TW32.toInt
  val i2w = TW32.fromInt;

  val fresh_var = Name.fresh_var
  val fresh_named_var = Name.fresh_named_var


    val show_cbnd = Stats.ff("show_cbnd")
    val linkUnitname = "LINKUNIT"
    val localVararg = fresh_named_var "toVararg"
    val localOnearg = fresh_named_var "toOnearg"
    val localSub = fresh_named_var "sub"
    val localVsub = fresh_named_var "vsub"
    val localLen = fresh_named_var "len"
    val localVlen = fresh_named_var "vlen"
    val localUpdate = fresh_named_var "update"
    val localArray = fresh_named_var "array"
    val localVector = fresh_named_var "vector"

    (*
	Name mangling

	If a compilation unit named U exports a name N, then the
	linker sees U.N rather than N.  Note unit names do not contain
	#".".  This mangling is orthogonal to the ML + C + LINK
	external label distinction (handled by msLabel).
     *)

    fun mangle (unitname : string) (name : string) : string = unitname ^ "." ^ name

    (*
	As far as the linker is concerned, there is no reason to
	mangle LOCAL labels.  We use decorateVar at certain key points
	because having the unit name in the label can be useful when
	debugging.
     *)
    fun decorateVar (v : Nil.var) : Rtl.label =
	LOCAL_CODE (mangle (get_unitname()) (Name.var2string v))

    val exncounter_label = C_EXTERN_LABEL "exncounter"
    val error = fn s => (Util.error "tortl.sml" s)

    val con_depth = ref 0
    val exp_depth = ref 0
    val cbnd_depth = ref 0
    fun resetDepth() = (con_depth := 0;
		        exp_depth := 0;
			cbnd_depth := 0)

    val HeapProfile = NONE : int option
    val elim_tail_call = Stats.tt("ElimTailCall")

    (*This value is entirely arbitrary, except that it must be > 256
     * Even this is up for grabs if we change the definition of
     * NilUtil.istaglike to mark Coercion_c as taglike
     *)
    val coercion_term = VALUE(INT 0w257)

   datatype work = FunWork of (state * var * con * function)
                 | ConFunWork of (state * var * (var * kind) list * con)
   local
       val worklist : work list ref = ref nil
       val workcount = ref 0
   in
       fun addWork more = (worklist := more :: (!worklist))
       fun getWork() = (case (!worklist) of
			    [] => NONE
			  | (a::b) => (workcount := (!workcount) + 1;
				       worklist := b; SOME (!workcount, a)))
       fun resetWork() = (curfun := 0; workcount := 0; worklist := [])
   end

  (* Context: is the context around an expression the identity
   context, or not the identity context.   The identity context
   carries the register containing the return address for the
   function with it.*)
   (* This is just used for deciding whether a call is a tail call.  -joev *)
  datatype context = ID of regi | NOTID

  fun xbnd state (bnd : bnd) : state =
      let val state =
      (case bnd of
	      Con_b (phase,cbnd) => (msg "working on con_b\n";
				     ((xcbnd state (phase,cbnd))
				      handle e => (print "Error in Con_b. Here is the convarmap:\n";
						   print_convars state;
						   print "And here are the kinds:\n";
						   print_kinds state;
						   raise e)))
	    | Exp_b (v,t,e) =>
		  let val _ = (msg "working on exp_b "; msg (Name.var2string v); msg "\n")
		      val (term,state) = xexp(state,v,e,t,NOTID)

		      val term =
			  if (is_global v)
			      then (case term of
					LOCATION(REGISTER (const, I (REGI (v, COMPUTE _)))) =>
					    LOCATION(REGISTER (const, I (REGI (v, niltrace2rep state t))))
				      | _ => term)
			  else term
                      (* Inserting bogus constructor since it's never used *)
		  in  add_term (state,v,Prim_c(Exn_c, []),term,SOME e)
		  end
	    | Fixopen_b var_fun_seq => error "no open functions permitted"
	    | Fixcode_b var_fun_seq =>
		  let
		      val _ = msg "working on fixcode_b\n"
		      fun folder (((v,funcon),function),s) =
			  let val _ = add_global v
			  in  add_code (s, v, funcon, decorateVar v)
			  end
		      val var_fun_list = (Sequence.toList var_fun_seq)
		      val state = foldl folder state var_fun_list
		      val s' = promote_maps state
		      val _ = app (fn ((v,c),f) => addWork (FunWork(s',v,c,f))) var_fun_list
		  in state
		  end
	    | Fixclosure_b (is_recur,var_varconexpset) =>
		  let
		      val _ = msg "working on fixclosure_b\n"
		      val var_vcelist = Sequence.toList var_varconexpset
		      val _ = add_instr(ICOMMENT ("allocating " ^
						  (Int.toString (length var_vcelist)) ^ " closures"))
		      val toplevel = istoplevel()
		      fun folder(((v,tipe),{code,cenv,venv}),state) =
			  add_term(state,v,tipe,
				   VALUE(LABEL(LOCAL_DATA(Name.var2string v))),NONE)
		      val state = if toplevel then foldl folder state var_vcelist else state

		      val backpatch = (not toplevel) andalso is_recur

		      fun loadcl (((v,tipe),{code,cenv,venv}),state) =
			  let val _ = incClosure()
			      val (code_lv,state) = xexp (state,fresh_named_var "codereg",
							   Var_e code, Nil.TraceUnknown, NOTID)
			      val (_,con_lv,state) = xcon'(state,fresh_named_var "cenv", cenv)

			      val (exp_lv,state) =
				if backpatch then
				  (VALUE(TAG Rtltags.uninitVal),state)
				else
				  xexp (state,fresh_named_var "venv",venv, Nil.TraceUnknown,NOTID)

			      val vls = [code_lv, con_lv, exp_lv]

			      val (lv,state) =
				if backpatch then
				  TortlRecord.make_record_mutable(state,vls)
				else if toplevel then
				  TortlRecord.make_record_const(state,vls,SOME(LOCAL_DATA(Name.var2string v)))
				else TortlRecord.make_record(state,vls)

			  in  (lv,state)
			  end

		      val (cl_lvs,rec_state) = Listops.foldl_acc loadcl state var_vcelist


		      val rec_state = 
			if (not toplevel) then                  (* If we're not at the top level, then we must*)
			  let                                   (* load the closures into registers *)
			    fun load ((lv,((v,tipe),_)),state) = 
			      let
				val ir = load_ireg_term(lv,NONE)
			      in (ir,add_reg (state,v,tipe,I ir))
			      end
			    val (clregsi,rec_state) = Listops.foldl_acc load rec_state (Listops.zip cl_lvs var_vcelist)
			  in                                      (* If we're backpatching, then we must write the *)
			    if backpatch then                     (* translated venv into the closure, using the register *)
			      let
				fun dowrite (clregi, ((_,tipe),{code,cenv,venv}),s) =
				  let 
				    val (I ir,s) = xexp' (s,fresh_named_var "venv", venv,
							  Nil.TraceUnknown, NOTID)
				    val _ = if (Rtltags.maxRecordLength < 4)
					      then error "the INIT in dowrite of making a closure is wrong\n"
					    else ()
				  (* This write is an INIT because the entire closure is allocated all at once
				   * and so resides in the same generation *)
				  in  (add_instr(STORE32I(REA(clregi,8), ir)); s)
				  end
				val _ = Listops.foldl2 dowrite rec_state (clregsi, var_vcelist)
			      in rec_state
			      end
			    else rec_state                        (* If we're not backpatching, we're done *)
			  end
			else rec_state                            (* If we're at the toplevel, we're done *)

		      val _ = add_instr(ICOMMENT ("done allocating " ^
						  (Int.toString (length var_vcelist)) ^ " closures"))
		      val _ = msg "done with fixclosure_b\n"
		  in  rec_state
		  end)
      in
	  state
      end


  and xcbnd state (phase, cbnd) =
 	let val _ = cbnd_depth := (!cbnd_depth) + 1
	    val _ = if (!debug)
	 	      then (print "xcon_bnd"; print (Int.toString (!cbnd_depth)); print " called";
			    if (debug_full())
			      then (print " on cbnd = \n"; Ppnil.pp_conbnd cbnd)
			      else ();
			    print "\n")
		      else ()
	    val result = xcbnd' state (phase, cbnd)
	    val _ = cbnd_depth := (!cbnd_depth) - 1
 	in  result
	end

  and xcbnd' state (phase, cbnd : conbnd) : state =
      (case cbnd of
	   Con_cb (v,c) =>
		let
		    val (termOpt,state) =
			if (phase = Compiletime)
			    then (NONE, state)
			else
			    let val (_,term,state) = xcon'(state,v,c)
			    in  (SOME term,state)
			    end
		    val state = add_conterm (state,NONE,v,Single_k c,termOpt)

		    val _ =
			if !debug then
			    (print "Var ";
			     Ppnil.pp_var v;
			     print " => ";
			     Ppnil.pp_con(find_con state v);
			     print "\n")
			else ()
		in
		    state
		end
	 | Code_cb (conwork as (name,vklist,c)) =>
		let val funkind = Arrow_k(Code,vklist,Single_k c)
		    val l = decorateVar name
		    val _ = add_global name
		    val state = add_conterm (state,NONE,name,funkind, SOME(VALUE(CODE l)))
		    val _ = if phase = Compiletime
				then ()
			    else addWork (ConFunWork(promote_maps state,name,vklist,c))
		in  state
		end
	 | Open_cb _ => error "Open_cb encountered")

  and xconst (state : state, arg_v : (con,exp) Prim.value)
      : term * state =
      let
	  open Prim
	  open TW64
      in
	  (case arg_v of
	       uint (W64, _) => error "64-bit ints not done"
	     | int (W64, _) => error "64-bit ints not done"
	     | uint (ws, w64) =>
		   let val w32 = TW64.toUnsignedHalf w64
		   in  (VALUE(INT w32), state)
		   end
	     | int (ws, w64) =>
		   let val w32 = TW64.toUnsignedHalf w64
		   in  (VALUE(INT w32), state)
		   end
	      | (float (F64, s)) => (VALUE(REAL (mk_float_data s)), state)
	      | (float (F32, _)) => error "32 bit floats not done"
	      | (vector (c,a)) => let val (vals,state) =
		                          (Array.foldr (fn (e,(vls,state)) =>
							let val (VALUE v,state) = xexp (state,Name.fresh_var(),e,
										 Nil.TraceUnknown,NOTID)
							in  (v::vls,state)
							end)
					   ([],state) a)
				  in   TortlArray.xvector(state,c,vals)
				  end
	      | (intvector (sz,a)) => let val (vals,state) =
		                          (Array.foldr (fn (e,(vls,state)) =>
							let val (VALUE v,state) = xexp (state,Name.fresh_var(),e,
										 Nil.TraceUnknown,NOTID)
							in  (v::vls,state)
							end)
					   ([],state) a)
				  in   TortlArray.xvector(state,Prim_c(Int_c sz,[]),vals)
				  end
	      | (floatvector (sz,a)) => let val (vals,state) =
		                          (Array.foldr (fn (e,(vls,state)) =>
							let val (VALUE v,state) = xexp (state,Name.fresh_var(),e,
										 Nil.TraceUnknown,NOTID)
							in  (v::vls,state)
							end)
					   ([],state) a)
				  in   TortlArray.xvector(state,Prim_c(Float_c sz,[]),vals)
				  end
	      | (array _)  => error "array/vector/refcell constants not implemented"
	      | (intarray _)  => error "array/vector/refcell constants not implemented"
	      | (floatarray _)  => error "array/vector/refcell constants not implemented"
	      | refcell _ => error "array/vector/refcell constants not implemented"
	      | (tag(t,c)) => let val i = TW32.fromInt (Name.tag2int t)
			      in  (VALUE(INT i), state)
			      end)
      end



  and xexp' (state   : state,     (* state of bound variables *)
	     name    : var,       (* Purely for debugging and generation of useful names *)
	     e       : exp,       (* The expression being translated *)
	     trace   : niltrace,  (* The type of the expression being translated *)
	     context              (* The evaluation context this expression is in *)
	     ) : reg * state =
      let val (term, s) = xexp (state,name,e,trace,context)
      in  (load_reg_term(term,NONE), s)
      end

  and xexp_list (state,elist) : term list * state =
      let fun folder(e,state) = xexp(state,fresh_var(),e,Nil.TraceUnknown,NOTID)
      in  Listops.foldl_acc folder state elist
      end

  (* xexp just wraps xexp'' with debugging output.  joev *)
  and xexp (state   : state,     (* state of bound variables *)
	    name    : var,       (* Purely for debugging and generation of useful names *)
	    arg_e   : exp,       (* The expression being translated *)
	    trace   : niltrace,  (* The type of the expression being translated *)
	    context              (* The evaluation context this expression is in *)
	    ) : term * state =
      let
	  val _ = exp_depth := !exp_depth + 1
	  val _ = if (!debug)
		      then (print "xexp ";  print (Int.toString (!exp_depth)); print " called";
			    if (debug_full())
				then (print " on exp = \n";
				      Ppnil.pp_exp arg_e)
			    else ();
			    print "\n")
		  else ()
	  val res = xexp'' (state,name,arg_e,trace,context)
	  val _ = if (!debug)
		      then (print "xexp ";  print (Int.toString (!exp_depth)); print " returned ";
			    if (debug_full())
				then (print " with term = \n";
				      print (term2string (#1 res)))
			    else ();
			    print "\n")
		  else ()
	  val _ = exp_depth := !exp_depth - 1

      in  res
      end

  and xexp''
      (state   : state,     (* state of bound variables *)
       name    : var,       (* Purely for debugging and generation of useful names *)
       arg_e   : exp,       (* The expression being translated *)
       trace   : niltrace,  (* The type of the expression being translated *)
       context              (* The evaluation context this expression is in *)
       ) : term * state =
      let
	  fun pickdesti rep = alloc_named_regi name rep
	  fun pickdestf () = alloc_named_regf name
	  val res =
	  case arg_e of
	      Var_e v => (getrep state v,state)
	    | Const_e v => xconst(state,v)
	    | Let_e (_, [], body) => xexp (state,name,body,trace,context)
	    | Let_e (_, bnds, body) =>
		  let val (first_bnds,last_bnd) = Listops.split bnds
		      fun folder (bnd,s) = xbnd s bnd
		      val state = foldl folder state first_bnds
		  in
		      (case (last_bnd,body) of
			   (Exp_b(v,nt,e),Var_e v') =>
			       if (Name.eq_var(v,v'))
				   then xexp(state,name,e,nt,context)
			       else xexp(xbnd state last_bnd,fresh_var(),body,trace,context)
			 | _ => xexp(xbnd state last_bnd,fresh_var(),body,trace,context))
		  end
	    | Prim_e (NilPrimOp nilprim, trlist,clist, elist) => xnilprim (state,nilprim,trlist,clist,elist,
									  context,trace)
	    | Prim_e (PrimOp prim, _,clist, elist) => xprim(state,prim,clist,elist,context,trace)
	    | Switch_e sw => xswitch (state,name,sw,trace,context)
	    | ExternApp_e (f, elist) => (* there is no environment - not a closure *)
		  let
		      val _ = incApp()
		      val _ = add_instr (ICOMMENT ("making external call"))

		      fun efolder(e,(eregs,fregs,state)) =
			  let val (res,state) = xexp' (state,fresh_named_var "call_e",
							e, Nil.TraceUnknown, NOTID)
			  in  case res of
			      I _ => (res::eregs,fregs,state)
			    | F _ => (eregs,res::fregs,state)
			  end

		      val (rev_iregs, rev_fregs, state) = foldl efolder ([],[],state) elist
		      val iregs = rev rev_iregs
		      val fregs = rev rev_fregs
		      val args = iregs @ fregs

		      val expvar = case f of Var_e v => v | _ => error "Function in an extern app should be a variable."
		      val fun_reglabel =
			case getrep state expvar of
			  VALUE (CODE (clab as (C_EXTERN_LABEL _))) => LABEL' clab
			| LOCATION loc => (print "Bad rep for "; Ppnil.pp_exp f; print "\n"; REG'(load_ireg_loc(loc,NONE)))
			| _ => error "bad varval for function"

		      val (_,dest) = alloc_reg_trace state trace
		      val _ = add_instr(CALL{call_type = C_NORMAL,
					     func = fun_reglabel,
					     args = args,
					     results = [dest],
					     save = getLocals()})
		      val result = (LOCATION(REGISTER (false,dest)), new_gcstate state)
		      val _ = add_instr (ICOMMENT ("done making external call"))

		  in  result
		  end

	    | App_e (openness, f, clist, elist, eflist) => (* assume the environment is passed in first *)
		  let
		      val _ = incApp()
		      val call_type =
			      (case openness of
				    Open => error "no open calls permitted here"
				  | Code => "direct call "
				  | Closure => (if (length elist = 0 andalso length eflist = 0)
						    then "closure polycall"
						else "closure call"))

		      (* Compute all the arguments and get them in registers. *)
		      local
			  fun cfolder (c,state) = xcon(state,fresh_named_var "call_carg", c)
			  fun efolder(e,state) = xexp' (state,fresh_named_var "call_e",
						       e, Nil.TraceUnknown, NOTID)
		      in
			  val (cregsi,state) = Listops.foldl_acc cfolder state clist
			  val (eregs, state) = Listops.foldl_acc efolder state elist
			  val (efregs, state) = Listops.foldl_acc efolder state eflist
		      end

		      val _ = add_instr (ICOMMENT ("making " ^ call_type))

		      (* a. Find out if this is a self call. *)
		      (* b. Ensure that either we know the label of the function code, or have it in a register. *)
		      (* c. If there are additional arguments (i.e. from a closure), get them into registers. *)
		      val (selfcall,fun_reglabel,cregsiCl,eregsCl,state) =
			  (case (openness,f) of
			       (Code,Var_e expvar) =>
				 (* This is a direct call. *)
				 (Name.eq_var(#1(getCurrentFun()), expvar),
				      (case (getrep state expvar) of
					 (VALUE (CODE l)) => LABEL' l
				       | (LOCATION loc) => REG'(load_ireg_loc(loc, NONE))
				       | _ => error "bad varval for function"),
					 [], [],state)
			     | (Code,_) => error "ill-formed application"
			     | (Open,_) => error "no open apps allowed"
			     | (Closure,cl) =>
				   let val (clreg,state) = xexp' (state,fresh_var(),cl,
								 Nil.TraceUnknown,NOTID)
				       val clregi =
					   (case clreg of
						I ir => ir
					      | F _ => error "closure compiled to float reg")
				       val funregi = alloc_named_regi (fresh_named_var "funreg")
					   NOTRACE_CODE
				       val cregi =  alloc_named_regi (fresh_named_var "creg") TRACE
				       val eregi =  alloc_named_regi (fresh_named_var "ereg") TRACE
				       val _ = record_project(clregi, 0, funregi)
				       val _ = record_project(clregi, 1, cregi)
				       val _ = record_project(clregi, 2, eregi)
				   in  (false, REG' funregi, [cregi], [I eregi], state)
				   end)

		      (* Now we really have all the arguments.  Put them in order. *)
		      val args = (map I cregsiCl) @ (map I cregsi) @
				 eregsCl @ eregs @ efregs

		      val (resrep,dest) = alloc_reg_trace state trace
		      val context = if (!elim_tail_call) then context else NOTID
		  in
		      (case (context,selfcall) of
			   (NOTID, _) =>
			     (* This is just a normal call. *)
			       (add_instr(CALL{call_type = ML_NORMAL,
					       func = fun_reglabel,
					       args = args,
					       results = [dest],
					       save = getLocals()});
				add_instr (ICOMMENT ("done making normal call"));
				(LOCATION(REGISTER (false,dest)),
				 new_gcstate state))
			 | (ID r,true) =>
			       (* This is a recursive tailcall.  Move the recursive arguments *)
			       (* into the right registers, and jump back to the beginning.   *)
			       (shuffle_regs(args,getArgs());
				add_instr(BR (getTop()));
				add_instr (ICOMMENT ("done making self tail call"));
				(VALUE (VOID resrep), state))
			 | (ID r,false) =>
			       (* This is a non-recursive tailcall.                           *)
			       (add_instr(CALL{call_type = ML_TAIL r,
					       func = fun_reglabel,
					       args = args,
					       results = [dest],
					       save = []});
				add_instr (ICOMMENT ("done making tail call"));
				(LOCATION(REGISTER (false,dest)),
				 new_gcstate state)))
		  end

	    | ForgetKnown_e (sumcon,which) => (coercion_term,state)
	    | Fold_e (vars,from,to) => (coercion_term,state)
	    | Unfold_e (vars,from,to) => (coercion_term,state)
	    | Coerce_e (coercion,cargs,exp) =>
	      let
		  (* We need to translate the coercion in case it generates       *)
		  (* any code that might diverge.  It would be nice if we         *)
	          (* were guaranteed at this point that that wouldn't happen, but *)
		  (* I'm not certain that we're sure of that.                     *)
                  (* The exception is when the coercion is a variable, in which   *)
		  (* case (a) we're sure that it's a value, and (b) it might be   *)
                  (* missing due to the optimization that leaves coercions out of *)
                  (* closure environments.                               joev     *)
		  (* A-normal form should guarantee that this is always a variable*)
		  (*             -leaf                                            *)
		  val state = 
		    case coercion of 
		      (Var_e _) => state
		    | _ => #2 (xexp (state,fresh_named_var "coercion_app",coercion,
				     Nil.TraceKnown TraceInfo.Notrace_Int,NOTID))
	      in
		  xexp (state,name,exp,trace,NOTID)
	      end

	    | Raise_e (exp, _) =>
		  let val _ = add_instr THROW_EXN
		      val (I except,state) = xexp'(state,name,exp,Nil.TraceKnown TraceInfo.Trace,NOTID)
		      val rep = niltrace2rep state trace
		      val _ = add_instr(MV(except,exnarg))
		      val _ = record_project(exnptr,0,SREGI HANDLER)
		      val _ = record_project(exnptr,1,SREGI STACK)
		      val _ = add_instr(RESTORESP)
		      val _ = add_instr(JMP(SREGI HANDLER,[]))
		  in  (VALUE(VOID rep), state)
		  end

	    | Handle_e {body = exp, bound = exnvar,
			handler = handler_body, ...} =>
		  let
		      (* compute free variables that need to be saved for handler *)

		      local
			  val (free_evars,free_cvars) = NilUtil.freeExpConVarInExnHandler(false, 0, handler_body)
			  (* Include only constructors that have locations *)
			  val cTerms = List.mapPartial
			    (* getconvarrep' instead of getconvarrep, because handler_body might contain some *)
			    (* compile-time convars that were bound outside of the current function.    joev  *)
			                    (fn v => (case (getconvarrep' state v) of
							SOME (SOME (LOCATION loc)) => SOME (LOCATION loc)
						      | _ => NONE))
					    (Name.VarSet.listItems free_cvars)
			  (* Drop the exception variables, and include only locations *)
			  val eTerms = List.mapPartial
			                    (fn v =>
					     if (Name.eq_var(v,exnvar)) then NONE else
					     (case (getrep state v) of
							 (VALUE v) => NONE
						       | (LOCATION l) => SOME(LOCATION l)))
					    (Name.VarSet.listItems free_evars)
			  val terms = eTerms @ cTerms
			  fun folder (term, acc as (ir,it,fr,ft)) =
			      (case term of
				   LOCATION(REGISTER (_,I (r as (REGI _)))) => (r::ir,term::it,fr,ft)
				 | LOCATION(REGISTER (_,I (SREGI _))) => error "SREGI free in handler!!!"
				 | LOCATION(REGISTER (_,F r)) =>  (ir,it,r::fr,term::ft)
				  (* don't need to save globals locations or values *)
				 | LOCATION(GLOBAL _) => acc
				 | VALUE _ => acc)
		      in  val (freeIregs, freeIterms, freeFregs, freeFterms) =
			            foldl folder ([],[],[],[]) terms
		      end


		      val hl = fresh_code_label "exn_handler"
		      val handler_after = fresh_code_label "exn_handler_after"

		      (* --- save the floating point values, if any *)
		      val (fpbase,bstate) =
			  (case freeFterms of
			       [] => (NONE,state)
			     | _ => let val (farray,state) = fparray(state,freeFterms)
				    in  (SOME farray, state)
				    end)

		      (* --- create the record of free variables *)
		      val freeTerms =
			  (case fpbase of
			       NONE => freeIterms
			     | SOME farrayTerm => freeIterms @
				   [LOCATION(REGISTER (false, I farrayTerm))])
		      val (freeRecTerm,bstate) = TortlRecord.make_record(bstate, freeTerms)

		      (* --- create and install exn record --- *)
		      val hlreg = alloc_regi NOTRACE_CODE
		      val _ = add_instr(LADDR(LEA(hl,0),hlreg))
		      val handlerTerm = LOCATION(REGISTER (false, I hlreg))
		      val relStack = alloc_regi NOTRACE_INT
		      val _ = add_instr(LOADSP relStack)
		      val stackptrTerm = LOCATION(REGISTER (false, I relStack))
		      val exnptrTerm = LOCATION(REGISTER (false, I (SREGI EXNSTACK)))
		      (* The ordering of fields in the exnrecord is used by the translation
		         of Raise_e and the global_exnrec of service_*.s *)
		      val (exnRecTerm,bstate) = TortlRecord.make_record(bstate, [handlerTerm, stackptrTerm,
								     freeRecTerm, exnptrTerm])
		      val freeRec = load_ireg_term(exnRecTerm,NONE)
		      val _ = add_instr(MV(freeRec, SREGI EXNSTACK))
		      val _ = add_instr PUSH_EXN

                      (* --- compute the body; restore the exnpointer; branch to after handler *)
		      (* NOTID and not context because we have to remove the exnrecord *)
		      val (reg,bstate) = xexp' (bstate,name,exp,trace,NOTID)
		      val _ = record_project(exnptr,3,exnptr)
		      val _ = add_instr POP_EXN
		      val _ = add_instr(BR handler_after)

		      (* --- now the code for handler --- *)
		      val _ = add_instr (ILABEL hl)
		      val _ = add_instr CATCH_EXN
		                                     (* This translates to updating the stack chain; it
						       relies on stack pointer having been fixed
						       already by Raise_e.
						       This must occur first in the handler.
						     *)

		      (* --- pop the exn record ---- *)
		      val freeRec = alloc_regi TRACE
		      val _ = record_project(exnptr,2,freeRec)
		      val _ = record_project(exnptr,3,exnptr)

		      (* --- restore the int registers ---- *)
		      val _ = let
				  fun f (h :: t,offset) =
					 (record_project(freeRec,offset,h);
					  f (t,offset+1))
				    | f ([],offset) = ()
			      in f ((case fpbase of
					 SOME base => freeIregs @ [base]
				       | _ => freeIregs), 0)
			      end

		      (* --- restore the float registers, if any --- *)
		      val _ = let fun f (base : regi, h :: t,offset) =
			                    (add_instr(LOAD64F(REA(base,offset),h));
					     f (base, t,offset+8))
				    | f (base,nil,offset) = ()
			      in (case fpbase of
				      SOME (base : regi)=> f (base,freeFregs,0)
				    | NONE => ())
			      end

		      val xr = alloc_regi TRACE
		      val _ = add_instr(MV(SREGI EXNARG, xr))
		      val hstate = new_gcstate state
		      val hstate = add_reg (hstate,exnvar,Prim_c(Exn_c,[]),I xr)

                      (* --- compute the handler; move result into same register
                             as result reg of expression; add after label; and fall-through *)

		      val (hreg,hstate) = xexp'(hstate,name,handler_body,trace,context)
(*		      val _ = (case (hreg,reg) of
				   (I hreg,I reg) => add_instr(MV(hreg,reg))
				 | (F hreg,F reg) => add_instr(FMV(hreg,reg))
				 | _ => error "hreg/ireg mismatch in handler")
*)
		      val _ = add_instr (mv(hreg,reg))
		      val _ = add_instr(ILABEL handler_after)

		      val state = join_states[state,bstate,hstate]

		  in
		      (LOCATION(REGISTER (false,reg)), state)
		  end
      in res
      end



      (* zero_one : state * regi * niltrace * exp * exp * context -> term * state *)
      (* A helper for some cases of xswitch.                                      *)
      (* The trick is to notice that for certain args, the comparison and computation
         can be folded into one instruction. *)
      and zero_one (state : state, r : regi,
		    trace : niltrace,
		    zeroexp, oneexp, context) : term * state =
	  let
	      val thenl = fresh_code_label "zero_case"
	      val elsel = fresh_code_label "one_case"
	      val afterl = fresh_code_label "after_zeroone"
	      val _ = add_instr(BCNDSI(NE,r,IMM 0,elsel,false))
	      val _ = add_instr(ILABEL thenl)
	      val (zero,state_zero) = xexp' (state,fresh_named_var "zero_result",
						 zeroexp, trace, context)
	      val (_,dest) = alloc_reg_trace  state trace
	      val _ = add_instr(mv(zero, dest))
	      val _ = add_instr(BR afterl)
	      val _ = add_instr(ILABEL elsel)
	      val (one,state_one) = xexp' (state,fresh_named_var "nonzero_result", oneexp,
					       trace, context)
	      val state = join_states[state_zero,state_one]
	      val _ = add_instr(mv(one,dest))
	      val _ = add_instr(ILABEL afterl)
	  in (LOCATION (REGISTER (false, dest)), state)
	  end


  and xswitch
      (state   : state,
       name    : var,      (* Purely for debugging and generation of useful names *)
       sw      : switch,   (* The switch expression being translated *)
       trace   : niltrace, (* The type of the switch expression *)
       context             (* The evaluation context this expression is in *)
       ) : term * state =
      let
	  val _ = incCase()

	  val swtype = case sw of
	    Intsw_e _ => "intcase"
	  | Exncase_e _ => "exncase"
	  | Typecase_e _ => "typecase"
	  | Sumsw_e _ => "sumcase"
	  | Ifthenelse_e _ => "ifthenelse"
	  val afterl = fresh_code_label ("after_" ^ swtype)

	  val dest = ref NONE
	  fun move r =
	    let val d = case !dest of
	                     SOME d => d
			   | NONE => let val d = #2(alloc_reg_trace  state trace)
				     in dest := SOME d; d
				     end
	    in add_instr(mv(r,d))
	    end

	  fun no_match state =
	      (case (!dest) of
		   SOME _ => let val c = NilDefs.dummy_con (* XXX not really *)
				 val (r,newstate) =
		                      xexp'(state,fresh_var(),Raise_e(NilDefs.internal_match_exn,c),
					    trace, context)
			     in  move r; newstate
			     end
		 | NONE => error "empty switch statement")

	  (* If tag cmp i, branch to label.  Comparison is unsigned. *)
	  fun checku lbl cmp i tag =
	      (if in_imm_range i
		   then add_instr(BCNDUI(cmp,tag,IMM (w2i i),lbl,true))
	       else
		   let val tmp = alloc_regi(NOTRACE_INT)
		   in  add_instr(LI(i,tmp));
		       add_instr(BCNDUI(cmp,tag,REG tmp,lbl,true))
		   end)

	  fun do_arm_body (state,body,trace,context) : state =
	    let val (r,state) = xexp'(state,fresh_var(),body,trace,context)
	    in move r;
	       add_instr(BR afterl);
	       state
	    end

      in
	  case sw of
	      Intsw_e {size, arg, arms, default, ...} =>
		  let val (I r,state) = xexp' (state,fresh_named_var "intsw_arg",arg,
					       Nil.TraceKnown TraceInfo.Notrace_Int,NOTID)
		  in  case (arms,default) of
		      ([(0w0,z)],SOME e) => zero_one(state, r, trace, z, e, context)
		    | ([(0w0,z),(0w1,one)],NONE) => zero_one(state, r, trace, z, one, context)
		    | _ => (* general case *)
			  let
			      fun scan(states,lab,[]) =
				  (add_instr(ILABEL lab);
				   case default of
				       NONE => (no_match state::states)
				     | SOME e =>
					   let val (r,newstate) = (xexp' (state,fresh_var(),e,
									   trace,context))
					   in  move r; newstate::states
					   end)
				| scan(states,lab,(i,body)::rest) =
				  let val next = fresh_code_label "intarm"
				  in  add_instr(ILABEL lab);
				      checku next NE i r;
				      scan( do_arm_body(state,body,trace,context)::states, next, rest )
				  end
			      val new_states = scan([],fresh_code_label "intarm",arms)
			      val state = join_states new_states
			  in
			      add_instr(ILABEL afterl);
			      case !dest of
				  SOME r => (LOCATION(REGISTER (false, r)),state)
				| _ => error "no arms"
			  end
		  end
	    | Exncase_e {arg, bound, arms, default, ...} =>
		  let
		      val (I exnarg,state) = xexp' (state,fresh_named_var "exncase_arg",arg,
						     Nil.TraceKnown TraceInfo.Trace,NOTID)

		      val exntag = alloc_regi(NOTRACE_INT)
		      val _ = record_project(exnarg,0,exntag)
		      fun scan(states,lab,[]) =
			  (add_instr(ILABEL lab);
			   case default of
			       NONE =>
				   let val con = NilDefs.dummy_con  (* XXX not really *)
				       val (r,newstate) = xexp'(state,fresh_var(),
								  Raise_e(arg,con),trace,context)
				   in  move r; newstate :: states
				   end
			     | SOME e =>
				   let val (r,newstate) = xexp' (state,fresh_var(),e,
								  trace,context)
				   in  move r; newstate :: states
				   end)
			| scan(states,lab,(armtag,tr,body)::rest) =
			  let
			      val _ = add_instr(ILABEL lab)
			      val tagcon = type_of state armtag
			      val (I armtagi,state) = xexp'(state,fresh_var(),armtag,
								   Nil.TraceKnown TraceInfo.Notrace_Int,NOTID)
			      val (_,Prim_c(Exntag_c,[c])) = simplify_type state tagcon
			      val next = fresh_code_label "exnarm"
			      val test = alloc_regi(NOTRACE_INT)
			      val (_,carried) = alloc_reg_trace state tr
			      val carriedi = (case carried of
						  I ir => ir
						| _ => error "carried value is an unboxed float")
			      val state = add_reg (state,bound,c,carried)
			  in  add_instr(BCNDSI(NE,exntag,REG armtagi,next,true));
			      record_project(exnarg,1,carriedi);
			      scan( do_arm_body(state,body,trace,context)::states,next,rest )
			  end
		      val states = scan([],fresh_code_label "exnarm",arms)
		      val state = join_states states
		  in
		      add_instr(ILABEL afterl);
		      case !dest of
			  SOME r => (LOCATION(REGISTER (false, r)),state)
			| _ => error "no arms"
		  end
	    | Typecase_e {arg,arms,default,...} =>
		  let
		      val (conr,state) = xcon(state,fresh_named_var "typecase_arg",arg)
		      val defaultl = fresh_code_label "defaultTypecase"
		      val contagr = alloc_regi(NOTRACE_INT)
		      val arityr = alloc_regi(NOTRACE_INT)

		      val _ = add_instr(BCNDUI(LE, conr, IMM 255, defaultl, false))
		      val _ = record_project(conr,0,contagr)
		      val _ = add_instr(BCNDSI(NE, contagr, IMM 5, defaultl, false)) (* skip non-records *)
		      val _ = record_project(conr,1,arityr)

		      fun scan(states,lab,[]) =
			  let val _ = add_instr(ILABEL lab)
			      val _ = add_instr(ILABEL defaultl)
			      val (r,newstate) = xexp' (state,fresh_var(),default,
						       trace,context)
			  in  move r; newstate :: states
			  end
			| scan(states,lab,(Record_c _, vklist,body)::rest) =
			  let
			      val arity = length vklist
			      val next = fresh_code_label "typecasearm"
			      fun loop (pos,[],state) = state
				| loop (pos,(v,k)::rest,state) =
				  let val r = alloc_regi TRACE
				      val _ = record_project(conr,pos,r)
				      val state = add_conterm (state,NONE,v,k,SOME(LOCATION(REGISTER (false,I r))))
				  in  loop (pos+1,rest,state)
				  end
			      val _ =  add_instr(ILABEL lab);
			      val _ = add_instr(BCNDSI(NE,arityr,IMM arity,next,true));
			      val state = loop (2,vklist,state)
			  in scan( do_arm_body(state,body,trace,context)::states, next, rest)
			  end
		      val states = scan([],fresh_code_label "typecasearm",arms)
		      val state = join_states states
		  in
		      add_instr(ILABEL afterl);
		      case !dest of
			  SOME r => (LOCATION(REGISTER (false, r)),state)
			| _ => error "no arms"
		  end
	    | Sumsw_e {sumtype, arg, bound, arms, default,...} =>
	      let val (tagcount,_,cons) = reduce_to_sum "sumsw" state sumtype
		  val totalcount = tagcount + TilWord32.fromInt(length cons)
		  fun spcon i = Prim_c(Sum_c{tagcount=tagcount,
					     totalcount=totalcount,
					     known = SOME i},
				       if (length cons = 1)
					   then cons
				       else [NilDefs.con_tuple cons])
	      in
	       (case (tagcount,cons,arms, default) of
		  (0w2,[], [(0w0,_,zeroexp),(0w1,_,oneexp)], NONE) =>
			let val (I r,state) = xexp'(state,fresh_named_var "intsw_arg",arg,
						    Nil.TraceKnown TraceInfo.Trace,NOTID)
			in  zero_one(state,r, trace, zeroexp, oneexp, context)
			end
		| _ =>
		  let val (I r,state) = xexp'(state,fresh_named_var "sumsw_arg",arg,
						Nil.TraceKnown TraceInfo.Trace,NOTID)
		    val nomatchl = fresh_code_label "nomatch_sum"
		      val one_carrier = (length cons) = 1
		      val total = TW32.uplus(tagcount, i2w(length cons))
		      val exhaustive =
			  let val handled = map (fn (w,_,_) => (w2i w)) arms
			      fun mem i = Listops.member(i,handled)
			  in  List.all mem (Listops.count (w2i total))
			  end
		      val tag = alloc_regi(NOTRACE_INT)
		      fun scan(newstates,lab,[]) =
			  (add_instr(ILABEL lab);
			   if exhaustive
			       then newstates
			   else (add_instr(ILABEL nomatchl);
				case default of
				     NONE => (no_match state)::newstates
				   | SOME e =>
					 let val (r,state) = xexp' (state,fresh_var(),e,
								     trace,context)
					 in  move r; state::newstates
					 end))
			| scan(newstates,lab,(i,tr,body)::rest) =
			  let val next = fresh_code_label "sumarm"
			      val test = alloc_regi(NOTRACE_INT)
			      val _ = add_instr(ILABEL lab)
			      val state =
				  if (TW32.ult(i,tagcount))
				      then state
				  else add_reg (state,bound,spcon i, I r)
			      val check_ptr_done = ref (TW32.equal(tagcount,0w0))
			      val load_tag_done = ref false
			      fun check_ptr() =
				  (if (!check_ptr_done)
				       then ()
				   else (checku nomatchl LE 0w255 r;
					 check_ptr_done := true))
			      fun load_tag() =
				  (if (!load_tag_done)
				       then ()
				   else (record_project(r,0,tag);
					 load_tag_done := true))
			  in
			      (case (exhaustive andalso TW32.equal(TW32.uplus(i,0w1),total),
				     TW32.ult(i,tagcount)) of
				  (true,_) => ()
				| (_,true) => checku next NE i r
				| (_,false) => (if exhaustive then () else check_ptr();
						if one_carrier
						    then ()
						else (load_tag();
						     checku next NE (TW32.uminus(i,tagcount)) tag)));
		              scan( do_arm_body(state,body,trace,context)::newstates, next, rest )
			  end
		      val states = scan([],fresh_code_label "sumarm",arms)
		      val state = join_states states
		  in
		      add_instr(ILABEL afterl);
		      case !dest of
			  SOME r => (LOCATION(REGISTER(false,r)),state)
			| _ => error "no arms"
		  end)
	      end
	    | Ifthenelse_e _ =>
	      error "Ifthenelse_e not supported in Tortl"
      end


  and xnilprim (state : state, nilprim,trlist,clist,elist,context,trace) : term * state =
      let fun error' s = (print "NIL primexpression was:\n";
			  Ppnil.pp_exp (Nil.Prim_e(Nil.NilPrimOp nilprim, trlist,clist,elist));
			  print "\n";
			  error s)
      in
      (case nilprim of
	   Nil.record [] => (TortlRecord.empty_record,state)
	 | Nil.record labels =>
	       let val _ = incRecord()
		   fun folder(e,state) = xexp(state,fresh_var(), e, Nil.TraceUnknown, NOTID)
		   val (terms,state) = Listops.foldl_acc folder state elist
		   val tagword::terms = terms
	       in  TortlRecord.make_record_with_tag(state,tagword,terms)
	       end
         | partialRecord _ => error "partialRecord not implemented"
	 | select label =>
	       let val _ = incSelect()
		   val [e] = elist
		   val (I addr, state) = xexp' (state,fresh_var(),e,Nil.TraceUnknown,NOTID)
		   val reccon = type_of state e
		   val (_,c) = simplify_type state reccon
		   val _ = (case c of
				Prim_c(Record_c labels, fieldcons) => ()
			      | _ => (print "non-record reccon = "; Ppnil.pp_con reccon; print "\n";
				      print "non-record con = "; Ppnil.pp_con c; print "\n"))
		   val Prim_c(Record_c labels, fieldcons) = c
		   fun loop [] _ n = error' "bad select 1"
		     | loop _ [] n = error' "bad select 2"
		     | loop (l1::lrest) (c1::crest) n = if (Name.eq_label(l1,label))
							    then n
							else loop lrest crest (n+1)
		   val which = loop labels fieldcons 0
		   val I desti = #2(alloc_reg_trace state trace)
		   val _ = record_project(addr, which, desti)
	       in  (LOCATION(REGISTER(false, I desti)), state)
	       end
	 | inject_known known =>
		let val _ = incSumInject()
		  fun folder(e,state) = xexp(state,fresh_var(), e, Nil.TraceUnknown, NOTID)
		  val (terms,state) = Listops.foldl_acc folder state elist
		in  TortlSum.xinject_sum_static ((state,known,hd clist),terms,trace)
		end
	 | inject known =>
		(incSumDynInject();
		 case elist of
		    [] => (print "Warning: tortl encountered inject with no argument\n";
			   print "         Converting to inject_known\n";
			   TortlSum.xinject_sum_static ((state,known,hd clist),[],trace))
		  | [e] =>
			let val (e_lv,state) = xexp (state,fresh_var(),e,
						      Nil.TraceUnknown,NOTID)
			    val (_,c_lv,state) = xcon'(state,fresh_var(),hd clist)
			in  TortlSum.xinject_sum_dynamic ((state,known,hd clist),c_lv,e_lv,trace)
			end
		| _ => error "inject_dynamic with more than one argument")
	 | project_known k =>
	       let val _ = incSumProject()
		   val [sumcon] = clist
		   val [e] = elist
		   val (I base,state) = xexp' (state,fresh_var(),e,Nil.TraceUnknown,NOTID)
	       in  TortlSum.xproject_sum_static ((state,k,sumcon),base,trace)
	       end

	 | project k =>
	       let val _ = incSumDynProject()
		   val [sumcon] = clist
		   val [e] = elist
		   val (I base,state) = xexp' (state,fresh_var(),e,Nil.TraceUnknown,NOTID)
		   val (cr,state) = xcon(state,fresh_var(),sumcon)
	       in  TortlSum.xproject_sum_dynamic ((state,k,sumcon),cr,base,trace)
	       end

	 | box_float Prim.F64 =>
	       let val _ = incPrim()
		   val [e] = elist
		   val (lv,state) = xexp(state,fresh_var(),e,Nil.TraceKnown TraceInfo.Notrace_Real,NOTID)
		   val (vl,state) = boxFloat_vl(state,lv)
	       in (vl, state)
	       end
	 | unbox_float Prim.F64 =>
	       let val _ = incPrim()
		   val [e] = elist
		   val (I ir,state) = xexp'(state,fresh_var(),e,Nil.TraceKnown TraceInfo.Trace,NOTID)
		   val fr = unboxFloat ir
	       in (LOCATION(REGISTER(false, F fr)), state)
	       end
	 | box_float Prim.F32 => error "32-bit floats not done"
	 | unbox_float Prim.F32 => error "32-bit floats not done"
	 | make_exntag =>
		   let val _ = incPrim()
		       val _ = (case trace of
				    Nil.TraceKnown TraceInfo.Notrace_Int => ()
				  | _ => error "make_exntag result has funny trace")
		       val desti = alloc_regi NOTRACE_INT
		       val addr = alloc_regi TRACE
		       val tmp = alloc_regi NOTRACE_INT
		       val _ = (add_instr(LADDR(LEA(exncounter_label,0),addr));
				add_instr(LOAD32I(REA(addr,0),desti));
				add_instr(ADD(desti,IMM 1,tmp));
				add_instr(STORE32I(REA(addr,0),tmp)))
		   in  (LOCATION(REGISTER (false, I desti)), state)
		   end
	 | inj_exn name =>
		   let val _ = incPrim()
		       val [e1,e2] = elist
		       val desti = alloc_regi NOTRACE_INT
		       val (vl1,state) = xexp (state,fresh_var(),e1,Nil.TraceUnknown,NOTID)
		       val (vl2,state) = xexp (state,fresh_var(),e2,Nil.TraceUnknown,NOTID)
		       fun char2val c = Const_e(Prim.uint(Prim.W8, TW64.fromInt (ord c)))
		       val name_array = Array.fromList (map char2val (explode name))
		       val (vl3,state) = xconst(state, Prim.vector(char_con,name_array))
		       val terms = [vl1,vl2,vl3]
		   in  TortlRecord.make_record(state,terms)
		   end
	 | make_vararg oe =>
		   let val _ = incVararg()
		       val [c1,c2] = clist
		       val [e] = elist
		       val e = App_e(Closure,Var_e localVararg, [c1,c2], [e], [])
		   in  xexp (state,fresh_var(),e,Nil.TraceKnown TraceInfo.Trace,NOTID)
		   end
	 | make_onearg (openness,eff) =>
		   let val _ = incOnearg()
		       val [c1,c2] = clist
		       val [e] = elist
		       val e = App_e(Closure,Var_e localOnearg, [c1,c2], [e], [])
		   in  xexp (state,fresh_var(),e,Nil.TraceKnown TraceInfo.Trace,NOTID)
		   end
	 | mk_record_gctag => (TortlRecord.record_tag_from_reps (map (niltrace2rep state) trlist),state)

	 | mk_sum_known_gctag =>
	   let val [ksumcon] = clist
	       val (known,sumcon_hnf) =
		   case #2 (simplify_type state ksumcon) of
		       schnf as (Nil.Prim_c (Nil.Sum_c {known=SOME k,...},_)) => (k,schnf)
		     | c => (Ppnil.pp_con c;
			     error "mk_sum_known_gctag called with non-known-sum type")
	       val tr = hd trlist
	   in (TortlSum.make_sum_tag_static ((state,known,sumcon_hnf),tr),state)
	   end)

      end


   and xprim(state, prim, clist, elist, context, trace) : term * state =
      let

	  open Prim
	  fun error' s = (print "nilprimexpression was:\n";
			  Ppnil.pp_exp (Nil.Prim_e(Nil.PrimOp prim, [],clist,elist));
			  print "\n";
			  error s)

	  (* First, rewrite negation as subtraction.
	  *)
	  val (prim,elist) = (case (prim,elist) of
				  (Prim.neg_int is, [e]) => (Prim.minus_int is, [NilDefs.zero_int_exp,e])
				| _ => (prim,elist))

	  val _ = incPrim()
	  fun xtt int_tt = INT_TT
	    | xtt real_tt = REAL_TT
	    | xtt both_tt = BOTH_TT
	  (* ----------- Casts / Identity ----------------------- *)
	  fun identity() =
	      let val (vl_list,state) = xexp_list(state,elist)
		  val [vl] = vl_list
	      in  (vl,state)
	      end
	  (* ----------- integer comparisons ----------------------- *)
          fun stdcmp2i (signed, oper) =
	      let fun flip EQ = EQ
		    | flip LE = GE
		    | flip LT = GT
		    | flip GE = LE
		    | flip GT = LT
		    | flip NE = NE
		  fun swap (a,v1 as VALUE(INT i),v2) =
		      if in_imm_range i
			  then (flip a,v2,v1)
		      else (a,v1,v2)
		    | swap arg = arg
		  val (vl_list,state) = xexp_list(state,elist)
		  val [vl1,vl2] = vl_list
		  val (oper,vl1,vl2) = swap(oper,vl1,vl2)
		  val a' = load_ireg_term(vl1,NONE)
		  val b' = load_ireg_sv vl2
		  val dest = alloc_regi NOTRACE_INT
		  val cmp = if signed then CMPSI else CMPUI
		  val _ =  add_instr(cmp(oper,a',b',dest))
	      in (LOCATION(REGISTER(false, I dest)), state)
	      end

          (* ----------- floating point comparison ---------------- *)
	  fun cmpf oper =
	      let val (vl_list,state) = xexp_list(state,elist)
		  val [vl1,vl2] = vl_list
		  val a' = load_freg_term(vl1,NONE)
		  val b' = load_freg_term(vl2,NONE)
		  val dest = alloc_regi NOTRACE_INT
		  val _ =  add_instr(CMPF(oper,a',b',dest))
	      in (LOCATION(REGISTER(false, I dest)), state)
	      end


	  (* ----------- unary integer operations ----------------- *)
	  fun op1i oper : term * state =
	      let val (vl_list,state) = xexp_list(state,elist)
		  val [vl1] = vl_list
		  val a' = load_ireg_term(vl1,NONE)
		  val dest = alloc_regi NOTRACE_INT
		  val _ = add_instr(oper(a',dest))
	      in (LOCATION(REGISTER(false, I dest)), state)
	      end

	  (* ----------- binary integer operations ----------------- *)
	  fun op2i (comflag, oper) : term * state =
	      let fun commute (v1 as (VALUE(INT i)),v2) =
		         if in_imm_range i then (v2,v1) else (v1,v2)
		    | commute arg = arg
		  val (vl_list,state) = xexp_list(state,elist)
		  val [vl1,vl2] = vl_list
		  (* commute values if the first arg is a small imm *)
		  val (vl1,vl2) = if comflag then commute(vl1,vl2) else (vl1,vl2)
		  val a' = load_ireg_term(vl1,NONE)
		  val b' = load_ireg_sv vl2
		  val dest = alloc_regi NOTRACE_INT
		  val _ = add_instr(oper(a',b',dest))
	      in (LOCATION(REGISTER(false, I dest)), state)
	      end

	  (* ----------- binary and unary float operations ----------------- *)
	  fun op2f oper : term * state =
	      let val (vl_list,state) = xexp_list(state,elist)
	      in  (case vl_list of
		   [vl1,vl2] =>
		       let val a' = load_freg_term(vl1,NONE)
			   val b' = load_freg_term(vl2,NONE)
			   val dest = alloc_regf()
			   val _ = add_instr(oper(a',b',dest))
		       in (LOCATION(REGISTER(false, F dest)), state)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")
	      end
	  fun op1f oper : term * state =
	      let  val (vl_list,state) = xexp_list(state,elist)
	      in  (case vl_list of
		   [vl] =>
		       let val a' = load_freg_term(vl,NONE)
			   val dest = alloc_regf()
			   val _ = add_instr(oper(a',dest))
		       in (LOCATION(REGISTER(false, F dest)), state)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")
	      end
	  (* ----------- Array and vector operations -------------- *)
	  val isLink = (get_unitname()) = linkUnitname
	  fun aggregateApp localVar =
	      xexp (state,fresh_var(),
		   App_e(Code,Var_e localVar,
			 clist, elist, []), trace, context)
	  fun extract_dispatch(t,state,arg,
			       (xfloat,xint,xknown,xdynamic)) =
	      let fun dynamic c =
		  let val (con_ir,state) = xcon(state,fresh_var(),c)
		  in  xdynamic (state,c,con_ir) arg
		  end
	      in
		  (case (t,clist) of
		       (IntArray is,_) => xint (state,is) arg
		     | (IntVector is,_) => xint (state,is) arg
		     | (FloatArray fs,_) => xfloat (state,fs) arg
		     | (FloatVector fs,_) => xfloat (state,fs) arg
		     | (OtherArray true, [c]) => xknown (state) arg
		     | (OtherVector true, [c]) => xknown (state) arg
		     | (OtherArray false, [c]) => dynamic c
		     | (OtherVector false, [c]) => dynamic c
		     | _ => error' "table primitive did not have right type args")
	      end
	  val unit_result = (TortlRecord.empty_record, state)
      in (case prim of
	      soft_vtrap tt => (add_instr(SOFT_VBARRIER(xtt tt)); unit_result)
	    | soft_ztrap tt => (add_instr(SOFT_ZBARRIER(xtt tt)); unit_result)
	    | hard_vtrap tt => (add_instr(HARD_VBARRIER(xtt tt)); unit_result)
	    | hard_ztrap tt => (add_instr(HARD_ZBARRIER(xtt tt)); unit_result)


	    | float2int =>
		  let val (vl_list,state) = xexp_list (state,elist)
		      val [vl] = vl_list
		      val src = load_freg_term(vl,NONE)
		      val dest = alloc_regi NOTRACE_INT
		      val _ = add_instr(CVT_REAL2INT(src,dest))
		  in (LOCATION(REGISTER(false, I dest)), state)
		  end

	    | int2float =>
		  let val (vl_list,state) = xexp_list (state,elist)
		      val [vl] = vl_list
		      val src = load_ireg_term(vl,NONE)
		      val dest = alloc_regf()
		      val _ = add_instr(CVT_INT2REAL(src,dest))
		  in (LOCATION(REGISTER(false, F dest)), state)
		  end

            (* XXX do we want overflow in some cases or are these casts? *)
	    | int2uint _ => identity()
	    | uint2uint _ => identity()
	    | uint2int _ => identity()
	    | int2int _ => identity()

	    | neg_float fs => op1f FNEGD
	    | abs_float fs => op1f FABSD
	    | plus_float fs => op2f FADDD
	    | minus_float fs => op2f FSUBD
	    | mul_float fs => op2f FMULD
	    | div_float fs => op2f FDIVD
	    | less_float fs => cmpf LT
	    | greater_float fs => cmpf GT
	    | lesseq_float fs => cmpf LE
	    | greatereq_float fs => cmpf GE
	    | eq_float fs => cmpf EQ
	    | neq_float fs => cmpf NE

	    | plus_int W32 =>  op2i (true,ADDT)
	    | mul_int W32 =>   op2i (true,MULT)
	    | minus_int W32 => op2i (false,SUBT)
	    | div_int W32 =>   error "ml style div not implemented!"
	    | mod_int W32 =>   error "ml style mod not implemented!"
	    | quot_int W32 =>  op2i (false,DIVT)
	    | rem_int W32 =>   op2i (false,MODT)

	    | plus_uint W32 =>  op2i (true, ADD)
	    | mul_uint W32 =>   op2i (true, MUL)
	    | minus_uint W32 => op2i (false, SUB)
	    | div_uint W32 =>   op2i (false, UDIV)
	    | mod_uint W32 =>   op2i (false, UMOD)

	    (* XXXXX should this mod with 255 *)
	    | plus_uint W8 =>  op2i (true, ADDT)
	    | mul_uint W8 =>   op2i (true, MULT)
	    | minus_uint W8 => op2i (false, SUBT)
	    | div_uint W8 =>   op2i (false, DIVT)
	    | mod_uint W8 =>   op2i (false, MODT)

	    | less_int W32 => stdcmp2i (true, LT)
	    | greater_int W32 => stdcmp2i (true, GT)
	    | lesseq_int W32 => stdcmp2i (true, LE)
	    | greatereq_int W32 => stdcmp2i (true, GE)
	    | eq_int _ => stdcmp2i (false, EQ)
	    | neq_int _ => stdcmp2i (false, NE)

	    (* assume upper bits of 8-bit and 16-bit unsigned ints are zero *)
	    | less_uint _ => stdcmp2i (false, LT)
	    | greater_uint _ => stdcmp2i (false, GT)
	    | lesseq_uint _ => stdcmp2i (false, LE)
	    | greatereq_uint _ => stdcmp2i (false, GE)

	    | neg_int is => error "neg_int should have been compiled away already"
	    | abs_int is => error "abs_int should have been compiled away already"

	    | not_int W64 => op1i NOTB
	    | not_int _ => op1i NOTB
	    | and_int W64 => op2i (true, ANDB)
	    | and_int _ => op2i (true, ANDB)
	    | or_int W64 => op2i (true, ORB)
	    | or_int _ => op2i (true, ORB)
	    | xor_int W64 => op2i (true, XORB)
	    | xor_int _ => op2i (true, XORB)
	    | lshift_int W32 => op2i (false, SLL)
	    | rshift_int W32 => op2i (false, SRA)
	    | rshift_uint W32 => op2i (false, SRL)

	    | (uinta2uinta (is1,is2)) => identity()
	    | (uintv2uintv (is1,is2)) => identity()
	    | (array2vector table) => identity()
	    | (vector2array table) => identity()

	    | (length_table t) =>
		  (case (isLink, t) of
		       (false, OtherArray false) => aggregateApp localLen
		     | (false, OtherVector false) => aggregateApp localLen
		     | _ => let val ([vl],state) = xexp_list (state,elist)
			    in  extract_dispatch(t,state,vl,
						 (TortlArray.xlen_float,
						  TortlArray.xlen_int,
						  TortlArray.xlen_ptr,
						  TortlArray.xlen_dynamic))
			    end)

	     | (sub t) =>
		  (case (isLink, t) of
		       (false, OtherArray false) => aggregateApp localSub
		     | (false, OtherVector false) => aggregateApp localVsub
		     | _ => let val ([vl1,vl2],state) = xexp_list (state,elist)
			    in  extract_dispatch(t,state,(vl1,vl2,trace),
						 (TortlArray.xsub_float,
						  TortlArray.xsub_int,
						  TortlArray.xsub_ptr,
						  TortlArray.xsub_dynamic))
			    end)

	     | (update t) =>
		  (case (isLink, t) of
		       (false, OtherArray false) => aggregateApp localUpdate
		     | (false, OtherVector false) => error "can't update vector"
		     | _ => let val ([vl1,vl2,vl3],state) = xexp_list (state,elist)
			    in  extract_dispatch(t,state,(vl1,vl2,vl3),
						 (TortlArray.xupdate_float,
						  TortlArray.xupdate_int,
						  TortlArray.xupdate_ptr,
						  TortlArray.xupdate_dynamic))
			    end)

	     (* Note that zero is NOT a legal representation for an empty array. *)
	     | (create_empty_table t) =>
		       extract_dispatch(t,state,(VALUE (INT 0w0),NONE),
					(TortlArray.xarray_float,
					 TortlArray.xarray_int,
					 TortlArray.xarray_ptr,
					 TortlArray.xarray_dynamic))
	     | (create_table t) =>
		  (case (isLink, t) of
		       (false, OtherArray false) => aggregateApp localArray
		     | (false, OtherVector false) => aggregateApp localVector
		     | _ => let val ([vl1,vl2],state) = xexp_list(state,elist)
			    in  extract_dispatch(t,state,(vl1,SOME vl2),
						 (TortlArray.xarray_float,
						  TortlArray.xarray_int,
						  TortlArray.xarray_ptr,
						  TortlArray.xarray_dynamic))
			    end)

	     | equal_table t =>
		  let val (vl_list,state) = xexp_list (state,elist)
		      val [vl1,vl2] = vl_list
		      val ir1 = load_ireg_term(vl1,NONE)
		      val ir2 = load_ireg_term(vl2,NONE)
		      val desti = alloc_regi NOTRACE_INT
		      val _ = add_instr(CMPUI(EQ,ir1,REG ir2,desti))
		  in  (LOCATION(REGISTER (false,I desti)), state)
		  end
             | _ => (print "primitive: ";
                       Ppnil.pp_prim prim;
                       print "not implemented\n";
                       raise Util.UNIMP))
      end



   (* ------------------- translate constructors ------------------------ *)

  and xcon arg : regi * state =
      let val (_, term, s) = xcon' arg
      in  (load_ireg_term(term,NONE), s)
      end

  and xcon' (state : state,
	    name : var, (* Purely for debugging and generation of useful names *)
	    arg_con : con     (* The expression being translated *)
	    ) : bool * term * state =
      let
	  val _ = con_depth := !con_depth + 1
	  val _ = if (!debug)
		      then (print "xcon ";  print (Int.toString (!con_depth));
			    print " called";
			    if (debug_full())
				then (print " on con = \n";
				      Ppnil.pp_con arg_con)
			    else ();
			    if (!debug_full_env)
				then (print "\nand with env = \n";
				      show_state state; print "\n")
			    else ();
			    print "\n")
		  else ()
	  val res = xcon''(state,name,arg_con)
	  val _ = if (!debug)
		      then (print "xcon ";  print (Int.toString (!con_depth));
			    print " returned\n")
		  else ()
	  val _ = con_depth := !con_depth - 1
      in  res
      end

  and xcon'' (state : state,
	    name : var, (* Purely for debugging and generation of useful names *)
	    arg_con : con     (* The expression being translated *)
	    ) : bool * term * state =
    (* The boolean result indicates whether this con is "constant". *)
    (* Terms whose representations are statically computed here are constant; *)
    (* representations constructed at run-time can be "constant" if they don't *)
    (* depend on anything non-constant.                                        *)
      let
	  fun mktag i = (VALUE (INT (TW32.fromInt i)))
	  fun mk_ptr  i = (true, mktag i, state)
	  fun mk_sum (state,preTerms,cons) =
	      let fun folder (c,(const,s)) =
		       let val (const',t,s) = xcon'(s,fresh_named_var "xcon_sum",c)
		       in (t,(const andalso const', s))
		       end
		  val (postTerms,(const,state)) = Listops.foldl_acc folder (true,state) cons
		  val terms = preTerms @ postTerms
		  val (res,state) = if const
				       then TortlRecord.make_record_const(state, terms, NONE)
				   else TortlRecord.make_record(state,terms)
	      in (const, res, state)
	      end
	  open Prim
      in
	  (case arg_con of
	       Prim_c(Int_c W8, []) => mk_ptr 0
	     | Prim_c(Int_c W16, []) => mk_ptr 1
	     | Prim_c(Int_c W32, []) => mk_ptr 2
	     | Prim_c(Int_c W64, []) => mk_ptr 3
	     | Prim_c(Float_c F32, []) => mk_ptr 6
	     | Prim_c(Float_c F64, []) => mk_ptr 7
	     | Prim_c(BoxFloat_c F32, []) => mk_ptr 10
	     | Prim_c(BoxFloat_c F64, []) => mk_ptr 11
	     | Prim_c(Exn_c,[]) => mk_ptr 12
	     | Prim_c(Exntag_c,[c]) => mk_sum(state,[mktag 12],[c])
	     | Prim_c(Array_c,[c]) => mk_sum(state,[mktag 0],[c])
	     | Prim_c(Vector_c,[c]) => mk_sum(state,[mktag 1],[c])

	     (* Special arrays and vectors represented the same *)
	     | Prim_c(IntArray_c sz,[]) => mk_sum (state,[mktag 0],[Prim_c(Int_c sz,[])])
	     | Prim_c(IntVector_c sz,[]) => mk_sum (state,[mktag 1],[Prim_c(Int_c sz,[])])
	     | Prim_c(FloatArray_c sz,[]) => mk_sum (state,[mktag 0],[Prim_c(BoxFloat_c sz,[])])
	     | Prim_c(FloatVector_c sz,[]) => mk_sum (state,[mktag 1],[Prim_c(BoxFloat_c sz,[])])

	     | Prim_c (Ref_c,_) => error "Ref is not supported by toRtl.  Forgot to set NilDefaults?"

	     | Prim_c(Loc_c,_) => error "LOC cannot be a constructor"
	     | Prim_c(Sum_c {known,totalcount,tagcount},[c]) =>
		   mk_sum(state,
			   [mktag 4,mktag (case known of
					   NONE => ~1
					 | SOME w => TW32.toInt w),
			    mktag (TW32.toInt tagcount),
			    mktag (TW32.toInt totalcount)],[c])
	     | Prim_c(Sum_c {known,totalcount,tagcount},_) => error "Sum_c does not have 1 type arg"
	     | Prim_c(Record_c _,cons) => mk_sum(state,[mktag 5,mktag(length cons)],cons)
	     | Prim_c(Vararg_c _,cons) => mk_sum(state,[mktag 6],cons)
	     | Prim_c _ => error "ill-formed primitive type"
	     | Mu_c (is_recur,vcset) =>
		   let val (const,lv,state) = mk_sum(state,[mktag 8],[])
		       val num_mu = Sequence.length vcset
		   in  if (num_mu = 1) then (const,lv,state)
		       else let val terms = Listops.map0count (fn _ => lv) num_mu
				val (result,state) =
				    if const (* all mus are base-case/degenerate now *)
					then TortlRecord.make_record_const(state,terms,NONE)
				    else TortlRecord.make_record_const(state,terms,NONE)
			    in  (const,result,state)
			    end
		   end

     (* ----------------------------------------------------------------------------------
        This used to be the code that represented Mu types by creating circular structures.

		   let val vclist = sequence2list vcset
		       fun loop _ [] (s,rev_lvs) = (s,rev rev_lvs)
			 | loop n ((v',c)::vrest) (s,rev_lvs) =
			   let val (lv,k,s) = if is_recur
						then mk_sum(s,[mktag 7,mktag 0],[])
					    else mk_sum(s,[mktag 7],[c])
			   in  loop (n+1) vrest (s,lv::rev_lvs)
			   end
		       val (state,lvs) = loop 0 vclist (state,[])

		       val state =
			   if is_recur
			       then
				   let val clregs = map (fn lv => load_ireg_term(lv,NONE)) lvs
				       fun folder (((v,_),clreg),s) =
						add_convar s (v,SOME(REGISTER (false,I clreg)),
							    NONE,Word_k Runtime,
							    SOME(Word_k Runtime),NONE)
				       val recstate = foldl folder state (zip vclist clregs)
				       fun do_write ((clreg,(v,c)),s) =
					   let val (r,s) = xcon(s,v,c)
					       use record_project abstractrion
					   in  add_instr(MUTATE(EA(clreg,4),r,NONE)); s
					   end
				   in  foldl do_write recstate (zip clregs vclist)
				   end
			       else state
		   in (case lvs of
			   [lv] => (lv, Word_k Runtime, state)
			 | _ => let val kind = kind_tuple (map (fn _ => Word_k Runtime) lvs)
				    val (lv,state) = make_record(state,lvs)
				in  (lv,kind,state)
				end)
		   end
*)
	     | AllArrow_c {openness=Open,...} => error "open Arrow_c"
	     | AllArrow_c {openness=Closure,eFormals=clist,fFormals=numfloat,body_type=c,...} =>
		   mk_sum(state,[mktag 9],[])
	     | AllArrow_c {openness=Code,eFormals=clist,fFormals=numfloat,body_type=c,...} =>
		   mk_sum(state,[mktag 10],[])
	     | ExternArrow_c _ =>
		   mk_sum(state,[mktag 11],[])
	     | Var_c v =>
		   let val (vl,vv) =
		       (case (getconvarrep state v) of
			    SOME (VALUE vv) => (true, VALUE vv)
			  | SOME (LOCATION vl) =>
				let val const = (case vl of
							GLOBAL _ => true
						      | REGISTER (const,_) => const)
				in  (const, LOCATION vl)
				end
			  | NONE => error ("Tried to translate compiletime convar " ^ (Name.var2string v)))
		   in  (vl,vv,state)
		   end
	     | Let_c (letsort, cbnds, c) =>
		   let fun folder (cbnd,s) = xcbnd s (Runtime,cbnd)
		       val state' = foldl folder state cbnds
		       val (vl,vv,state') = xcon'(state',fresh_var(),c)
		   in  (vl,vv,join_states[state,state'])
		   end
	     | Crecord_c lclist =>
		   let fun folder ((l,c),(const,state)) =
			   let val v = fresh_named_var (Name.label2string l)
			       val (const',term,state) = xcon'(state,v,c)
			   in (term, (const andalso const', state))
			   end
		       val (terms,(const,state)) = Listops.foldl_acc folder (true,state) lclist
		       val (result,state) =
			   (case (!do_single_crecord,terms) of
				(true,[term]) => (term, state)
			      | _ =>
				    if const
					then TortlRecord.make_record_const(state,terms,NONE)
				    else TortlRecord.make_record(state,terms))
		   in  (const,result,state)
		   end
	     | Proj_c (c, l) =>
		   let val (const,lv,state) = xcon'(state,fresh_named_var "proj_c",c)
		       val Record_k lvk_seq = std_kind_of state c
		       fun default() =
			   let fun loop [] _ = error "ill-formed projection"
				 | loop (((l',_),k)::vrest) n = if (Name.eq_label(l,l'))
								    then n
								else loop vrest (n+1)
			       val which = loop (Sequence.toList lvk_seq) 0
			       val dest = alloc_regi TRACE
			       val ir = load_ireg_term(lv,NONE)
			       val _ = add_instr(ICOMMENT ("Proj_c at label " ^
							   (Name.label2string l)))
			       val _ = record_project(ir,which,dest)
			   in (const,LOCATION(REGISTER (const,I dest)), state)
			   end
		   in  (case (!do_single_crecord,Sequence.toList lvk_seq) of
			    (true,[(_,k)]) => (const,lv,state)
			  | _ => default())
		   end
	     | Closure_c (c1,c2) =>
		   let
		       fun kinder [(_,Arrow_k(_,vklist : (var * kind) list, k)),_] =
			   let val vklist' = Listops.butlast vklist
			   in  Arrow_k(Closure,vklist',k)
			   end
			 | kinder _ = error "bad Closure_c"
		       val (_,codeTerm,state) = xcon'(state,fresh_named_var "xcon_code",c1)
		   in  mk_sum(state,[codeTerm],[c2])
		   end
	     | App_c (c,clist) => (* pass in env argument first *)
		   let val _ = incApp()
		       val _ = add_instr(ICOMMENT "start making constructor call")
		       val (const_fun,lv,state) = xcon'(state,fresh_named_var "closure",c)
		       val clregi = load_ireg_term(lv,NONE)
		       val (cregsi,(const_arg,state)) =
			   Listops.foldl_acc  (fn (c,(const,state)) =>
				   let val (const',vl,state) = xcon'(state,fresh_named_var "clos_arg",c)
				   in  (load_ireg_term(vl,NONE),(const andalso const', state))
				   end) (true,state) clist
		       val const = const_fun andalso const_arg
		       val coderegi = alloc_regi NOTRACE_CODE
		       val envregi = alloc_regi TRACE
		       val _ = record_project(clregi,0,coderegi)
		       val _ = record_project(clregi,1,envregi)
		       val desti = alloc_regi TRACE
		       val _ = add_instr(CALL{call_type = ML_NORMAL,
					      func = REG' coderegi,
					      args = map I (envregi :: cregsi),
					      results = [I desti],
					      save = getLocals()})
		       val state = new_gcstate state
		       val _ = add_instr(ICOMMENT "done making constructor call")
		   in (const,LOCATION (REGISTER (const,I desti)),state)
		   end
	     | Coercion_c {vars,from,to} =>
		   error "Coercion types are not supposed to be constructors.")
      end



  local
      fun doconfun is_top (state,vname,vklist,body) =
	  let
	      val name = (case (Name.VarMap.find(!exports,vname)) of
			      NONE => decorateVar vname
			    | SOME [] => error "export has no labels"
			    | SOME (l::_) => l)
	      val _ = incFun()
	      val _ = reset_state(is_top, (vname, name))
	      val _ = if (!debug)
			  then (print "-----doconfun on "; print (Pprtl.label2s name);
				if (debug_full())
				    then (print " with body\n"; Ppnil.pp_con body)
				else ();
				print "\n")
		      else ()
              fun folder ((v,k),s) =
			let val r = alloc_named_regi v TRACE
			    val s' = add_conterm (s,NONE,v,k,SOME(LOCATION(REGISTER (false,I r))))
			in  (r,s')
                        end
	      val (cargs,state) = Listops.foldl_acc folder state vklist
	      val args = map I cargs
	      val return = alloc_regi NOTRACE_CODE
	      val _ = set_args(args, return)
	      val state = needalloc(state,IMM 0)
	      val (ir,state) = xcon(state,fresh_named_var "result",body)
	      val result = getResult(fn() => I ir)
	      val I resulti = result
	      val _ = (add_instr(MV(ir,resulti));
		       add_instr(RETURN return))
	      val p = get_proc()
	  in add_proc p
	  end
     fun dofun_help is_top (state,vname, c,
			    Function{tFormals=vklist,
				     eFormals=vclist,
				     fFormals=vflist,body,...}) =
	  let
	      val name = decorateVar vname
	      val _ = reset_state(is_top, (vname, name))
	      val _ = incFun()
	      val _ = msg ("-----dofun_help : " ^ (Pprtl.label2s name) ^ "\n")

	      val {tFormals = tFa, eFormals = eFa, ...} = reduce_to_arrow state c

              fun folder ((v,(_,k)),s) =
		  let val r = alloc_named_regi v TRACE
		      val s' = add_conterm (s,NONE,v,k,SOME(LOCATION(REGISTER (false,I r))))
		  in  (r,s')
		  end

	      val (cargs,state) = Listops.foldl_acc folder state (zip vklist tFa)

              fun folder (((v,tr),c),s) = let val ir = alloc_named_regi v (niltrace2rep s tr)
					 val s' = add_reg (s,v,c,I ir)
			             in  (ir, s')
                                     end

	      val (eiargs,state) = Listops.foldl_acc folder state (zip vclist eFa)

              fun folder (v,s) = let val fr = alloc_named_regf v
					       val s' = add_reg (s,v,Prim_c(Float_c Prim.F64,[]), F fr)
					   in  (fr,s')
                                           end
              val (efargs,state) = Listops.foldl_acc folder state vflist

	      val args = (map I cargs) @ (map I eiargs) @ (map F efargs)
	      val return = alloc_regi NOTRACE_CODE
	      val _ = set_args(args, return)
	      val state = needalloc(state,IMM 0)
	      val (r,state) = xexp' (state,fresh_named_var "result",body,
				      Nil.TraceUnknown, ID return)
	      val result = getResult(fn() => r)
	      val results = (case result of    (* XXX Not used joev *)
				 I ir => ([ir],[])
			       | F fr => ([],[fr]))
	      val _ = (add_instr(mv(r,result));
		       add_instr(RETURN return))
	      val p = get_proc()
	  in  p
	  end
     fun dofun arg = add_proc(dofun_help false arg)
  in
      fun dofun_top arg = dofun_help true arg
      fun worklist_loop () =
	  (case getWork() of
	       NONE => ()
	     | SOME (n,FunWork vf) =>
		   let val _ = curfun := n
		       val temp = "function " ^ (Int.toString n) ^ ": " ^ (Name.var2name (#2 vf))
		       val _ = (msg "*** Working on "; msg temp; msg "\n")
		       val _ = dofun vf
		       val _ = (msg "*** Finished "; msg temp; msg "\n")
		   in  worklist_loop()
		   end
	     | SOME (n,ConFunWork vvkc) =>
		   let val _ = curfun := n
		       val temp = "con_function " ^ (Int.toString n) ^ ": " ^ (Name.var2name (#2 vvkc))
		       val _ = (msg "*** Working on "; msg temp; msg "\n")
		       val _ = doconfun false vvkc
		       val _ = (msg "*** Finished "; msg temp; msg "\n")
		   in  worklist_loop()
		   end)
  end


  (* Find all top-level variables that must be located globally.
     This includes exported variables, imported variables,
     code function varibles, and variables bound at the top-level
     but used not at the top-level.  If a term variable is global,
     then the variable inside its COMPUTE trace (if present)
     is also global.
  *)
  fun computeGlobals(imports, bnds, exports) =
      let val traceMap = ref (Name.VarMap.empty : var Name.VarMap.map)
	  val potentialGlobals = ref (Name.VarMap.empty : bool ref Name.VarMap.map)

	  (* First do the imports and exports *)
	  fun scanImport (ImportValue(l,v,_,_),s) = Name.VarSet.add(s,v)
	    | scanImport (ImportType(l,v,_),s) = Name.VarSet.add(s,v)
	    | scanImport (ImportBnd (phase, cb),s) =
	      let
		  val v =
		      case cb of
			  Con_cb(v,_) => v
			| Open_cb(v,_,_) => v
			| Code_cb(v,_,_) => v
	      in
		  Name.VarSet.add(s,v)
	      end
	  fun scanExport (ExportValue(l,v),s) = Name.VarSet.add(s,v)
	    | scanExport (ExportType(l,v),s) = Name.VarSet.add(s,v)
	  val fvs = foldl scanImport Name.VarSet.empty imports
	  val fvs = foldl scanExport fvs exports
	  fun addVar(v,tr) =
	      let val _ = potentialGlobals := Name.VarMap.insert(!potentialGlobals, v, ref false)
		  val vopt = (case tr of
				  TraceCompute cv => SOME cv
				| TraceKnown (TraceInfo.Compute (cv, _)) => SOME cv
				| _ => NONE)
	      in  (case vopt of
		       NONE => ()
		     | SOME cv => traceMap := Name.VarMap.insert(!traceMap, v, cv))
	      end
	  fun useVar (level,v) =
	      (case (level > 0, Name.VarMap.find(!potentialGlobals,v)) of
		   (true, SOME r) => r := true
		 | _ => ())
	  val ptrTrace = TraceKnown TraceInfo.Trace
	  val NOCHANGE = NilUtil.NOCHANGE
	  fun expHandler({level, ...}:NilUtil.bound, Var_e v) = (useVar(level,v); NOCHANGE)
	    | expHandler _ = NOCHANGE
	  fun conHandler({level, isConstr=true,...}:NilUtil.bound, Var_c v) = (useVar(level,v); NOCHANGE)
	    | conHandler _ = NOCHANGE
	  fun bndHandler({level=0,...}:NilUtil.bound, Exp_b (v, tr, _)) = (addVar(v,tr); NOCHANGE)
	    | bndHandler(_, Fixcode_b vfSeq) = (app (fn ((v,_),_) => addVar(v,ptrTrace))
						(Sequence.toList vfSeq); NOCHANGE)
	    | bndHandler({level=0,...}, Fixclosure_b (_,vclSeq)) = (app (fn ((v,_),_) => addVar(v,ptrTrace))
						(Sequence.toList vclSeq); NOCHANGE)
	    | bndHandler _ = NOCHANGE
	  fun cbndHandler({level=0,isConstr=true,...}:NilUtil.bound,
			  Con_cb(v, _)) = (addVar(v,ptrTrace); NOCHANGE)
	    | cbndHandler(_, Code_cb(v, _, _)) = (addVar(v,ptrTrace); NOCHANGE)
	    | cbndHandler _ = NOCHANGE
	  val handlers = {exphandler = expHandler,
			  bndhandler = bndHandler,
			  conhandler = conHandler,
			  cbndhandler = cbndHandler,
			  kindhandler = NilUtil.default_kindhandler}
	  val _ = map (NilUtil.bnd_rewrite handlers) bnds
	  fun folder (v, ref false, fvs) = fvs
	    | folder (v, ref true, fvs) = let val fvs = Name.VarSet.add(fvs,v)
					  in  (case Name.VarMap.find(!traceMap, v) of
						   NONE => fvs
						 | SOME cv => Name.VarSet.add(fvs,cv))
					  end
	  val globals = Name.VarMap.foldli folder fvs (!potentialGlobals)
      in  globals
      end

   fun mkentry (unitname : string) : entry =
       let fun base s = LINK_EXTERN_LABEL (mangle unitname s)
       in
	   {main               = base "main",
	    global_start       = base "globalstart",
	    global_end         = base "globalend",
	    gc_table           = base "gctable",
	    trace_global_start = base "traceglobalstart",
	    trace_global_end   = base "traceglobalend"}
       end

   fun translate' (unitname : string, ximport : Nil.label -> Rtl.label,
		   imports : Nil.import_entry list,
		   bnds : Nil.bnd list,
		   exports : Nil.export_entry list,
		   parms : Name.LabelSet.set) : Rtl.module =
         let
	     val trueGlobals = computeGlobals(imports, bnds, exports)

	     (* Create named exports for setting global state *)
	     local
		 val help = ML_EXTERN_LABEL o Name.label2string
		 fun mapper (ExportValue(l,v)) = (v, help l)
		   | mapper (ExportType(l,v)) = (v, help l)
	     in  val named_exports = map mapper exports
	     end

             (* Set global state and reset debugging info *)
	     val entry = mkentry unitname
	     val closureLabel = LINK_EXTERN_LABEL(mangle unitname "closure")
	     val mainCodeLabel = LINK_EXTERN_LABEL(mangle unitname "code")
	     val mainCodeVar = Name.fresh_named_var "code"
	     val _ = set_global_state (unitname,
				       (mainCodeVar,mainCodeLabel)::named_exports,
				       trueGlobals)
	     val _ = resetDepth()
	     val _ = resetWork()
	     val exp = Let_e(Sequential, bnds, NilDefs.unit_exp)
	     val con = NilDefs.unit_con

	     val _ = reset_state(true, (mainCodeVar,mainCodeLabel))
	     fun folder (ImportValue(l,v,tr,c),s) =
		 let (* For extern/C functions:
		        (1) The label IS the code value rather than an address
			    containing the code value.
			(2) The label should be mangled differently from ML values.
			*)
		     val lv =
			 (case #2 (simplify_type s c) of
			      ExternArrow_c _ =>
				  let val clab = C_EXTERN_LABEL(Name.label2string l)
				  in  VALUE(CODE clab)
				  end
			    | _ =>
				  let val mllab = ximport l
				      val rep = niltrace2rep s tr
				  in  LOCATION(GLOBAL(mllab,rep))
				  end)
		 in  add_term(s,v,c,lv,NONE)
		 end
	       | folder (ImportType(l,v,k),s) =
		 let val vl = GLOBAL(ximport l,TRACE)
		 in  add_conterm (s,SOME l,v,k, SOME(LOCATION vl))
		 end
	       | folder (ImportBnd (phase, cb), s) =
		 xcbnd s (phase, cb)

	     val localMirrorArray = load_ireg_val(INT (if (!TortlArray.mirrorPtrArray) then 0w1 else 0w0), NONE)
	     val _ = add_instr(CALL{call_type = C_NORMAL,
				    func = LABEL' (C_EXTERN_LABEL "AssertMirrorPtrArray"),
				    args = [I localMirrorArray],
				    results = [],
				    save = getLocals()})
	     val state = needalloc(make_state(),IMM 0)
	     val state = foldl folder state imports
	     val return = alloc_regi NOTRACE_CODE
	     val args = []
	     val _ = set_args(args, return)
	     val (r,state) = xexp' (state,fresh_named_var "result",exp,
				   Nil.TraceUnknown, ID return)
	     val result = getResult(fn() => r)
	     val _ = (add_instr (mv(r,result));
		      add_instr(RETURN return))
	     val p = get_proc()
	     val _ = add_proc p

	     val _ = msg "tortl - translating functions now\n"
	     val _ = worklist_loop()

	     val _ = add_data(COMMENT("Module closure"))
	     val {dynamic,static=moduleClosureTag} = Rtltags.recordtag [NOTRACE_CODE, TRACE, TRACE]
	     val {dynamic,static=globalTag} = Rtltags.recordtag [TRACE, TRACE] (* Doubled for replicaGlobals *)
	     val _ = add_data(INT32(moduleClosureTag))
	     val _ = add_data(DLABEL(closureLabel))
	     val _ = add_data(DATA(mainCodeLabel))
	     val _ = add_data(INT32(0w0))
	     val _ = add_data(INT32(0w0))

	     val _ = add_data(INT32(globalTag))
	     val _ = add_data(DLABEL(#main entry))
	     val _ = add_data(DATA(closureLabel))
	     val _ = add_data(DATA(closureLabel))

	     val procs = rev (!pl)
	     val data = rev (!dl)

	     (* We want to eliminate global_end in favor of a sentinal
	        or explicit count.  The solaris linker inserts padding
	        that could, in principle, confuse the runtime. *)

	     val data = (DLABEL (#global_start entry)) ::
		        (data @
			 [DLABEL (#global_end entry),
			  INT32 0w0])
	     val {partialRecordLabels,
		  partialRecords, totalRecords,
		  partialFields, totalFields} = get_static_records()
	     val module = Rtl.MODULE {procs = procs,
				      data = data,
				      entry = entry,
				      global = partialRecordLabels,
				      parms = parms}

	     val _ = resetDepth()
	     val _ = resetWork()
	     val _ = unset_global_state ()

	 in module
	 end

    local
	val {vararg,
	     onearg,
	     bnds = vararg_onearg_bnds} = Vararg.generate()
	val {sub,vsub,len,vlen,update,array,vector,
	     bnds = aggregate_bnds} = Optimize.generate()

	type info = (Nil.label * Nil.var * Nil.niltrace * Nil.con) * Nil.var
	val linkunit : info list = [(vararg, localVararg),
				    (onearg, localOnearg),
				    (sub, localSub),
				    (vsub, localVsub),
				    (len, localLen),
				    (vlen, localVlen),
				    (update, localUpdate),
				    (array, localArray),
				    (vector, localVector)]
	fun makeImport (((l,_,tr,c),v) : info) = Nil.ImportValue(l,v,tr,c)
	fun makeExport (((l,v,_,_),_) : info) = Nil.ExportValue(l,v)
    in
	val linkUnitbnds    : Nil.bnd list          = aggregate_bnds @ vararg_onearg_bnds
	val linkUnitimports : Nil.import_entry list = map makeImport linkunit
	val linkUnitexports : Nil.export_entry list = map makeExport linkunit
    end

     fun translate (unitname : string, module : Nil.module) : Rtl.module =
	 let val Nil.MODULE {bnds,imports,exports} = module

	   fun need_unit l = 
	     let val l = if Name.is_flat l then hd (Name.split_label l) else l
	     in if Name.is_unit l then SOME l
		else NONE (* extern *)
	     end

	     fun mapper (import : Nil.import_entry) : Name.label option =
		 (case import
		    of ImportValue (l,_,_,_) => need_unit l
		     | ImportType (l,_,_) => need_unit l
		     | ImportBnd _ => NONE)
	     val unitlist = List.mapPartial mapper imports
	     val parms = Name.LabelSet.addList (Name.LabelSet.empty, unitlist)
	     val imports = linkUnitimports @ imports
	     val ximport : Nil.label -> Rtl.label =
		 ML_EXTERN_LABEL o Name.label2string
	 in  translate' (unitname, ximport, imports, bnds, exports, parms)
	 end

     fun linkUnit (entries : entry list) : Rtl.module =
	 let
	     open Nil
	     type acc = import_entry list * bnd list * Rtl.label Name.LabelMap.map
	     fun importUnit (entry : entry, (imports, bnds, map) : acc) : acc =
		 let
		     val lab = Name.fresh_internal_label "import"
		     val v1 = Name.fresh_var()
		     val v3 = Name.fresh_var()
		     val c = AllArrow_c {openness = Closure, effect = Partial,
					 tFormals = [], eFormals = [], fFormals = 0w0,
					 body_type = Prim_c(Record_c [], [])}
		     val nt = TraceKnown TraceInfo.Trace
		     val import = ImportValue (lab, v1, nt, c)
		     val bnd = Exp_b(v3, nt, App_e(Closure, Var_e v1, [], [], []))
		     val map = Name.LabelMap.insert (map, lab, #main entry)
		 in  (import :: imports,
		      bnd :: bnds,
		      map)
		 end
	     val (imports, bnds, map) = foldr importUnit (nil, nil, Name.LabelMap.empty) entries
	     val bnds = linkUnitbnds @ bnds
	     fun ximport (l : Nil.label) : Rtl.label =
		 (case Name.LabelMap.find (map, l)
		    of NONE => error "unknown linkunit label"
		     | SOME l => l)
	     val parms = Name.LabelSet.empty
	 in
	     translate'(linkUnitname,ximport,imports,bnds,linkUnitexports,parms)
	 end

     fun entryTables (unitnames : string list) : Rtl.module =
	 let
	     val entries : entry list = map mkentry unitnames
	     val Rtl.MODULE{procs,data=linkData,
			    entry,global,parms} = linkUnit entries
	     val entries = (mkentry linkUnitname) :: entries

	     fun mktable (name : string, proj : entry -> Rtl.label) : Rtl.data list =
		 DLABEL (LINK_EXTERN_LABEL name) ::
		 map (DATA o proj) entries

	     val data = List.concat[linkData,
				    [DLABEL (LINK_EXTERN_LABEL "modulecount"),
				     INT32 (TilWord32.fromInt (length entries))],
				    mktable("gctable", #gc_table),
				    mktable("globalstart", #global_start),
				    mktable("globalend", #global_end),
				    mktable("traceglobalstart", #trace_global_start),
				    mktable("traceglobalend", #trace_global_end)]

	 in  Rtl.MODULE{procs=procs, data=data,
			entry=entry, global=global, parms=parms}
         end

end

