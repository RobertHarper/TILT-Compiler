(*$import TortlVararg Rtl Pprtl TortlSum TortlArray TortlBase Rtltags Nil NilUtil Ppnil Stats TraceOps NilContext TORTL *)

(* (1) This translation relies on the layout of the thread structure which is
       pointed to by the thread pointer.  Check Runtime/thread.h for details.
   (2) Empty records translate to 256, requiring no allocation 
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
      
  fun msg str = if (!debug) then print str else ()

  (* Module-level declarations *)
  open Util Listops Name
  open Nil NilUtil
  open Rtl Rtltags Pprtl TortlBase

  val maxsp_disp = 8 * 32 + 8 * 32
  val notinml_disp = 8 * 32 + 8 * 32 + 8 + 8 + 8


    val show_cbnd = Stats.ff("show_cbnd")

    val exncounter_label = ML_EXTERN_LABEL "exncounter"
    val error = fn s => (Util.error "tortl.sml" s)
    structure TW32 = TilWord32
    structure TW64 = TilWord64
    val w2i = TW32.toInt
    val i2w = TW32.fromInt;

    val con_depth = ref 0
    val exp_depth = ref 0
    val cbnd_depth = ref 0
    fun resetDepth() = (con_depth := 0;
		        exp_depth := 0;
			cbnd_depth := 0)

      
   type translate_params = { HeapProfile : int option, do_write_list : bool, 
                             codeAlign : Rtl.align, FullConditionalBranch : bool, 
                             elim_tail_call : bool, recognize_constants : bool }
   val cur_params = ref { HeapProfile = NONE : int option, 
			 do_write_list = true,
			 codeAlign = Rtl.OCTA, FullConditionalBranch = false, 
			 elim_tail_call = true, recognize_constants = true}


   datatype work = FunWork of (state * var * function)
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
  
  datatype context = ID of regi | NOTID
   
  fun xbnd state (bnd : bnd) : state =
      (case bnd of
	      Con_b (phase,cbnd) => xcbnd state (phase,cbnd)
	    | Exp_b (v,t,e) => 
		  let val _ = (msg "working on exp_b "; msg (var2string v); msg "\n")
		      val (term,state) = xexp(state,v,e,t,NOTID)
		  in  if istoplevel()
			  then add_global_equation (state,v,e,term)
		      else add_term_equation (state,v,e,term)
		  end
	    | Fixopen_b var_fun_seq => error "no open functions permitted"
	    | Fixcode_b var_fun_seq =>
		  let 
		      val _ = msg "working on fixopen_b\n"
		      fun folder ((v,function),s) =
			  let val funcon = NilUtil.function_type Code function
			  in  add_code (s, v, funcon, LOCAL_CODE (Name.var2string v))
			  end
		      val var_fun_list = (Sequence.toList var_fun_seq)
		      val state = foldl folder state var_fun_list
		      val s' = promote_maps state
		      val _ = app (fn (v,f) => addWork (FunWork(s',v,f))) var_fun_list
		  in state
		  end
	    | Fixclosure_b (is_recur,var_varconexpset) => 
		  let 
		      val _ = msg "working on fixclosure_b\n"
		      val var_vcelist = Sequence.toList var_varconexpset
		      val _ = add_instr(ICOMMENT ("allocating " ^ 
						  (Int.toString (length var_vcelist)) ^ " closures"))
		      val toplevel = istoplevel()
		      fun folder((v,{code,cenv,venv,tipe}),state) = 
			  add_global(state,v,tipe,VALUE(LABEL(LOCAL_DATA(Name.var2string v))))
		      val state = if toplevel then foldl folder state var_vcelist else state
		      fun loadcl ((v,{code,cenv,venv,tipe}),state) = 
			  let val (code_lv,state) = xexp(state,fresh_named_var "codereg",
							   Var_e code, Nil.TraceUnknown, NOTID)
			      val (_,con_lv,state) = xcon'(state,fresh_named_var "cenv", cenv)
			      val (exp_lv,state) = 
				  (case (toplevel,is_recur) of
				       (true,_) => xexp(state,fresh_named_var "venv",venv,
							Nil.TraceUnknown,NOTID)
				     | (_,true) => (VALUE(TAG uninit_val),state)
				     | (_,false) => xexp(state,fresh_named_var "venv",venv,
							 Nil.TraceUnknown,NOTID))
			      val vls = [code_lv, con_lv, exp_lv]
			      val reps = [NOTRACE_CODE, TRACE, TRACE]
			      val (lv,state) = 
				  (case (toplevel,is_recur) of
				       (true,_) => make_record_const(state,NONE,reps,vls,
								     SOME(LOCAL_DATA(Name.var2string v)))
				     | (_,true) => make_record_mutable(state,NONE,reps,vls)
				     | (_,false) => make_record(state,NONE,reps,vls))
			      val ir = load_ireg_term(lv,NONE)
			      val s' = if toplevel then state else add_reg (state,v,tipe,I ir)
			  in  (ir,s')
			  end
		      val (clregsi,rec_state) = foldl_list loadcl state var_vcelist
		      fun dowrite (clregi, (_,{code,cenv,venv,tipe}),s) = 
			  let val (I ir,s) = xexp'(s,fresh_named_var "venv", venv, 
						     Nil.TraceUnknown, NOTID)
			  in  (add_instr(INIT(EA(clregi,8), ir, NONE)); s)
			  end
		      val _ = if is_recur 
				  then (foldl2 dowrite rec_state (clregsi, var_vcelist); ())
			      else ()
		      val _ = add_instr(ICOMMENT ("done allocating " ^ 
						  (Int.toString (length var_vcelist)) ^ " closures"))
		      val _ = msg "done with fixclosure_b\n"
		  in  rec_state
		  end)

  
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
		in  if istoplevel()
			then add_conglobal (state,v,Single_k c,termOpt)
		    else add_conterm (state,v,Single_k c,termOpt)
		end
	 | Code_cb (conwork as (name,vklist,c)) => 
		let val funkind = Arrow_k(Code,vklist,Single_k c)
		    val funcon = Let_c(Sequential,[cbnd],Var_c name)
		    val l = LOCAL_CODE(Name.var2string name)
		    val state = add_conterm (state,name,funkind, SOME(VALUE(CODE l)))
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
	  fun xvector (c,a : exp Array.array) : term * state =
	      let 
		  val (vals,state) = 
		      (Array.foldr (fn (e,(vls,state)) => 
				    let val (v,state) = xexp(state,Name.fresh_var(),e,
							     Nil.TraceUnknown,NOTID)
				    in  (v::vls,state)
				    end)
		       ([],state) a)
		  fun charVector() =
		      let fun mapper (VALUE (INT w)) = chr (TW32.toInt w)
			    | mapper _ = error "char vector but non-INT value"
			  val chars = map mapper vals
		      in  add_data(STRING (String.implode chars));
			  add_data(ALIGN LONG)
		      end
		  fun wordVector() =
		      let fun apper (VALUE v) = 
			       (case v of
				    INT w => add_data(INT32 w)
				  | TAG w => add_data(INT32 w)
				  | REAL l => add_data(DATA l)
				  | RECORD (l,_) => add_data(DATA l)
				  | VOID _ => error "got a vvoid in xvector"
				  | LABEL l => add_data(DATA l)
				  | CODE l => add_data(DATA l))
			    | apper _ = error "vector but non value component"
		      in  app apper vals
		      end

		  val c = #2(simplify_type state c)
		  val shiftAmount= (case c of
					Prim_c(Int_c Prim.W8, []) => int_len_offset
				      | Prim_c(Int_c Prim.W16, []) => error "word16 not handled"
				      | _ => 2 + int_len_offset)
		  val numElem = TW32.fromInt(Array.length a)
		  val tagword = TW32.orb(TW32.lshift(numElem, shiftAmount), intarray)
		  val _ = add_data(INT32 tagword)
		  val label = fresh_data_label "vectorData"
		  val _ = add_data(DLABEL label)
		  val _ = (case c of
			       Prim_c(Int_c Prim.W8, []) => charVector()
			     | Prim_c(Int_c Prim.W16, []) => error "word16 not handled"
			     | _ => wordVector())
	      in  (VALUE(LABEL label), state)
	      end
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
	      | (vector (c,a)) => xvector(c,a)
	      | (array _)  => error "array/vector/refcell constants not implemented"
	      | refcell _ => error "array/vector/refcell constants not implemented"
	      | (tag(t,c)) => let val i = TW32.fromInt (tag2int t)
			      in  (VALUE(INT i), state)
			      end)
      end



  and xexp' (state   : state,     (* state of bound variables *)
	     name    : var,       (* Purely for debugging and generation of useful names *)
	     e       : exp,       (* The expression being translated *)
	     trace   : niltrace,  (* The type of the expression being translated *)
	     context              (* The evaluation context this expression is in *)
	     ) : reg * state =
      let val (term, s) = xexp(state,name,e,trace,context) 
      in  (load_reg_term(term,NONE), s)
      end

  and xexp_list (state,elist) : term list * state = 
      let fun folder(e,state) = xexp(state,fresh_var(),e,Nil.TraceUnknown,NOTID)
      in  foldl_acc folder state elist
      end

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
	  val res = xexp''(state,name,arg_e,trace,context)
	  val _ = if (!debug)
		      then (print "xexp ";  print (Int.toString (!exp_depth));
			    print " returned \n")
		  else ()
	  val _ = exp_depth := !exp_depth - 1
	      
      in  res
      end

  and xexp'' (state   : state,     (* state of bound variables *)
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
	      Var_e v => (case (getrep state v) of
			      (_,SOME value) => (VALUE value, state)
			    | (SOME loc,_) => (LOCATION loc, state)
			    | (NONE,NONE) => error "no info on var")
	    | Const_e v => xconst(state,v)
	    | Let_e (_, [], body) => xexp(state,name,body,trace,context)
	    | Let_e (_, bnds, body) => 
		  let val first_bnds = Listops.butlast bnds
		      val last_bnd = List.last bnds
		      fun folder (bnd,s) = xbnd s bnd
		      val state = foldl folder state first_bnds
		  in
		      (case (last_bnd,body) of
			   (Exp_b(v,nt,e),Var_e v') => 
			       if (eq_var(v,v'))
				   then xexp(state,name,e,nt,context)
			       else xexp(xbnd state last_bnd,fresh_var(),body,trace,context)
			 | _ => xexp(xbnd state last_bnd,fresh_var(),body,trace,context))
		  end
	    | Prim_e (NilPrimOp nilprim, clist, elist) => xnilprim(state,nilprim,clist,elist,
								   context,trace)
	    | Prim_e (PrimOp prim, clist, elist) => xprim(state,prim,clist,elist,context,trace)
	    | Switch_e sw => xswitch(state,name,sw,trace,context)
	    | ExternApp_e (f, elist) => (* assume the environment is passed in first *)
		  let 
		      val tmp1 = alloc_regi NOTRACE_INT
		      val tmp2 = alloc_regi NOTRACE_INT
		      val _ = add_instr (ICOMMENT ("making external call"))
		      fun cfolder (c,state) = xcon(state,fresh_named_var "call_carg", c)
		      fun efolder(e,(eregs,fregs,state)) = 
			  let val (res,state) = xexp'(state,fresh_named_var "call_e", 
							e, Nil.TraceUnknown, NOTID)
			  in  case res of
			      I ir => (ir::eregs,fregs,state)
			    | F fr => (eregs,fr::fregs,state)
			  end

		      val (rev_iregs, rev_fregs, state) = foldl efolder ([],[],state) elist
		      val iregs = rev rev_iregs
		      val fregs = rev rev_fregs
		      val args = (map I iregs) @ (map F fregs)

		      val Var_e expvar = f
		      val (vlopt,vvopt) = getrep state expvar

		      val fun_reglabel = 
				(case (vlopt,vvopt) of
				     (NONE,SOME(CODE l)) => LABEL' l
				   | (SOME(REGISTER (_,I r)),_) => REG' r
				   | (SOME(GLOBAL (l,_)),_) => 
					 let val addr = alloc_regi NOTRACE_LABEL
					     val reg = alloc_regi NOTRACE_CODE
					 in  (add_instr(LADDR(l,0,addr));
					      add_instr(LOAD32I(EA(addr,0),reg));
					      REG' reg)
					 end
				   | _ => error "bad varloc or varval for function")
				     
				     
		      val (_,dest) = alloc_reg_trace state trace
		      val _ = add_instr(LI(0w1,tmp1))
		      val _ = add_instr(STORE32I(EA(SREGI THREADPTR,notinml_disp),tmp1))
		      val _ = add_instr(CALL{call_type = C_NORMAL,
					     func = fun_reglabel,
					     args = args,
					     results = [dest],
					     save = getLocals()})
		      val _ = add_instr(LI(0w0,tmp2))
		      val _ = add_instr(STORE32I(EA(SREGI THREADPTR,notinml_disp),tmp2))
		      val result = (LOCATION(REGISTER (false,dest)), new_gcstate state)
		      val _ = add_instr (ICOMMENT ("done making external call"))
			  
		  in  result
		  end

	    | App_e (openness, f, clist, elist, eflist) => (* assume the environment is passed in first *)
		  let 
		      val callcount = Stats.counter("RTLcall")()
		      val call_type = 
			      (case openness of
				    Open => error "no open calls permitted here"
				  | Code => "direct call "
				  | Closure => (if (length elist = 0 andalso length eflist = 0)
						    then "closure polycall"
						else "closure call ")
				    ^ (Int.toString callcount))

		      local 
			  fun cfolder (c,state) = xcon(state,fresh_named_var "call_carg", c)
			  fun efolder(e,state) = xexp'(state,fresh_named_var "call_e", 
						       e, Nil.TraceUnknown, NOTID)
		      in
			  val (cregsi,state) = foldl_list cfolder state clist
			  val (eregs, state) = foldl_list efolder state elist
			  val (efregs, state) = foldl_list efolder state eflist
		      end

		      val _ = add_instr (ICOMMENT ("making " ^ call_type))
		      fun direct_call expvar = 
			  let 
			      val (vlopt,vvopt) = getrep state expvar
			  in  (Name.eq_var(#1(getCurrentFun()), expvar), 
			       (case (vlopt,vvopt) of
				    (NONE,SOME(CODE l)) => LABEL' l
				  | (SOME(REGISTER (_,I r)),_) => REG' r
				  | (SOME(GLOBAL (l,_)),_) => 
					let val addr = alloc_regi NOTRACE_LABEL
					    val reg = alloc_regi NOTRACE_CODE
					in  (add_instr(LADDR(l,0,addr));
					     add_instr(LOAD32I(EA(addr,0),reg));
					     REG' reg)
					end
				  | _ => error "bad varloc or varval for function"),
				    [], [],state)
			  end
		      
		      val (selfcall,fun_reglabel,cregsi',eregs',state) = 
			  (case (openness,f) of
			       (Code,Var_e expvar) => (direct_call expvar)
			     | (Code,_) => error "ill-formed application"
			     | (Open,_) => error "no open apps allowed"
			     | (Closure,cl) =>
				   let val (clreg,state) = xexp'(state,fresh_var(),cl,
								 Nil.TraceUnknown,NOTID)
				       val clregi = 
					   (case clreg of
						I ir => ir
					      | F _ => error "closure compiled to float reg")
				       val funregi = alloc_named_regi (fresh_named_var "funreg") 
					   NOTRACE_CODE
				       val cregi =  alloc_named_regi (fresh_named_var "creg") TRACE
				       val eregi =  alloc_named_regi (fresh_named_var "ereg") TRACE
				       val _ = (add_instr(LOAD32I(EA(clregi,0),funregi));
						add_instr(LOAD32I(EA(clregi,4),cregi));
						add_instr(LOAD32I(EA(clregi,8),eregi)))
				   in  (false, REG' funregi, [cregi], [I eregi],state)
				   end)
			       
		      val args = (map I cregsi) @ 
			         (map I cregsi') @
				 eregs @ eregs' @ efregs
		      val (resrep,dest) = alloc_reg_trace state trace
		      val context = if (#elim_tail_call(!cur_params))
					then context
				    else NOTID
		  in
		      (case (context,selfcall) of
			   (NOTID, _) => 
			       (add_instr(CALL{call_type = ML_NORMAL,
					       func = fun_reglabel,
					       args = args,
					       results = [dest],
					       save = getLocals()});
				add_instr (ICOMMENT ("done making normal call"));
				(LOCATION(REGISTER (false,dest)), 
				 new_gcstate state))
			 | (ID r,true) =>  
			       (shuffle_regs(args,getArgs());
				add_instr(BR (getTop()));
				add_instr (ICOMMENT ("done making self tail call"));
				(VALUE (VOID resrep), state))
			 | (ID r,false) =>
			       (add_instr(CALL{call_type = ML_TAIL r,
					       func = fun_reglabel,
					       args = args,
					       results = [dest],
					       save = []});
				add_instr (ICOMMENT ("done making tail call"));
				(LOCATION(REGISTER (false,dest)),
				 new_gcstate state)))
		  end

	    | Raise_e (exp, _) =>
		  let val (I ir,state) = xexp'(state,name,exp,Nil.TraceUnknown,NOTID)
		      val newpc = alloc_regi NOTRACE_CODE
		      val rep = niltrace2rep state trace
		  in  add_instr(LOAD32I(EA(exnptr,0),newpc));
		      add_instr(MV (ir,exnarg));
		      add_instr RESTORE_CS;
		      add_instr (JMP(newpc,nil));
		      (VALUE(VOID rep), state)
		  end
            (* --- We rely on the runtime to unwind the stack so we don't need to save the
	           free variables of the continuation of this expression. *)
	    | Handle_e (exp, exnvar, handler_body) => 
		  let
		      (* compute free variables that need to be saved for handler *)


		      local
			  val handler_body' = Let_e(Sequential,[Exp_b(exnvar,TraceKnown (valOf(TraceOps.get_trace (NilContext.empty (), NilUtil.exn_con))), NilUtil.match_exn)],
						    handler_body)
			  val (free_evars,free_cvars) = NilUtil.freeExpConVarInExp(false, handler_body')
			  val evar_reps = map (fn v => #1(getrep state v)) free_evars
			  val free_cvars = (List.filter 
					    (fn v => (case (getconvarrep' state v) of
							  NONE => false
							| SOME _ => true)) free_cvars)
			  val cvar_reps = map (fn v => #1(getconvarrep state v)) free_cvars
			  val vlopts = cvar_reps @ evar_reps
			  fun loop [] (irep,ir,fr) = (irep,ir,fr)
			    | loop (vlopt::rest) (irep,ir,fr) = 
			      (case vlopt of
				   SOME(REGISTER (_,I (r as (REGI (_,rep))))) => loop rest 
				       (rep::irep,r::ir,fr)
				 | SOME(REGISTER (_,I (SREGI _))) => error "SREGI free in handler!!!"
				 | SOME(REGISTER (_,F r)) => loop rest (irep,ir,r::fr)
				  (* don't need to save globals - or varval only *)
				 | SOME(GLOBAL _) => loop rest (irep,ir,fr)
				 | NONE => loop rest (irep,ir,fr))
		      in
			  val (local_int_reps,local_iregs, local_fregs) = loop vlopts ([],[],[])
		      end



		      val hl = fresh_code_label "exn_handler"
		      val hlreg = alloc_regi NOTRACE_CODE
		      val bl = fresh_code_label "exn_after"

		      val (fpbase,state) = (* --- save the floating point values, if any *)
			  (case local_fregs of
			       [] => (NONE,state)
			     | _ => let val vv = map (fn freg => LOCATION(REGISTER(false,F freg))) local_fregs
				        val (ir,state) = fparray(state,vv)
				    in  (SOME ir, state)
				    end)


		      (* --- create the exn record and set the exnptr to it to install it *)
		      val int_vallocs = (map (fn ireg => LOCATION(REGISTER(false,I ireg)))
					 ([hlreg, stackptr,exnptr] @ local_iregs))
		      val reps = (NOTRACE_CODE :: NOTRACE_LABEL :: TRACE :: local_int_reps)
		      val _ = add_instr(LADDR(hl,0,hlreg))
		      val (_,state) = make_record(state,SOME exnptr,reps, int_vallocs)
			  

                      (* --- compute the body; restore the exnpointer; branch to after handler *)
		      (* NOTID and not context because we have to remove the exnrecord *)
		      val (reg,state) = xexp'(state,name,exp,trace,NOTID)
		      val _ = (add_instr(LOAD32I(EA(exnptr,8),exnptr));
			       add_instr(BR bl));


		      (* --- now the code for handler --- *)
		      val _ = add_instr(ILABEL hl)
		      val _ = add_instr(HANDLER_ENTRY)

		      (* --- restore the int registers - stack-pointer FIRST --- *)
		      val _ = let 
				  val int_regs =
				      (case fpbase of
					   SOME r => [stackptr, exnptr, r] 
					 | NONE => [stackptr, exnptr]) @ local_iregs
				  fun f (h :: t,offset) =
				      (if eqregi(h,exnptr) then ()
				       else add_instr(LOAD32I(EA(exnptr,offset),h));
					   f (t,offset+4))
				    | f (nil,offset) = ()
			      in f (int_regs,4)
			      end


		      (* --- restore the float registers --- *)			  
		      val _ = let fun f (base, h :: t,offset) = 
			                    (add_instr(LOADQF(EA(base,offset),h));
					     f (base, t,offset+8))
				    | f (base,nil,offset) = ()
			      in (case fpbase of
				      SOME base => f (base,local_fregs,0)
				    | NONE => ())
			      end
			  
		      (* --- now that stack-pointer is restored, 
		         ---    we can update maxsp of the thread
		         ---    we can move the exn arg 
		         ---    we can call new_gcstate 
		         ---    note that since the exnarg and ra register are the same
		                  the exn arg must be moved before the gc_check *)
		      val tmp1 = alloc_regi(NOTRACE_INT)
		      val tmp2 = alloc_regi(NOTRACE_INT)
		      val _ = add_instr(LOAD32I(EA(SREGI THREADPTR,maxsp_disp),tmp1))
		      val _ = add_instr(CMPUI(GT,SREGI STACKPTR,REG tmp1,tmp2))
		      val _ = add_instr(CMV(NE,tmp2,REG(SREGI STACKPTR), tmp1))
		      val _ = add_instr(STORE32I(EA(SREGI THREADPTR,maxsp_disp),tmp1))
		      val xr = alloc_named_regi exnvar TRACE
		      val _ = add_instr(MV(exnarg,xr))
		      val hstate = new_gcstate state
		      val hstate = add_reg (hstate,exnvar,Prim_c(Exn_c,[]),I xr)


                      (* --- restore exnptr; compute the handler; move result into same register
                             as result reg of expression; add after label; and fall-through *)
		      val _ = add_instr(LOAD32I(EA(exnptr,8),exnptr))
		      val (hreg,hstate) = xexp'(hstate,name,handler_body,trace,context)
		      val _ = (case (hreg,reg) of
				   (I hreg,I reg) => add_instr(MV(hreg,reg))
				 | (F hreg,F reg) => add_instr(FMV(hreg,reg))
				 | _ => error "hreg/ireg mismatch in handler")
		      val _ = add_instr(ILABEL bl)
		      val state = join_states[state,hstate]

		  in 
		      (LOCATION(REGISTER (false,reg)), state)
		  end

      in res
      end



      (* The trick is to notice that for certain args, the comparison and computation
         can be folded into one instruction. *)
      and zero_one (state : state, r : regi, 
		    trace : niltrace, 
		    zeroexp, oneexp, context) : term * state = 
	  let 
	      val thenl = fresh_code_label "zero_case"
	      val elsel = fresh_code_label "one_case"
	      val afterl = fresh_code_label "after_zeroone"
	      val _ = add_instr(BCNDI(NE,r,IMM 0,elsel,false))
	      val _ = add_instr(ILABEL thenl)
	      val (zero,state_zero) = xexp'(state,fresh_named_var "zero_result", 
						 zeroexp, trace, context)
	      val (_,dest) = alloc_reg_trace state trace
	      val _ = add_instr(mv(zero, dest))
	      val _ = add_instr(BR afterl)
	      val _ = add_instr(ILABEL elsel)
	      val (one,state_one) = xexp'(state,fresh_named_var "nonzero_result", oneexp, 
					       trace, context)
	      val state = join_states[state_zero,state_one]
	      val _ = add_instr(mv(one,dest))
	      val _ = add_instr(ILABEL afterl)
	  in (LOCATION (REGISTER (false, dest)), state)
	  end


  and xswitch (state   : state,
	       name    : var,      (* Purely for debugging and generation of useful names *)
	       sw      : switch,   (* The switch expression being translated *)
	       trace   : niltrace, (* The type of the switch expression *)
	       context             (* The evaluation context this expression is in *)
	       ) : term * state =
      let
	  val dest = ref NONE
	  fun move r = let val _ = (case (!dest) of
					  NONE => dest := (SOME (#2(alloc_reg_trace state trace)))
					| _ => ())
			     val d = valOf(!dest)
			 in add_instr(mv(r,d))
			 end
	  fun no_match state = 
	      (case (!dest) of
		   SOME _ => let val c = NilUtil.bool_con (* XXX not really *)
				 val (r,newstate) = 
		                      xexp'(state,fresh_var(),Raise_e(NilUtil.match_exn,c),
					    trace, context)
			     in  move r; newstate
			     end
		 | NONE => error "empty switch statement")
	  val switchcount = Stats.counter("RTLswitch")()
      in
	  case sw of
	      Intsw_e {size, arg, arms, default, result_type} => 
		  let val (I r,state) = xexp'(state,fresh_named_var "intsw_arg",arg,
					      Nil.TraceUnknown,NOTID)
		  in  case (arms,default) of
		      ([(0w0,z)],SOME e) => zero_one(state, r, trace, z, e, context)
		    | ([(0w0,z),(0w1,one)],NONE) => zero_one(state, r, trace, z, one, context)
		    | _ => (* general case *)
			  let 
			      val afterl = fresh_code_label "after_intcase"
			      fun scan(states,lab,[]) = 
				  (add_instr(ILABEL lab);
				   case default of
				       NONE => (no_match state::states)
				     | SOME e => 
					   let val (r,newstate) = (xexp'(state,fresh_var(),e,
									   trace,context))
					   in  move r; newstate::states
					   end)
				| scan(states,lab,(i,body)::rest) =
				  let val next = fresh_code_label "intarm"
				  in  add_instr(ILABEL lab);
				      if in_imm_range i
					  then add_instr(BCNDI(NE,r,IMM (w2i i), next,true))
				      else let val tmp = alloc_regi(NOTRACE_INT)
					   in add_instr(LI(i,tmp));
					      add_instr(BCNDI(NE,r,REG tmp, next,true))
					   end;
				      let val (r,newstate) = 
					  xexp'(state,fresh_var(),body,trace,context)
				      in  move r;
					  add_instr(BR afterl);
					  scan(newstate::states,next,rest)
				      end
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
	    | Exncase_e {arg, bound, arms, default, result_type} => 
		  let
		      val (I exnarg,state) = xexp'(state,fresh_named_var "exncase_arg",arg,
						     Nil.TraceUnknown,NOTID)

		      val exntag = alloc_regi(NOTRACE_INT)
		      val _ = add_instr(LOAD32I(EA(exnarg,0),exntag))
		      val afterl = fresh_code_label "after+exncase"
		      fun scan(states,lab,[]) =
			  (add_instr(ILABEL lab);
			   case default of
			       NONE => 
				   let val con = NilUtil.bool_con  (* XXX not really *)
				       val (r,newstate) = xexp'(state,fresh_var(),
								  Raise_e(arg,con),trace,context)
				   in  move r; newstate :: states
				   end
			     | SOME e => 
				   let val (r,newstate) = xexp'(state,fresh_var(),e,
								  trace,context)
				   in  move r; newstate :: states
				   end)
			| scan(states,lab,(armtag,tr,body)::rest) = 
			  let 
			      val _ = add_instr(ILABEL lab)
			      val (I armtagi,state) = xexp'(state,fresh_var(),armtag,
								   Nil.TraceUnknown,NOTID)
			      val tagcon = type_of state armtag
			      val (_,Prim_c(Exntag_c,[c])) = simplify_type state tagcon
			      val next = fresh_code_label "exnarm"
			      val test = alloc_regi(NOTRACE_INT)
			      val (_,carried) = alloc_reg_trace state tr
			      val carriedi = (case carried of
						  I ir => ir
						| _ => error "carried value is an unboxed float")
			      val state = add_reg (state,bound,c,carried)
			  in  add_instr(BCNDI(NE,exntag,REG armtagi,next,true));
			      add_instr(LOAD32I(EA(exnarg,4),carriedi));
			      let val (r,state) = xexp'(state,fresh_var(),body,
							  trace,context)
			      in  move r;
				  add_instr(BR afterl);
				  scan(state::states,next,rest)
			      end
			  end
		      val states = scan([],fresh_code_label "exnarm",arms)
		      val state = join_states states
		  in  
		      add_instr(ILABEL afterl);
		      case !dest of
			  SOME r => (LOCATION(REGISTER (false, r)),state)
			| _ => error "no arms"
		  end 
	    | Typecase_e _ => error "typecase_e not implemented"
	    | Sumsw_e {sumtype, arg, bound, arms, default, result_type} =>
	      let val (tagcount,_,cons) = reduce_to_sum "sumsw" state sumtype
		  val totalcount = tagcount + TilWord32.fromInt(length cons)
		  fun spcon i = Prim_c(Sum_c{tagcount=tagcount,
					     totalcount=totalcount,
					     known = SOME i}, 
				       if (length cons = 1)
					   then cons
				       else [con_tuple_inject cons])
	      in 
	       (case (tagcount,cons,arms, default) of
		  (0w2,[], [(0w0,_,zeroexp),(0w1,_,oneexp)], NONE) => 
			let val (I r,state) = xexp'(state,fresh_named_var "intsw_arg",arg,
						    Nil.TraceUnknown,NOTID)
			in  zero_one(state,r, trace, zeroexp, oneexp, context)
			end
		| _ =>
		  let val (I r,state) = xexp'(state,fresh_named_var "sumsw_arg",arg,
						Nil.TraceUnknown,NOTID)
		      val afterl = fresh_code_label "after_sum"
		      val nomatchl = fresh_code_label "nomatch_sum"
		      val one_carrier = (length cons) = 1
		      val total = TW32.uplus(tagcount, i2w(length cons))
		      val exhaustive = 
			  let val handled = map (fn (w,_,_) => (w2i w)) arms
			      fun mem i = member(i,handled)
			  in  List.all mem (count (w2i total))
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
					 let val (r,state) = xexp'(state,fresh_var(),e,
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
			      (* If tag cmp i, branch to label *)
			      fun check lbl cmp i tag = 
				  (if in_imm_range i
				       then add_instr(BCNDI(cmp,tag,IMM (w2i i), lbl, true))
				   else 
				       let val tmp = alloc_regi(NOTRACE_INT)
				       in  add_instr(LI(i,tmp));
					   add_instr(BCNDI(cmp,tag,REG tmp,lbl, true))
				       end)
			      val check_ptr_done = ref (TW32.equal(tagcount,0w0))
			      val load_tag_done = ref false
			      fun check_ptr() = 
				  (if (!check_ptr_done) 
				       then () 
				   else check nomatchl LE 0w255 r;
				       check_ptr_done := true)
			      fun load_tag() = 
				  (if (!load_tag_done) 
				       then () 
				   else add_instr(LOAD32I(EA(r,0),tag));
				      load_tag_done := true)
			  in  add_instr(ICOMMENT ("switch # : " ^ (Int.toString switchcount) ^ "case " 
						  ^ (TW32.toDecimalString i)));
			      (case (exhaustive andalso TW32.equal(TW32.uplus(i,0w1),total),
				     TW32.ult(i,tagcount)) of
				  (true,_) => ()
				| (_,true) => check next NE i r
				| (_,false) => (if exhaustive then () else check_ptr();
						if one_carrier
						    then ()
						else (load_tag();
						     check next NE (TW32.uminus(i,tagcount)) tag)));
			      let val (r,state) = xexp'(state,fresh_var(),body,
							  trace,context)
			      in  move r;
				  add_instr(BR afterl);
				  scan(state::newstates,next,rest)
			      end
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
      end





  and xnilprim(state : state, nilprim,clist,elist,context,trace) : term * state = 
      let fun error' s = (print "NIL primexpression was:\n";
			  Ppnil.pp_exp (Nil.Prim_e(Nil.NilPrimOp nilprim, clist,elist));
			  print "\n";
			  error s)
      in
      (case nilprim of 
	   Nil.record labels => 
	       let fun folder(e,state) = xexp(state,fresh_var(), e, Nil.TraceUnknown, NOTID)
		   val (terms,state) = foldl_list folder state elist
		   val reps = map valloc2rep terms
	       in  make_record(state,NONE,reps,terms)
	       end
         | partialRecord _ => error "partialRecord not implemented"
	 | select label => 
	       let 
		   val [e] = elist 
		   val (I addr, state) = xexp'(state,fresh_var(),e,Nil.TraceUnknown,NOTID)
		   val reccon = type_of state e
		   val (_,Prim_c(Record_c (labels,_),fieldcons)) = simplify_type state reccon
		   fun loop [] _ n = error' "bad select 1"
		     | loop _ [] n = error' "bad select 2"
		     | loop (l1::lrest) (c1::crest) n = if (Name.eq_label(l1,label))
							    then n
							else loop lrest crest (n+1)
		   val which = loop labels fieldcons 0
		   val I desti = #2(alloc_reg_trace state trace)
		   val _ = add_instr(LOAD32I(EA(addr,which * 4), desti))
	       in  (LOCATION(REGISTER(false, I desti)), state)
	       end
	 | inject_record known => 
		let val (lvs,state) = xexp_list (state,elist)
		in  TortlSum.xsum_record ((state,known,hd clist),lvs)
		end

	 | inject_nonrecord known => 
		let val (lvopt,state) = 
		    (case elist of
			 [] => (NONE,state)
		       | [e] => let val (lv,state) = xexp(state,fresh_var(),hd elist,
							    Nil.TraceUnknown,NOTID)
				in  (SOME lv,state)
				end)
		in  TortlSum.xsum_nonrecord ((state,known,hd clist),lvopt,trace)
		end
	 | inject known => 
		(case elist of
		    [] => (print "Warning: tortl encountered inject with no argument\n";
			   print "         COnverting to inject_nonrecord\n";
			   TortlSum.xsum_nonrecord ((state,known,hd clist),NONE,trace))
		  | [e] =>
			let val (e_lv,state) = xexp(state,fresh_var(),e,
						      Nil.TraceUnknown,NOTID)
			    val (_,c_lv,state) = xcon'(state,fresh_var(),hd clist)
			in  TortlSum.xsum_dynamic ((state,known,hd clist),c_lv,e_lv,trace)
			end
		| _ => error "inject_dynamic with more than one argument")
	 | project_sum_record (k,field) => 
	       let val [e] = elist
		   val (I base,state) = xexp'(state,fresh_var(),e,Nil.TraceUnknown,NOTID)
		   val ssumcon = type_of state e
	       in  TortlSum.xproject_sum_record ((state,k,ssumcon),field,clist,base,trace)
	       end

	 | project_sum_nonrecord k =>
	       let val [sumcon] = clist
		   val [e] = elist
		   val (I base,state) = xexp'(state,fresh_var(),e,Nil.TraceUnknown,NOTID)
		   val ssumcon = type_of state e
	       in  TortlSum.xproject_sum_nonrecord ((state,k,sumcon),base,ssumcon,trace)
	       end

	 | project_sum k =>
	       let val [sumcon] = clist
		   val [e] = elist
		   val (I base,state) = xexp'(state,fresh_var(),e,Nil.TraceUnknown,NOTID)
		   val (cr,state) = xcon(state,fresh_var(),sumcon)
	       in  TortlSum.xproject_sum_dynamic ((state,k,sumcon),cr,base,trace)
	       end

	 | box_float Prim.F64 => 
	       let val [e] = elist
		   val (lv,state) = xexp(state,fresh_var(),e,Nil.TraceUnknown,NOTID)
		   val (vl,state) = boxFloat_vl(state,lv)
	       in (vl, state)
	       end
	 | unbox_float Prim.F64 => 
	       let val [e] = elist
		   val (I ir,state) = xexp'(state,fresh_var(),e,Nil.TraceUnknown,NOTID)
		   val fr = alloc_regf()
		   val _ = add_instr(LOADQF(EA(ir,0),fr))
	       in (LOCATION(REGISTER(false, F fr)), state)
	       end
	 | box_float Prim.F32 => error "32-bit floats not done"
	 | unbox_float Prim.F32 => error "32-bit floats not done"
	 | roll => let val [e] = elist
		   in  xexp(state,fresh_var(),e,Nil.TraceUnknown,context)
		   end
	 | unroll => 
		   let val [e] = elist
		   in  xexp(state,fresh_var(),e,Nil.TraceUnknown,context)
		   end
	 | make_exntag => 
		   let val desti = alloc_regi NOTRACE_INT
		       val addr = alloc_regi TRACE
		       val tmp = alloc_regi NOTRACE_INT
		       val _ = (add_instr(LADDR(exncounter_label,0,addr));
				add_instr(LOAD32I(EA(addr,0),desti));
				add_instr(ADD(desti,IMM 1,tmp));
				add_instr(STORE32I(EA(addr,0),tmp)))
		   in  (LOCATION(REGISTER (false, I desti)), state)
		   end
	 | inj_exn name => 
		   let val [e1,e2] = elist
		       val desti = alloc_regi NOTRACE_INT
		       val (vl1,state) = xexp(state,fresh_var(),e1,Nil.TraceUnknown,NOTID)
		       val (vl2,state) = xexp(state,fresh_var(),e2,Nil.TraceUnknown,NOTID)
		       fun char2val c = Const_e(Prim.uint(Prim.W8, TW64.fromInt (ord c)))
		       val name_array = Array.fromList (map char2val (explode name))
		       val (vl3,state) = xconst(state, Prim.vector(char_con,name_array))
		       val vallocs = [vl1,vl2,vl3]
		       val reps = map valloc2rep vallocs
		   in  make_record(state,NONE,reps,vallocs)
		   end
	 | make_vararg oe => 
		   let fun local_xexp (s,e) = 
		          let val (I ir, _) = xexp'(s,fresh_var(),e, Nil.TraceUnknown,NOTID)
			  in  ir
			  end
		       val [c1,c2] = clist
		       val (argc, state) = xcon(state,fresh_var(),c1)
		       val (resc, state) = xcon(state,fresh_var(),c2)
		       val (I function,state) = xexp'(state,fresh_var(),hd elist,
							Nil.TraceUnknown, NOTID)
		       val (state,resulti) = TortlVararg.xmake_vararg local_xexp (state,argc,resc,function)
		   in  (LOCATION(REGISTER(false, I resulti)), state)
		   end
	 | make_onearg (openness,eff) => 
		   let  fun local_xexp (s,e) = 
		          let val (I ir, _) = xexp'(s,fresh_var(),e, Nil.TraceUnknown,NOTID)
			  in  ir
			  end
		       val [c1,c2] = clist
		       val (argc, state) = xcon(state,fresh_var(),c1)
		       val (resc, state) = xcon(state,fresh_var(),c2)
		       val (I function,state) = xexp'(state,fresh_var(),hd elist,
							Nil.TraceUnknown, NOTID)
		       val (state,resulti) = TortlVararg.xmake_onearg local_xexp (state,argc,resc,function)
		   in  (LOCATION(REGISTER(false, I resulti)), state)
		   end
	 | peq => error "peq not done")
      end



  and xprim(state,Prim.neg_int is,clist,elist,context,trace) =
          xprim(state, Prim.minus_int is, clist, (Const_e(Prim.int(Prim.W32,TW64.zero)))::elist,context,trace)
    | xprim(state : state, prim,clist,elist,context,trace) : term * state = 
      let 
	  fun error' s = (print "nilprimexpression was:\n";
			  Ppnil.pp_exp (Nil.Prim_e(Nil.PrimOp prim, clist,elist));
			  print "\n";
			  error s)
	  open Prim
	  val (vl_list,state) = xexp_list(state,elist)
	  val int32 = Prim_c(Int_c W32, []) 
	  val float64 = Prim_c(Float_c F64, []) 
	  fun xtt int_tt = INT_TT
	    | xtt real_tt = REAL_TT
	    | xtt both_tt = BOTH_TT
	  fun commute (v1 as (VALUE(INT i)),v2) = 
	      if in_imm_range i then (v2,v1) else (v1,v2)
	    | commute arg = arg
	  (* ----------- integer comparisons ----------------------- *)
	  fun flip EQ = EQ
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
          fun stdcmp2i signed oper =
                   let val [vl1,vl2] = vl_list
		       val (oper,vl1,vl2) = swap(oper,vl1,vl2)
		       val a' = load_ireg_term(vl1,NONE)
		       val b' = load_ireg_sv vl2
		       val dest = alloc_regi NOTRACE_INT
		       val cmp = if signed then CMPSI else CMPUI
		       val _ =  add_instr(cmp(oper,a',b',dest))
		   in (LOCATION(REGISTER(false, I dest)), state)
		   end

          val stdcmp2si = stdcmp2i true
          val stdcmp2ui = stdcmp2i false

          (* ----------- floatint point comparison ---------------- *)
	  fun cmpf oper =
	      let val [vl1,vl2] = vl_list
		  val a' = load_freg_term(vl1,NONE)
		  val b' = load_freg_term(vl2,NONE)
		  val dest = alloc_regi NOTRACE_INT
		  val _ =  add_instr(CMPF(oper,a',b',dest))
	      in (LOCATION(REGISTER(false, I dest)), state)
	      end


	  (* ----------- unary integer operations ----------------- *)
	  fun op1i oper : term * state =
	      let val [vl1] = vl_list
		  val a' = load_ireg_term(vl1,NONE)
		  val dest = alloc_regi NOTRACE_INT
		  val _ = add_instr(oper(a',dest))
	      in (LOCATION(REGISTER(false, I dest)), state)
	      end

	  (* ----------- binary integer operations ----------------- *)
	  fun op2i comflag oper : term * state =
	      let val [vl1,vl2] = vl_list 
		  (* commute values if the first arg is a small imm *)
		  val (vl1,vl2) = if comflag then commute(vl1,vl2) else (vl1,vl2)
		  val a' = load_ireg_term(vl1,NONE)
		  val b' = load_ireg_sv vl2
		  val dest = alloc_regi NOTRACE_INT
		  val _ = add_instr(oper(a',b',dest))
	      in (LOCATION(REGISTER(false, I dest)), state)
	      end

	  val commutesop2i = op2i true
	  val stdop2i = op2i false
	  fun add_ibar b1 b2 = (add_instr (b1 INT_TT); add_instr (b2 INT_TT))
	  fun trapZero result = (add_ibar SOFT_ZBARRIER HARD_ZBARRIER; result)
	  fun trapOver result = (add_ibar SOFT_VBARRIER HARD_VBARRIER; result)
	  (* ----------- binary and unary float operations ----------------- *)
	  fun op2f oper : term * state =
	      (case vl_list of
		   [vl1,vl2] => 
		       let val a' = load_freg_term(vl1,NONE)
			   val b' = load_freg_term(vl2,NONE)
			   val dest = alloc_regf()
			   val _ = add_instr(oper(a',b',dest))
		       in (LOCATION(REGISTER(false, F dest)), state)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")
	  fun op1f oper : term * state =
	      (case vl_list of
		   [vl] => 
		       let val a' = load_freg_term(vl,NONE)
			   val dest = alloc_regf()
			   val _ = add_instr(oper(a',dest))
		       in (LOCATION(REGISTER(false, F dest)), state)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")
	  fun extract_dispatch(t,state,arg,(xfloat,xint,xknown,xdynamic)) = 
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
		     | (OtherArray true, [c]) => xknown (state,c) arg
		     | (OtherVector true, [c]) => xknown (state,c) arg
		     | (OtherArray false, [c]) => dynamic c
		     | (OtherVector false, [c]) => dynamic c
		     | _ => error' "table primitive did not have right type args")
	      end
	  val unit_result = (unit_term, state)
      in (case prim of
	      soft_vtrap tt => (add_instr(SOFT_VBARRIER(xtt tt)); unit_result)
	    | soft_ztrap tt => (add_instr(SOFT_ZBARRIER(xtt tt)); unit_result)
	    | hard_vtrap tt => (add_instr(HARD_VBARRIER(xtt tt)); unit_result)
	    | hard_ztrap tt => (add_instr(HARD_ZBARRIER(xtt tt)); unit_result)
	       

	    | float2int => 
		  let val [vl] = vl_list 
		      val src = load_freg_term(vl,NONE)
		      val dest = alloc_regi NOTRACE_INT
		      val _ = add_instr(CVT_REAL2INT(src,dest))
		  in (LOCATION(REGISTER(false, I dest)), state)
		  end

	    | int2float => 
		  let val [vl] = vl_list
		      val src = load_ireg_term(vl,NONE)
		      val dest = alloc_regf()
		      val _ = add_instr(CVT_INT2REAL(src,dest))
		  in (LOCATION(REGISTER(false, F dest)), state)
		  end

            (* XXX do we want overflow in some cases or are these casts? *)
	    | int2uint _ => 
		  let val [vl] = vl_list 
		  in  (vl, state)
		  end

	    | uint2uint _ => 
		  let val [vl] = vl_list 
		  in (vl, state)
		  end

	    | uint2int _ => 
		  let val [vl] = vl_list 
		  in (vl, state)
		  end

	    | int2int _ => 
		  let val [vl] = vl_list 
		  in (vl, state)
		  end

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

	    | plus_int W32 =>  trapOver(commutesop2i ADDT)
	    | mul_int W32 =>   trapOver(commutesop2i MULT)
	    | minus_int W32 => trapOver(stdop2i SUBT)
	    | div_int W32 =>   error "ml style div not implemented!"
	    | mod_int W32 =>   error "ml style mod not implemented!"
	    | quot_int W32 =>  trapZero(stdop2i DIVT)
	    | rem_int W32 =>   trapZero(stdop2i MODT)

	    | plus_uint W32 =>  (commutesop2i ADD)
	    | mul_uint W32 =>   (commutesop2i MUL)
	    | minus_uint W32 => (stdop2i SUB)
	    | div_uint W32 =>   (stdop2i DIV)
	    | mod_uint W32 =>   (stdop2i MOD)

	    (* XXXXX should this mod with 255 *)
	    | plus_uint W8 =>  (commutesop2i ADDT)
	    | mul_uint W8 =>   (commutesop2i MULT)
	    | minus_uint W8 => (stdop2i SUBT)
	    | div_uint W8 =>   (stdop2i DIVT)
	    | mod_uint W8 =>   (stdop2i MODT)

	    | less_int W32 => stdcmp2si LT
	    | greater_int W32 => stdcmp2si GT
	    | lesseq_int W32 => stdcmp2si LE
	    | greatereq_int W32 => stdcmp2si GE
	    | eq_int _ => stdcmp2ui EQ
	    | neq_int _ => stdcmp2ui NE

	    (* assume upper bits of 8-bit and 16-bit unsigned ints are zero *)
	    | less_uint _ => stdcmp2ui LT
	    | greater_uint _ => stdcmp2ui GT
	    | lesseq_uint _ => stdcmp2ui LE
	    | greatereq_uint _ => stdcmp2ui GE

	    | neg_int is => error "should not get here"
	    | abs_int is => error "abs_int not done"

	    | not_int W64 => op1i NOTB
	    | not_int _ => op1i NOTB
	    | and_int W64 => commutesop2i ANDB
	    | and_int _ => commutesop2i ANDB
	    | or_int W64 => commutesop2i ORB
	    | or_int _ => commutesop2i ORB
	    | xor_int W64 => commutesop2i XORB
	    | xor_int _ => commutesop2i XORB
	    | lshift_int W32 => stdop2i SLL
	    | rshift_int W32 => stdop2i SRA
	    | rshift_uint W32 => stdop2i SRL

(*
	    | ((plus_int _) | (mul_int _) | (minus_int _) | (div_int _) | 
	       (mod_int _) | (quot_int _) | (rem_int _) | 
	       (plus_uint _) | (mul_uint _) | (minus_uint _) |
	       (div_uint _) | (mod_uint _) |
	       (less_int _) | (greater_int _) | (lesseq_int _) | (greatereq_int _) |
	       (less_uint _) | (greater_uint _) | (lesseq_uint _) | (greatereq_uint _) |
	       (eq_int _) | (neq_int _) | (not_int _) | (and_int _) | (or_int _) | 
	       (lshift_int _) | (rshift_int _) | (rshift_uint _)
	       ) => (print "non 32-bit math not done: ";
		     Ppnil.pp_prim prim; print "\n";
		     error "non 32-bit math not done")
*)

	    | (uinta2uinta (is1,is2)) =>
		  let val [vl] = vl_list 
		  in (vl, state)
		  end
		       
	    | (uintv2uintv (is1,is2)) =>
		  let val [vl] = vl_list 
		  in  (vl, state)
		  end

	    | (array2vector table) => 
		  (case (table,vl_list,clist) of 
		       (IntArray is,[vl],_) => (vl, state)
		     | (FloatArray fs,[vl],_) => (vl, state)
		     | (OtherArray _,[vl],[c]) => (vl,state)
		     | _ => error "illegal array2vector")

	    | (vector2array table) => 
		  (case (table,vl_list,clist) of 
		       (IntVector is,[vl],_) => (vl,state)
		     | (FloatVector fs,[vl],_) => (vl,state)
		     | (WordVector,[vl],[c]) => (vl,state)
		     | _ => error "illegal array2vector")
				
	     | (length_table t) => 
		       let val [vl] = vl_list 
		       in  extract_dispatch(t,state,vl,
					    (TortlArray.xlen_float,
					     TortlArray.xlen_int,
					     TortlArray.xlen_known,
					     TortlArray.xlen_dynamic))
		       end
	     | (sub t) => 
		       let val [vl1,vl2] = vl_list 
		       in  extract_dispatch(t,state,(vl1,vl2,trace),
					    (TortlArray.xsub_float,
					     TortlArray.xsub_int,
					     TortlArray.xsub_known,
					     TortlArray.xsub_dynamic))
		       end
	     | (update t) => 
		       let val [vl1,vl2,vl3] = vl_list 
		       in  extract_dispatch(t,state,(vl1,vl2,vl3),
					    (TortlArray.xupdate_float,
					     TortlArray.xupdate_int,
					     TortlArray.xupdate_known,
					     TortlArray.xupdate_dynamic))
		       end

	     (* we will represent zero length array as an integer zero-length array *)
	     (*   zero is NOT a legal representation for an empty array *)
	     | (create_empty_table t) => 
		       let val vl1 = VALUE(INT 0w0)
			   val vl2 = vl1
		       in  extract_dispatch(t,state,(vl1,vl2),
					    (TortlArray.xarray_float,
					     TortlArray.xarray_int,
					     TortlArray.xarray_known,
					     TortlArray.xarray_dynamic))
		       end

	     | (create_table t) => 
		       let val [vl1,vl2] = vl_list 
		       in  extract_dispatch(t,state,(vl1,vl2),
					    (TortlArray.xarray_float,
					     TortlArray.xarray_int,
					     TortlArray.xarray_known,
					     TortlArray.xarray_dynamic))
		       end

	     | equal_table t =>
		  let val [vl1,vl2] = vl_list 
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
      let 
	  fun mk_ptr  i = (true, VALUE (TAG (TW32.fromInt i)), state)
	  fun mk_ptr' i = (true, VALUE (TAG (TW32.fromInt i)), state)
	  fun mk_sum_help (state,indices,cons) = 
	      let val indices' = map (fn i => VALUE(INT (TW32.fromInt i))) indices
	          fun folder (c,(const,s)) =
		       let val (const',c,s) = xcon'(s,fresh_named_var "xcon_sum",c)
		       in (c,(const andalso const', s))
		       end
		  val (cons',(const,state)) = foldl_list folder (true,state) cons
		  val reps = (map (fn _ => NOTRACE_INT) indices') @ (map (fn _ => TRACE) cons')
		  val (lv,state) = if const 
				       then make_record_const(state,NONE,reps, indices' @ cons', NONE)
				   else make_record(state,NONE,reps, indices' @ cons')
	      in (const, lv, state)
	      end
	  fun mk_sum' (s,indices,cons) = mk_sum_help(s,indices,cons)
	  fun mk_sum (s,index,cons) = mk_sum_help(s,[index],cons)
	  open Prim
      in
	  (case arg_con of
	       Prim_c(Int_c W8, []) => mk_ptr 0
	     | Prim_c(Int_c W16, []) => mk_ptr 1
	     | Prim_c(Int_c W32, []) => mk_ptr 2
	     | Prim_c(Int_c W64, []) => mk_ptr' 3
	     | Prim_c(Float_c F32, []) => mk_ptr' 6 
	     | Prim_c(Float_c F64, []) => mk_ptr' 7
	     | Prim_c(BoxFloat_c F32, []) => mk_ptr 10
	     | Prim_c(BoxFloat_c F64, []) => mk_ptr 11
	     | Prim_c(Exn_c,[]) => mk_ptr 12
	     | Prim_c(Exntag_c,[c]) => mk_sum(state,12,[c])
	     | Prim_c(Array_c,[c]) => mk_sum(state,0,[c])
	     | Prim_c(Vector_c,[c]) => mk_sum(state,1,[c])
	     | Prim_c(Ref_c,[c]) => mk_sum(state,2,[c])
	     | Prim_c(Sum_c {known,totalcount,tagcount},[c]) => 
		   mk_sum'(state,
			   [4,(case known of
				   NONE => ~1
				 | SOME w => TW32.toInt w), 
			    TW32.toInt tagcount,
			    TW32.toInt totalcount],[c])
	     | Prim_c(Sum_c {known,totalcount,tagcount},_) => error "Sum_c does not have 1 type arg"
	     | Prim_c(Record_c _,cons) => mk_sum_help(state,[5,length cons],cons)
	     | Prim_c(Vararg_c _,cons) => mk_sum(state,6,cons)
	     | Prim_c _ => error "ill-formed primitive type"
	     | Mu_c (is_recur,vcset) => 
		   let val (const,lv,state) = mk_sum_help(state,[8],[])
		       val num_mu = Sequence.length vcset
		   in  if (num_mu = 1) then (const,lv,state)
		       else let val reps = Listops.map0count (fn _ => TRACE) num_mu
				val terms = Listops.map0count (fn _ => lv) num_mu
				val (result,state) = 
				    if const (* all mus are base-case/degenerate now *)
					then make_record_const(state,NONE,reps,terms,NONE)
				    else make_record_const(state,NONE,reps,terms,NONE)
			    in  (const,result,state)
			    end
		   end
(*
		   let val vclist = sequence2list vcset
		       fun loop _ [] (s,rev_lvs) = (s,rev rev_lvs)
			 | loop n ((v',c)::vrest) (s,rev_lvs) = 
			   let val (lv,k,s) = if is_recur
						then mk_sum'(s,[7,0],[])
					    else mk_sum(s,7,[c])
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
					   in  add_instr(MUTATE(EA(clreg,4),r,NONE)); s
					   end
				   in  foldl do_write recstate (zip clregs vclist)
				   end
			       else state
		   in (case lvs of
			   [lv] => (lv, Word_k Runtime, state)
			 | _ => let val kind = kind_tuple (map (fn _ => Word_k Runtime) lvs)
				    val reps = map (fn _ => TRACE) lvs
				    val (lv,state) = make_record(state,NONE,reps,lvs)
				in  (lv,kind,state)
				end)
		   end
*)
	     | AllArrow_c {openness=Open,...} => error "open Arrow_c"
	     | AllArrow_c {openness=Closure,eFormals=clist,fFormals=numfloat,body_type=c,...} => 
		   mk_sum_help(state,[9],[])
(*		   mk_sum_help(NONE,[9,TW32.toInt numfloat],c::clist) *)
	     | AllArrow_c {openness=Code,eFormals=clist,fFormals=numfloat,body_type=c,...} => 
		   mk_sum_help(state,[10],[])
(*		   mk_sum_help(state,[10,TW32.toInt numfloat],c::clist) *)
	     | ExternArrow_c _ =>
		   mk_sum_help(state,[11],[])
(*		   mk_sum_help(NONE,[11,TW32.toInt numfloat],c::clist) *)
	     | Var_c v => 
		   let val (vl,vv) = 
		       (case (getconvarrep state v) of
			    (_,SOME vv) => (true, VALUE vv)
			  | (SOME vl,_) => 
				let val const = (case vl of
							GLOBAL _ => true
						      | REGISTER (const,_) => const)
				in  (const, LOCATION vl)
				end
			  | (NONE,NONE) => error ("no info on convar" ^ (Name.var2string v)))
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
			   let val v = fresh_named_var (label2string l)
			       val (const',term,state) = xcon'(state,v,c)
			   in (term, (const andalso const', state))
			   end
		       val (terms,(const,state)) = foldl_list folder (true,state) lclist
		       val (result,state) = 
			   (case (!do_single_crecord,terms) of
				(true,[term]) => (term, state)
			      | _ =>
				    let val reps = map (fn _ => TRACE) terms
				    in  if const 
					    then make_record_const(state,NONE,reps,terms,NONE)
					else make_record(state,NONE,reps,terms)
				    end)
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
			       val _ = add_instr(LOAD32I(EA(ir,4 * which),dest))
			   in (const,LOCATION(REGISTER (const,I dest)), state)
			   end
		   in  (case (!do_single_crecord,Sequence.toList lvk_seq) of
			    (true,[(_,k)]) => (const,lv,state)
			  | _ => default())
		   end
	     | Closure_c (c1,c2) => 
		   let 
		       fun kinder [(_,Arrow_k(_,vklist : (var * kind) list, k)),_] = 
			   let val vklist' = butlast vklist
			   in  Arrow_k(Closure,vklist',k)
			   end
			 | kinder _ = error "bad Closure_c"
		   in  mk_sum_help(state,[],[c1,c2])
		   end
	     | Typecase_c _ => error "typecase_c not implemented"
	     | App_c (c,clist) => (* pass in env argument first *)
		   let val _ = add_instr(ICOMMENT "start making constructor call")
		       val (const_fun,lv,state) = xcon'(state,fresh_named_var "closure",c)
		       val clregi = load_ireg_term(lv,NONE)
		       val (cregsi,(const_arg,state)) = 
			   foldl_list  (fn (c,(const,state)) => 
				   let val (const',vl,state) = xcon'(state,fresh_named_var "clos_arg",c)
				   in  (load_ireg_term(vl,NONE),(const andalso const', state))
				   end) (true,state) clist
		       val const = const_fun andalso const_arg
		       val coderegi = alloc_regi NOTRACE_CODE
		       val envregi = alloc_regi TRACE
		       val _ = (add_instr(LOAD32I(EA(clregi,0),coderegi));
				add_instr(LOAD32I(EA(clregi,4),envregi)))
		       val desti = alloc_regi TRACE
		       val _ = add_instr(CALL{call_type = ML_NORMAL,
					      func = REG' coderegi,
					      args = (map I cregsi) @ [I envregi],
					      results = [I desti],
					      save = getLocals()})
		       val state = new_gcstate state
		       val _ = add_instr(ICOMMENT "done making constructor call")
		   in (const,LOCATION (REGISTER (const,I desti)),state)
		   end
	     | Annotate_c (_,c) => xcon'(state,name,c))
      end

  

  local 
      fun doconfun is_top (state,vname,vklist,body) =
	  let 
	      val name = (case (Name.VarMap.find(!exports,vname)) of
			      NONE => LOCAL_CODE (Name.var2string vname)
			    | SOME [] => error "export has no labels"
			    | SOME (l::_) => l)
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
			    val s' = add_conterm (s,v,k,SOME(LOCATION(REGISTER (false,I r))))
			in  (r,s')
                        end
	      val (cargs,state) = foldl_list folder state vklist
	      val args = map I cargs
	      val return = alloc_regi NOTRACE_CODE
	      val _ = set_args(args, return)
	      val state = needgc(state,IMM 0)
	      val (ir,state) = xcon(state,fresh_named_var "result",body)
	      val result = getResult(fn() => I ir)
	      val I resulti = result
	      val _ = (add_instr(MV(ir,resulti));
		       add_instr(RETURN return))
	      val p = get_proc()
	  in add_proc p
	  end
     fun dofun_help is_top (state,vname,Function{tFormals=vklist,
						 eFormals=vclist,
						 fFormals=vflist,body,...}) =
	  let 
	      val name = (case (Name.VarMap.find(!exports,vname)) of
			      NONE => LOCAL_CODE (Name.var2string vname)
			    | SOME [] => error "export has no labels"
			    | SOME (l::_) => l)
	      val _ = reset_state(is_top, (vname, name))
	      val _ = msg ("-----dofun_help : " ^ (Pprtl.label2s name) ^ "\n")
              fun folder ((v,k),s) = 
		  let val r = alloc_named_regi v TRACE
		      val s' = add_conterm (s,v,k,SOME(LOCATION(REGISTER (false,I r))))
		  in  (r,s')
		  end
	      val (cargs,state) = foldl_list folder state vklist
              fun folder ((v,tr,c),s) = let val ir = alloc_named_regi v (niltrace2rep s tr)
					 val s' = add_reg (s,v,c,I ir)
			             in  (ir, s')
                                     end
	      val (eiargs,state) = foldl_list folder state vclist
              fun folder (v,s) = let val fr = alloc_named_regf v
					       val s' = add_reg (s,v,Prim_c(Float_c Prim.F64,[]), F fr)
					   in  (fr,s')
                                           end
              val (efargs,state) = foldl_list folder state vflist

	      val args = (map I cargs) @ (map I eiargs) @ (map F efargs)
	      val return = alloc_regi NOTRACE_CODE
	      val _ = set_args(args, return)
	      val state = needgc(state,IMM 0)
	      val (r,state) = xexp'(state,fresh_named_var "result",body,
				      Nil.TraceUnknown, ID return)
	      val result = getResult(fn() => r)
	      val results = (case result of
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


  (* unitname is the name of the unit; unit names are globally unique. *)

   fun translate (unitname:string) trans_params (Nil.MODULE{bnds : bnd list,
					  imports : import_entry list,
					  exports : export_entry list}) =
         let 

	     val _ = msg "tortl - entered translate\n"

	     (* To reduce unnecessary labels, we can determine top-level locals.
	        These are variables bound and only used outside functions.
		For now, to avoid an initial pass, we leave it empty
	     *)
	     val toplevel_locals = VarSet.empty

	     local
		 fun help l = ML_EXTERN_LABEL(Name.label2string l)
		 fun mapper (ExportValue(l,v)) = (v, help l)
		   | mapper (ExportType(l,v)) = (v, help l)
	     in  val named_exports = map mapper exports
	     end
	     val mainCodeVar = Name.fresh_named_var("main_" ^ unitname ^ "_code")
	     val mainCodeName = ML_EXTERN_LABEL("main_" ^ unitname ^ "_code")
	     val mainName = ML_EXTERN_LABEL("main_" ^ unitname ^ "_doit")
	     val _ = resetDepth()
	     val _ = resetWork()
	     val _ = reset_global_state ((mainCodeVar,mainCodeName)::named_exports,
					 toplevel_locals)
		 
	     val exp = Let_e(Sequential, bnds,NilUtil.true_exp)
	     val con = NilUtil.bool_con

	     (* set the translation parameters *)
	     val _ = cur_params := trans_params
	     val _ = (case trans_params of
			  {HeapProfile = SOME c, ...} => (HeapProfile := true;
							  SetHeapProfileCounter c)
			| {HeapProfile = NONE, ...} => HeapProfile := false)

		 
	     val _ = msg "tortl - handling imports now\n"

	     val _ = reset_state(true, (mainCodeVar, mainCodeName))
	     fun folder (ImportValue(l,v,tr,c),s) = 
		 (* For extern or C functions, 
		    the label IS the code value rather than an address containing the code value *)
		 let val mllab = ML_EXTERN_LABEL(Name.label2string l)
		     (* XXX not implemented fully *)
		     val rep = (case tr of
				  TraceUnknown => error "tortl- Import has TraceUnknown - not implemented"
				| _ => niltrace2rep s tr)
		     val lv = 			 
			 (case c of
			      ExternArrow_c _ => VALUE(CODE mllab)
			    | _ => LOCATION(GLOBAL(mllab,rep)))
		 in  add_global(s,v,c,lv)
		 end
	       | folder (ImportType(l,v,k),s) = 
		 let val vl = (GLOBAL(ML_EXTERN_LABEL(Name.label2string l),TRACE))
		 in  add_conglobal (s,v,k, SOME(LOCATION vl))
		 end
	     val state = needgc(make_state(),IMM 0)
	     val state = foldl folder state imports
	     val return = alloc_regi NOTRACE_CODE
	     val args = []
	     val _ = set_args(args, return)
	     val (r,state) = xexp'(state,fresh_named_var "result",exp,
				      Nil.TraceUnknown, ID return)
	      val result = getResult(fn() => r)
	      val _ = (add_instr (mv(r,result));
		       add_instr(RETURN return))
	      val p = get_proc()
	     val _ = add_proc p

	     val _ = msg "tortl - calling worklist now\n"
	     val _ = worklist_loop()
	     val _ = msg "tortl - returned from worklist now\n"

	     val _ = add_data(COMMENT("Module closure"))
	     val _ = add_data(DLABEL(mainName))
	     val _ = add_data(DATA(mainCodeName))
	     val _ = add_data(INT32(0w0))
	     val _ = add_data(INT32(0w0))

	     val procs = rev (!pl)
	     val module = Rtl.MODULE {procs = procs,
				      data = rev(!dl),
				      main= mainName,
				      mutable = get_mutable()}

	     val _ = resetDepth()
	     val _ = resetWork()
	     val _ = reset_global_state ([], Name.VarSet.empty)

	 in module
	 end

     fun entryTables nl =
	 let val nl = map (fn ML_EXTERN_LABEL str => str
	                    | _ => error "bad RTL label") nl
	     val count = length nl
             fun mktable(name,suffix) =
		 DLABEL (ML_EXTERN_LABEL name) ::
		 map (fn s => DATA(ML_EXTERN_LABEL (s ^ suffix))) nl
	     val gc_table_begin =    mktable("GCTABLE_BEGIN_VAL","_GCTABLE_BEGIN_VAL")
	     val gc_table_end =	     mktable("GCTABLE_END_VAL","_GCTABLE_END_VAL")
	     val sml_globals =       mktable("SML_GLOBALS_BEGIN_VAL","_SML_GLOBALS_BEGIN_VAL")
	     val end_sml_globals =   mktable("SML_GLOBALS_END_VAL","_SML_GLOBALS_END_VAL")
	     val muttable_begin =    mktable("MUTABLE_TABLE_BEGIN_VAL","_MUTABLE_TABLE_BEGIN_VAL")
	     val muttable_end =      mktable("MUTABLE_TABLE_END_VAL","_MUTABLE_TABLE_END_VAL")
	     val codetable_begin =   mktable("CODE_BEGIN_VAL","_CODE_BEGIN_VAL")
	     val codetable_end =     mktable("CODE_END_VAL","_CODE_END_VAL")
             val entrytable =        mktable("client_entry","")
	     val count = [DLABEL (ML_EXTERN_LABEL "module_count"),
			  INT32 (TilWord32.fromInt count)]
	     val data = count @
		 gc_table_begin @
		 gc_table_end @
		 sml_globals @
		 end_sml_globals @
		 muttable_begin @
		 muttable_end @
		 codetable_begin @
		 codetable_end @
		 entrytable
	 in  MODULE{procs = [], data = data,
		    main = ML_EXTERN_LABEL "linkfile", 
		    mutable = []}
         end

end
