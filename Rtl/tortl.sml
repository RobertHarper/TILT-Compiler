(*$import TortlVararg Rtl Pprtl TortlSum TortlArray TortlBase Rtltags Nil NilUtil Ppnil Stats TraceOps NilContext TORTL *)

(* empty records translate to 256; no allocation *)
(* to do: strive for VLABEL not VGLOBAL *)
(* xtagsum_dynamic record case is fragile *)

(* A state contains classification information(type/kind),
                    residence information(register/global label),
		    and value information(int/label).
		    Only one of the latter two must be present.
   There is a current state and a global state.
   The latter contains only variables that have to be globally visible,
      This is a subset of variables bound at the top-level (outside any lambda).
      It is a subset because we would like unreferenced top-level variables to reside
      in registers, thus reducing a proliferation of global labels. 
      However, we consider all top-level type variables (say a) to be global even if not
      apparently used inside a lambda since a type reduction of a type term (b = {l = a})
      containing another global type variable(b) may yield to this type variable a.
      All imported variables are both top-level and global.  
      The check for global and top-level variables must also consider exports.
*)
   

structure Tortl :> TORTL =
struct

  val diag = ref false
  val debug = ref false
  val debug_full_when = ref 99999
  val curfun = ref 0
  fun debug_full() = !debug_full_when <= !curfun
  val debug_full_return = ref false
  val debug_full_env = ref false
  val debug_simp = ref false
  val debug_bound = ref false
      
  (* Module-level declarations *)
  open Util Listops Name
  open Nil NilUtil
  open Rtl Rtltags Pprtl TortlBase

    val do_vararg = Stats.bool("do_vararg") (* initialized elsewhere *)
    val show_cbnd = Stats.ff("show_cbnd")

    val exncounter_label = ML_EXTERN_LABEL "exncounter"
    val error = fn s => (Util.error "tortl.sml" s)
    structure TW32 = TilWord32
    structure TW64 = TilWord64
    val w2i = TW32.toInt
    val i2w = TW32.fromInt;

    val con_depth = ref 0
    val exp_depth = ref 0
    fun resetDepth() = (con_depth := 0;
			   exp_depth := 0)

      
   type translate_params = { HeapProfile : int option, do_write_list : bool, 
                             codeAlign : Rtl.align, FullConditionalBranch : bool, 
                             elim_tail_call : bool, recognize_constants : bool }
   val cur_params = ref { HeapProfile = NONE : int option, 
			 do_write_list = true,
			 codeAlign = Rtl.OCTA, FullConditionalBranch = false, 
			 elim_tail_call = true, recognize_constants = true}


   datatype work = FunWork of (state * var * function)
                 | ConFunWork of (state * var * (var * kind) list * con * kind)
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
	      Con_b (phase,cbnd) => xconbnd state (phase,cbnd)
	    | Exp_b (v,t,e) => 
		  let val c = type_of state e
		      val (loc_or_val,_,state) = xexp(state,v,e,SOME t,NOTID)
		  in  (case (istoplevel(),loc_or_val) of
			   (true, _)           => add_global (state,v,c,loc_or_val)
			 | (false, VAR_LOC vl) => add_var    (state,v,c,SOME vl,NONE)
			 | (false, VAR_VAL vv) => add_var    (state,v,c,NONE, SOME vv))
		  end
	    | Fixopen_b var_fun_seq => error "no open functions permitted"
	    | Fixcode_b var_fun_seq =>
		  let 
		      fun folder ((v,f as Function(effect,recur,vklist,_,vclist,vflist,b,c)),s) =
			  let val funcon = NilUtil.function_type Code f
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
		      val var_vcelist = Sequence.toList var_varconexpset
		      val _ = add_instr(ICOMMENT ("allocating " ^ 
						  (Int.toString (length var_vcelist)) ^ " closures"))
		      val toplevel = istoplevel()
		      fun folder((v,{code,cenv,venv,tipe}),state) = 
			  add_global(state,v,tipe,VAR_VAL(VLABEL(LOCAL_DATA(Name.var2string v))))
		      val state = if toplevel then foldl folder state var_vcelist else state
		      fun loadcl ((v,{code,cenv,venv,tipe}),state) = 
			  let val (code_lv,_,state) = xexp(state,fresh_named_var "codereg",
							   Var_e code,NONE,NOTID)
			      val (_,con_lv,_,state) = xcon'(state,fresh_named_var "cenv", cenv,NONE)
			      val (exp_lv,_,state) = 
				  (case (toplevel,is_recur) of
				       (true,_) => xexp(state,fresh_named_var "venv",venv,NONE,NOTID)
				     | (_,true) => (VAR_VAL(VTAG uninit_val),Crecord_c[],state)
				     | (_,false) => xexp(state,fresh_named_var "venv",venv,NONE,NOTID))
			      val vls = [code_lv,
					 con_lv,
					 exp_lv]
			      val reps = [NOTRACE_CODE, TRACE, TRACE]
			      val (lv,state) = 
				  (case (toplevel,is_recur) of
				       (true,_) => make_record_const(state,NONE,reps,vls,
								     SOME(LOCAL_DATA(Name.var2string v)))
				     | (_,true) => make_record_mutable(state,NONE,reps,vls)
				     | (_,false) => make_record(state,NONE,reps,vls))
			      val ir = load_ireg_locval(lv,NONE)
			      val s' = if toplevel then state else add_reg (state,v,tipe,I ir)
			  in  (ir,s')
			  end
		      val (clregsi,rec_state) = foldl_list loadcl state var_vcelist
		      fun dowrite (clregi, (_,{code,cenv,venv,tipe}),s) = 
				   let val (I ir,_,s) = xexp'(s,fresh_named_var "venv", venv, NONE, NOTID)
				   in  (add_instr(INIT(EA(clregi,8), ir, NONE)); s)
				   end
		      val _ = if is_recur 
				  then (foldl2 dowrite rec_state (clregsi, var_vcelist); ())
			      else ()
		      val _ = add_instr(ICOMMENT ("done allocating " ^ 
						  (Int.toString (length var_vcelist)) ^ " closures"))
		  in  rec_state
		  end)

  

  and xconbnd state (phase, cbnd : conbnd) : state = 
      (case cbnd of
	   Con_cb (v,c) => let 
			       val (lv,k,state) = 
				   if (phase = Compiletime)
				   then (VAR_VAL(VINT 0w1729), Type_k, state)
				   else
				       let val _ = if (!show_cbnd)
						     then (print "Con_cb: "; Ppnil.pp_var v;
							   print " = "; Ppnil.pp_con c; print "\n")
						   else ()
					   val (_,lv,k,state) = xcon'(state,v,c,NONE)
				       in  (lv,k,state)
				       end
			   in  case (istoplevel(),lv) of
			       (true,_) => add_conglobal "0" (state,v,k,SOME c, lv)
			     | (_,VAR_LOC vl) => add_convar "3" (state,v,k, SOME c,SOME vl,NONE)
			     | (_,VAR_VAL vv) => add_convar "4" (state,v,k, SOME c,NONE,SOME vv)
			   end
	 | Code_cb (conwork as (name,vklist,c,k)) => 
			     let val funkind = Arrow_k(Code,vklist,k)
				 val funcon = Let_c(Sequential,[cbnd],Var_c name)
				 val l = LOCAL_CODE(Name.var2string name)
				 val state = add_concode "5" (state,name,funkind, SOME funcon,l)
				 val _ = if phase = Compiletime
					     then ()
					 else addWork (ConFunWork(promote_maps state,name,vklist,c,k))
			     in  state
			     end
	 | Open_cb _ => (print "open Fun_cb:\n";
			     Ppnil.pp_conbnd cbnd; print "\n";
			     error "open Fun_cb"))

  and xconst (state : state, arg_v : (con,exp) Prim.value) 
      : loc_or_val * con * state =
      let
	  open Prim
	  open TW64
	  fun xvector (c,a : exp Array.array) : loc_or_val * con * state =
	      let 
		  val label = fresh_data_label "string"
		  val sz = Array.length a
		  val (state,vl) = (Array.foldr (fn (e,(state,vls)) => 
					 let val (a,b,state) = xexp(state,Name.fresh_var(),e,NONE,NOTID)
					 in  (state,(a,b)::vls)
					 end)
				    (state,[]) a)
		  fun layout segsize packager = 
		  let fun pack [] = ()
			| pack acc = packager(rev acc)
		      fun loop ls 0 acc = (pack acc; loop ls segsize [])
			| loop [] remain acc = pack acc
			| loop (a::rest) remain acc = loop rest (remain-1) (a::acc)
		  in  loop vl segsize []
		  end
		  fun getword(VAR_VAL(VINT w), _) = w
		    | getword vl = (print "bad value in vector: ";
				    print "\n";
				    error "bad string")
		  fun boxfloat_packager [(VAR_VAL(VREAL l),_)] = add_data(DATA l)
		    | boxfloat_packager [(VAR_VAL(VLABEL l),_)] = add_data(DATA l)
		    | boxfloat_packager _ = error "did not receive 1 boxed float"
		  fun word_packager [w] = add_data(INT32(getword w))
		    | word_packager _ = error "did not receive 1 word"
		  fun char_packager vals = 
		      let val (a,b,c,d) = (case (map getword vals) of
					       [a,b,c,d] => (a,b,c,d)
					     | [a,b,c] => (a,b,c,TW32.zero)
					     | [a,b] => (a,b,TW32.zero,TW32.zero)
					     | [a] => (a,TW32.zero,TW32.zero,TW32.zero)
					     | _ => error "did not receieve 1 to 4 characters")
			  val b = TW32.lshift(b,8)
			  val c = TW32.lshift(c,16)
			  val d = TW32.lshift(d,24)
		      in  add_data(INT32 (TW32.orb(TW32.orb(a,b),TW32.orb(c,d))))
		      end
		  fun general_packager [(VAR_VAL vv,_)] = 
		        (case vv of
			     (VINT w) => add_data(INT32 w)
			   | (VTAG w) => add_data(INT32 w)
			   | (VREAL l) => add_data(DATA l)
			   | VRECORD (l,_) => add_data(DATA l)
			   | (VVOID _) => error "got a vvoid in xvector"
			   | (VLABEL l) => add_data(DATA l)
			   | (VCODE l) => add_data(DATA l))
		    | general_packager [_] = error "did not receive a var_val"
		    | general_packager _ = error "did not receive 1 value"
		      

		  val c = #2(simplify_type state c)

		  val shift_amount = (case c of
					  Prim_c(Int_c Prim.W8, []) => 0
					| Prim_c(Int_c Prim.W32, []) => 2
					| Prim_c(BoxFloat_c Prim.F64, []) => 2
					| _ => 2)
		  val tagword = TW32.orb(TW32.lshift(TW32.fromInt sz,
						     shift_amount + int_len_offset),intarray)
		  val _ = add_data(COMMENT "static vector/array tag")
		  val _ = add_data(INT32 tagword)
		  val _ = add_data(DLABEL label)
		  val _ = (case c of
			       Prim_c(Int_c Prim.W8, []) => layout 4 char_packager
			     | Prim_c(Int_c Prim.W32, []) => layout 1 word_packager
			     | Prim_c(BoxFloat_c Prim.F64, []) => layout 1 boxfloat_packager
			     | _ => layout 1 general_packager)
	      in  (VAR_VAL(VLABEL label), Prim_c(Vector_c, [c]), state)
	      end
      in
	  (case arg_v of
	       uint (W64, _) => error "64-bit ints not done"
	     | int (W64, _) => error "64-bit ints not done"
	     | uint (ws, w64) =>
		   let val w32 = TW64.toUnsignedHalf w64
		   in  (VAR_VAL(VINT w32), Prim_c(Int_c ws, []),state)
		   end
	     | int (ws, w64) =>
		   let val w32 = TW64.toUnsignedHalf w64
		   in  (VAR_VAL(VINT w32), Prim_c(Int_c ws, []),state)
		   end
	      | (float (F64, s)) => (VAR_VAL(VREAL (mk_float_data s)), Prim_c(Float_c F64, []),state)
	      | (float (F32, _)) => error "32 bit floats not done"
	      | (vector (c,a)) => xvector(c,a)
	      | (array _)  => error "array/vector/refcell constants not implemented"
	      | refcell _ => error "array/vector/refcell constants not implemented"
	      | (tag(t,c)) => let val i = TW32.fromInt (tag2int t)
			      in  (VAR_VAL(VINT i), Prim_c(Int_c W32, []),state)
			      end)
      end



  and xexp' (state : state, (* state of bound variables *)
	     name : var, (* Purely for debugging and generation of useful names *)
	     e : exp,    (* The expression being translated *)
	     traceinfo_opt : niltrace option,  (* The type of the expression being translated *)
	     context     (* The evaluation context this expression is in *)
	     ) : reg * con * state =
      (case xexp(state,name,e,traceinfo_opt,context) of
	   (VAR_LOC var_loc, c, s) => (load_reg_loc(var_loc,NONE),c, s)
	 | (VAR_VAL var_val, c, s) => (load_reg_val(var_val,NONE),c, s))

  and xexp_list (state,elist) : loc_or_val list * state = 
      let fun folder(e,state) = let val (lv,c,s) = xexp(state,fresh_var(),e,NONE,NOTID)
				in  (lv,s)
				end
      in  foldl_acc folder state elist
      end

  and xexp (state : state, (* state of bound variables *)
	    name  : var, (* Purely for debugging and generation of useful names *)
	    arg_e : exp,         (* The expression being translated *)
	    traceinfo_opt  : niltrace option,  (* The type of the expression being translated *)
	    context     (* The evaluation context this expression is in *)
	    ) : loc_or_val * con * state =
      let 
	  val _ = exp_depth := !exp_depth + 1
	  val _ = if (!debug)
		      then (print "xexp ";  print (Int.toString (!exp_depth));
			    print " called";
			    if (debug_full())
				then (print " on exp = \n";
				      Ppnil.pp_exp arg_e)
			    else ();
			    print "\n")
		  else ()  
	  val res = xexp''(state,name,arg_e,traceinfo_opt,context)
	  val _ = if (!debug)
		      then (print "xexp ";  print (Int.toString (!exp_depth));
			    print " returned \n";
	                    if (debug_full() andalso !debug_full_return)
				then (print"with con = \n";
				      Ppnil.pp_con (#2 res);
				      print "\n")
			    else ())
		  else ()
	  val _ = exp_depth := !exp_depth - 1
	      
      in  res
      end

  and xexp'' (state : state, (* state of bound variables *)
	    name  : var, (* Purely for debugging and generation of useful names *)
	    arg_e : exp,         (* The expression being translated *)
	    traceinfo_opt  : niltrace option,  (* The type of the expression being translated *)
	    context     (* The evaluation context this expression is in *)
	    ) : loc_or_val * con * state =
      let 
	  fun pickdesti rep = alloc_named_regi name rep
	  fun pickdestf () = alloc_named_regf name
	  val res = 
	  case arg_e of
	      Var_e v => (case (getrep state v) of
			      (_,SOME vv,c) => (VAR_VAL vv, c, state)
			    | (SOME vl,_,c) => (VAR_LOC vl, c, state)
			    | (NONE,NONE,_) => error "no info on var")
	    | Const_e v => xconst(state,v)
	    | Let_e (_, [], body) => xexp(state,name,body,traceinfo_opt,context)
	    | Let_e (_, bnds, body) => 
		  let val first_bnds = Listops.butlast bnds
		      val last_bnd = List.last bnds
		      fun folder (bnd,s) = xbnd s bnd
		      val state = foldl folder state first_bnds
		      val (lv,c,state) = 
			  (case (last_bnd,body) of
			       (Exp_b(v,nt,e),Var_e v') => 
				   if (eq_var(v,v'))
				       then xexp(state,name,e,SOME nt,context)
				   else xexp(xbnd state last_bnd,fresh_var(),body,traceinfo_opt,context)
			     | _ => xexp(xbnd state last_bnd,fresh_var(),body,traceinfo_opt,context))
		      val cbnds = List.mapPartial (fn (Con_b (_,cb)) => SOME cb
		                                    | _ => NONE) bnds
		      val c' = Let_c(Sequential,cbnds,c)
		  in  (lv,c',state)
		  end
	    | Prim_e (NilPrimOp nilprim, clist, elist) => xnilprim(state,nilprim,clist,elist,
								   context,traceinfo_opt)
	    | Prim_e (PrimOp prim, clist, elist) => xprim(state,prim,clist,elist,context)
	    | Switch_e sw => xswitch(state,name,sw,traceinfo_opt,context)
	    | ExternApp_e (f, elist) => (* assume the environment is passed in first *)
		  let 
		      val _ = add_instr (ICOMMENT ("making external call"))
		      fun cfolder (c,state) =
			  let val (res,_,state) = xcon(state,fresh_named_var "call_carg", 
						       c, NONE)
			  in  (res,state)
			  end
		      fun efolder(e,(eregs,fregs,state)) = 
			  let val (res,_,state) = xexp'(state,fresh_named_var "call_e", 
							e, NONE, NOTID)
			  in  case res of
			      I ir => (ir::eregs,fregs,state)
			    | F fr => (eregs,fr::fregs,state)
			  end

		      val (rev_iregs, rev_fregs, state) = foldl efolder ([],[],state) elist
		      val iregs = rev rev_iregs
		      val fregs = rev rev_fregs

		      val Var_e expvar = f
		      val (vlopt,vvopt,funcon) = getrep state expvar

		      val fun_reglabel = 
				(case (vlopt,vvopt) of
				     (NONE,SOME(VCODE l)) => LABEL' l
				   | (SOME(VREGISTER (_,I r)),_) => REG' r
				   | (SOME(VGLOBAL (l,_)),_) => 
					 let val addr = alloc_regi LABEL
					     val reg = alloc_regi NOTRACE_CODE
					 in  (add_instr(LADDR(l,0,addr));
					      add_instr(LOAD32I(EA(addr,0),reg));
					      REG' reg)
					 end
				   | _ => error "bad varloc or varval for function")
				     
				     
		      val (_,ExternArrow_c(_,rescon)) = simplify_type state funcon
		      val SOME niltrace = traceinfo_opt
		      val dest = alloc_reg_trace state niltrace
		      val results = (case dest of
					 F fr => ([],[fr])
				       | I ir => ([ir],[]))	      
		      val _ = add_instr(CALL{extern_call = true,
					     func=fun_reglabel,
					     return= NONE,
					     args=(iregs,fregs),
					     results=results,
					     tailcall = false,
					     save=SAVE(getLocals())})

		      val result = (VAR_LOC(VREGISTER (false,dest)), rescon,
				    new_gcstate state)
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

			  val _ = add_instr (ICOMMENT ("making " ^ call_type))


			  fun cfolder (c,state) =
			      let val (res,_,state) = xcon(state,fresh_named_var "call_carg", c, NONE)
			      in  (res,state)
			      end
			  fun efolder(e,state) = 
			      let val (res,_,state) = xexp'(state,fresh_named_var "call_e", 
							    e, NONE, NOTID)
			      in  (res,state)
			      end

			  val (cregsi,state) = foldl_list cfolder state clist
			  val (eregs, state) = foldl_list efolder state elist
			  val (efregs, state) = foldl_list efolder state eflist

			  fun direct_call expvar = 
			      let 
				  val (vlopt,vvopt,funcon) = getrep state expvar
			      in  (Name.eq_var(#1(getCurrentFun()), expvar), 
				   (case (vlopt,vvopt) of
					(NONE,SOME(VCODE l)) => LABEL' l
				      | (SOME(VREGISTER (_,I r)),_) => REG' r
				      | (SOME(VGLOBAL (l,_)),_) => 
					    let val addr = alloc_regi LABEL
						val reg = alloc_regi NOTRACE_CODE
					    in  (add_instr(LADDR(l,0,addr));
						 add_instr(LOAD32I(EA(addr,0),reg));
						 REG' reg)
					    end
				      | _ => error "bad varloc or varval for function"),
					funcon, [], [],state)
			      end

			  val (selfcall,fun_reglabel,funcon,cregsi',eregs',state) = 
			      (case (openness,f) of
				   (Code,Var_e expvar) => (direct_call expvar)
				 | (Code,_) => error "ill-formed application"
				 | (Open,_) => error "no open apps allowed"
				 | (Closure,cl) =>
				       let val (clreg,funcon,state) = xexp'(state,fresh_var(),
									    cl,NONE,NOTID)
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
				       in  (false, REG' funregi, funcon, [cregi], [I eregi],state)
				       end)


		      in
			  val selfcall = selfcall
			  val fun_reglabel = fun_reglabel
			  val cregsi = cregsi
			  val cregsi' = cregsi'
			  val eregs = eregs 
			  val eregs' = eregs'
			  val efregs = efregs
			  fun reduce(vklist,clist,rescon) = 
			      let val cbnds = map2 (fn ((v,_),c) => Con_cb(v,c)) (vklist,clist)
			      in  if (null cbnds) then rescon 
				  else NilSubst.renameCon(Let_c(Sequential,cbnds,rescon))
			      end
			  val rescon = 
			      (case (#2(simplify_type state funcon)) of
				   (Prim_c(Vararg_c _, [_,rescon])) => reduce([],clist,rescon)
				 | (AllArrow_c(_,_,vklist,_,_,_,rescon)) => reduce(vklist,clist,rescon)
				 | c => (print "cannot compute type of result of call\n";
					  print "funcon = \n"; Ppnil.pp_con funcon; print "\n";
					  print "reduced to \n"; Ppnil.pp_con c; print "\n";
					  error "cannot compute type of result of call"))
						     
		      end

		      val iregs = (cregsi @ cregsi' @ 
				   (map (coercei "call") eregs) @
				   (map (coercei "call") eregs'))

		      val fregs = map coercef efregs

		      val SOME niltrace = traceinfo_opt
		      fun thunk() = alloc_reg_trace state niltrace
		      fun do_call return_opt = 
			  let val tailcall=(case return_opt of
						       SOME _ => true
						     | NONE => false)
			      val dest = if tailcall
					     then getResult thunk
					 else thunk()
			      val results = (case dest of
						 F fr => ([],[fr])
					       | I ir => ([ir],[]))	      
			      val _ = add_instr(CALL{extern_call = false,
						     func=fun_reglabel,
						     return=return_opt,
						     args=(iregs,fregs),
						     results=results,
						     tailcall = tailcall,
						     save=SAVE(getLocals())})
			  in  dest
			  end
		      val dest = (case (context,#elim_tail_call(!cur_params),selfcall) of
					(NOTID,_,_) => do_call NONE
				      | (_, false,_) => do_call NONE
				      | (ID r,true,true) =>  
					    (shuffle_iregs(iregs,getArgI());
					     shuffle_fregs(fregs,getArgF());
					     add_instr(BR (getTop()));
					     thunk())
				      | (ID r,true,false) => do_call (SOME r))
		      val result = (VAR_LOC(VREGISTER (false,dest)), rescon,
				     new_gcstate state)
		      val _ = add_instr (ICOMMENT ("done making " ^ call_type))

		  in  result
		  end

	    | Raise_e (exp, con) =>
		  let val (I ir,_,state) = xexp'(state,name,exp,NONE,NOTID)
		      val newpc = alloc_regi LABEL
		      val rep = con2rep state con
		  in  add_instr(LOAD32I(EA(exnptr,0),newpc));
		      add_instr(MV (ir,exnarg));
		      add_instr RESTORE_CS;
		      add_instr (JMP(newpc,nil));
		      (VAR_VAL(VVOID rep), con, state)
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
				   SOME(VREGISTER (_,I (r as (REGI (_,rep))))) => loop rest 
				       (rep::irep,r::ir,fr)
				 | SOME(VREGISTER (_,I (SREGI _))) => error "SREGI free in handler!!!"
				 | SOME(VREGISTER (_,F r)) => loop rest (irep,ir,r::fr)
				  (* don't need to save globals - or varval only *)
				 | SOME(VGLOBAL _) => loop rest (irep,ir,fr)
				 | NONE => loop rest (irep,ir,fr))
		      in
			  val (local_int_reps,local_iregs, local_fregs) = loop vlopts ([],[],[])
		      end



		      val hl = fresh_code_label "exn_handler"
		      val hlreg = alloc_regi LABEL
		      val bl = fresh_code_label "exn_after"

		      val reps = (LABEL :: LABEL :: TRACE :: local_int_reps)

		      val (fpbase,state) = (* --- save the floating point values, if any *)
			  (case local_fregs of
			       [] => (NONE,state)
			     | _ => let val vv = map (fn freg => VAR_LOC(VREGISTER(false,F freg))) local_fregs
				        val (ir,state) = fparray(state,vv)
				    in  (SOME ir, state)
				    end)


		      (* --- create the exn record and set the exnptr to it to install it *)
		      val int_vallocs = (map (fn ireg => VAR_LOC(VREGISTER(false,I ireg)))
					 ([hlreg, stackptr,exnptr] @ local_iregs))
		      val _ = add_instr(LADDR(hl,0,hlreg))
		      val (_,state) = make_record(state,SOME exnptr,reps, int_vallocs)
			  

                      (* --- compute the body; restore the exnpointer; branch to after handler *)
		      (* NOTID and not context because we have to remove the exnrecord *)
		      val (reg,arg_c,state) = xexp'(state,name,exp,traceinfo_opt,NOTID)
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
		         ---    we can move the exn arg 
		         ---    we can call new_gcstate 
		         ---    note that since the exnarg and ra register are the same
		                  the exn arg must be moved before the gc_check *)
		      val xr = alloc_named_regi exnvar TRACE
		      val _ = add_instr(MV(exnarg,xr))
		      val hstate = new_gcstate state
		      val hstate = add_reg (hstate,exnvar,Prim_c(Exn_c,[]),I xr)


                      (* --- restore exnptr; compute the handler; move result into same register
                             as result reg of expression; add after label; and fall-through *)
		      val _ = add_instr(LOAD32I(EA(exnptr,8),exnptr))
		      val (hreg,_,hstate) = xexp'(hstate,name,handler_body,NONE,context)
		      val _ = (case (hreg,reg) of
				   (I hreg,I reg) => add_instr(MV(hreg,reg))
				 | (F hreg,F reg) => add_instr(FMV(hreg,reg))
				 | _ => error "hreg/ireg mismatch in handler")
		      val _ = add_instr(ILABEL bl)
		      val state = join_states[state,hstate]

		  in 
		      (* for debugging, should check that arg_c and hcon are the same *)
		      (VAR_LOC(VREGISTER (false,reg)), arg_c, state)
		  end

(*
	  val _ = (print "xexp translating: ";
		   Ppnil.pp_exp arg_e;
		   print "\nreturning a";
		   case #1 res of
		       (VAR_VAL (VINT _)) => (print "varval int\n")
		     | (VAR_VAL (VREAL _)) => (print "varval float\n")
		     | (VAR_VAL (VRECORD _)) => (print "varval record\n")
		     | (VAR_VAL (VLABEL _)) => (print "varval label\n")
		     | (VAR_VAL (VCODE _)) => (print "varval code\n")
		     | (VAR_VAL (VVOID _)) => (print "varval void\n")
		     | (VAR_LOC _) => (print "varloc\n"))
*)

      in res
      end



      (* The trick is to notice that for certain args, the comparison and computation
         can be folded into one instruction. *)
      and zero_one (state : state, r : regi, 
		    traceinfo_opt : niltrace option, zeroexp, oneexp, context) : loc_or_val * con * state = 
	  let 
	      val thenl = fresh_code_label "zero_case"
	      val elsel = fresh_code_label "one_case"
	      val afterl = fresh_code_label "after_zeroone"
	      val _ = add_instr(BCNDI(NE,r,elsel,false))
	      val _ = add_instr(ILABEL thenl)
	      val (zero,zcon,state_zero) = xexp'(state,fresh_named_var "zero_result", 
						 zeroexp, traceinfo_opt, context)
	      val SOME traceinfo = traceinfo_opt
	      val dest = alloc_reg_trace state traceinfo
	      val _ = (case (zero, dest) of 
			   (I zz, I d) => add_instr(MV (zz,d))
			 | (F zz, F d) => add_instr(FMV (zz,d))
			 | _ => error "zero_one: different arms have results in float and int registers")
	      val _ = add_instr(BR afterl)
	      val _ = add_instr(ILABEL elsel)
	      val (one,ocon,state_one) = xexp'(state,fresh_named_var "nonzero_result", oneexp, 
					       traceinfo_opt, context)
	      val state = join_states[state_zero,state_one]
	      val _ = (case (one,dest) of 
			   (I oo, I d) => add_instr(MV (oo,d))
			 | (F oo, F d) => add_instr(FMV (oo,d))
			 | _ => error "zero_one: different arms have results in float and int registers")
	      val _ = add_instr(ILABEL afterl)
	  in (VAR_LOC (VREGISTER (false, dest)), zcon,state)
	  end


  and xswitch (state : state,
	       name : var,  (* Purely for debugging and generation of useful names *)
	       sw : switch, (* The switch expression being translated *)
	       traceinfo_opt : niltrace option, (* The type of the switch expression *)
	       context      (* The evaluation context this expression is in *)
	       ) : loc_or_val * con * state =
      let
	  val rescon = ref NONE
	  val dest = ref NONE
	  val SOME traceinfo = traceinfo_opt
	  fun mv (r,c) = let val _ = (case (!dest) of
					  NONE => dest := (SOME (alloc_reg_trace state traceinfo))
					| _ => ())
			     val _ = (case !rescon of
					  NONE => rescon := SOME c
					| _ => ())
			     val d = valOf(!dest)
			 in (case (r,d) of
				 (I ir1, I ir2) => add_instr(MV(ir1,ir2))
			       | (F fr1, F fr2) => add_instr(FMV(fr1,fr2))
			       | _ => error "register mismatch")
			 end
	  fun no_match state = 
	      (case (!rescon) of
		   SOME c => let val (r,c,newstate) = xexp'(state,fresh_var(),Raise_e(NilUtil.match_exn,c),
							    NONE, context)
			     in  mv(r,c); newstate
			     end
		 | NONE => error "empty switch statement")
	  val switchcount = Stats.counter("RTLswitch")()
      in
	  case sw of
	      Intsw_e {size, arg, arms, default} => 
		  let val (r,state) = (case (xexp'(state,fresh_named_var "intsw_arg",arg,
						   NONE,NOTID)) of
				   (I ireg,_,state) => (ireg,state)
				 | (F _,_,_) => error "intsw argument in float register")
		  in  case (arms,default) of
		      ([(0w0,z)],SOME e) => zero_one(state, r, traceinfo_opt, z, e, context)
		    | ([(0w0,z),(0w1,one)],NONE) => zero_one(state, r, traceinfo_opt, z, one, context)
		    | _ => (* general case *)
			  let 
			      val afterl = fresh_code_label "after_intcase"
			      fun scan(states,lab,[]) = 
				  (add_instr(ILABEL lab);
				   case default of
				       NONE => (no_match state::states)
				     | SOME e => 
					   let val (r,c,newstate) = (xexp'(state,fresh_var(),e,
									   traceinfo_opt,context))
					   in  mv(r,c); newstate::states
					   end)
				| scan(states,lab,(i,body)::rest) =
				  let val next = fresh_code_label "intarm"
				      val test = alloc_regi(NOTRACE_INT)
				  in  add_instr(ILABEL lab);
				      (if in_imm_range i then
					  add_instr(CMPSI(EQ,r,IMM(w2i i),test))
				      else 
					  let val tmp = alloc_regi(NOTRACE_INT)
					  in add_instr(LI(i,tmp));
					      add_instr(CMPSI(EQ,r,REG tmp,test))
					  end);
				      add_instr(BCNDI(EQ,test,next,true));
				      let val (r,c,newstate) = 
					  xexp'(state,fresh_var(),body,NONE,context)
				      in  mv(r,c);
					  add_instr(BR afterl);
					  scan(newstate::states,next,rest)
				      end
				  end
			      val new_states = scan([],fresh_code_label "intarm",arms)
			      val state = join_states new_states
			  in  
			      add_instr(ILABEL afterl);
			      case (!dest,!rescon) of
				  (SOME r,SOME c) => (VAR_LOC(VREGISTER (false, r)),c,state)
				| _ => error "no arms"
			  end
		  end
	    | Exncase_e {arg, bound, arms, default} => 
		  let
		      val (I exnarg,_,state) = xexp'(state,fresh_named_var "exntsw_arg",arg,NONE,NOTID)

		      val exntag = alloc_regi(NOTRACE_INT)
		      val _ = add_instr(LOAD32I(EA(exnarg,0),exntag))
		      val afterl = fresh_code_label "after+exncase"
		      fun scan(states,lab,[]) =
			  (add_instr(ILABEL lab);
			   case default of
			       NONE => 
				   let val con = (case !rescon of
						  SOME c => c 
						| NONE => error "no arms")
				       val (r,c,newstate) = xexp'(state,fresh_var(),
								  Raise_e(arg,con),NONE,context)
				   in  mv(r,c); newstate :: states
				   end
			     | SOME e => 
				   let val (r,c,newstate) = xexp'(state,fresh_var(),e,
								  traceinfo_opt,context)
				   in  mv(r,c); newstate :: states
				   end)
			| scan(states,lab,(armtag,body)::rest) = 
			  let 
			      val _ = add_instr(ILABEL lab)
			      val (I armtagi,tagcon,state) = xexp'(state,fresh_var(),armtag,
								   NONE,NOTID)
			      val (_,Prim_c(Exntag_c,[c])) = simplify_type state tagcon
			      val next = fresh_code_label "exnarm"
			      val test = alloc_regi(NOTRACE_INT)
			      val carried = alloc_reg state c
			      val carriedi = (case carried of
						  I ir => ir
						| _ => error "carried value is an unboxed float")
			      val state = add_reg (state,bound,c,carried)
			  in  add_instr(CMPSI(EQ,exntag,REG armtagi,test));
			      add_instr(BCNDI(EQ,test,next,true));
			      add_instr(LOAD32I(EA(exnarg,4),carriedi));
			      let val (r,c,state) = xexp'(state,fresh_var(),body,
							  NONE,context)
			      in  mv(r,c);
				  add_instr(BR afterl);
				  scan(state::states,next,rest)
			      end
			  end
		      val states = scan([],fresh_code_label "exnarm",arms)
		      val state = join_states states
		  in  
		      add_instr(ILABEL afterl);
		      case (!dest,!rescon) of
			  (SOME r,SOME c) => (VAR_LOC(VREGISTER (false, r)),c,state)
			| _ => error "no arms"
		  end 
	    | Typecase_e _ => error "typecase_e not implemented"
	    | Sumsw_e {sumtype, arg, bound, arms, default} =>
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
		  (0w2,[], [(0w0,zeroexp),(0w1,oneexp)], NONE) => 
			let val (r,state) = 
			    (case (xexp'(state,fresh_named_var "intsw_arg",arg,
					 NONE,NOTID)) of
				 (I ireg,_,state) => (ireg,state)
			       | (F _,_,_) => error "intsw argument in float register")
			in  zero_one(state,r, traceinfo_opt, zeroexp, oneexp, context)
			end
		| _ =>
		  let val (I r,_,state) = xexp'(state,fresh_named_var "sumsw_arg",arg,NONE,NOTID)
		      val afterl = fresh_code_label "after_sum"
		      val nomatchl = fresh_code_label "nomatch_sum"
		      val one_carrier = (length cons) = 1
		      val total = TW32.uplus(tagcount, i2w(length cons))
		      val exhaustive = 
			  let val handled = map (fn (w,_) => (w2i w)) arms
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
					 let val (r,c,state) = xexp'(state,fresh_var(),e,
								     traceinfo_opt,context)
					 in  mv(r,c); state::newstates
					 end))
			| scan(newstates,lab,(i,body)::rest) =
			  let val next = fresh_code_label "sumarm"
			      val test = alloc_regi(NOTRACE_INT)
			      val _ = add_instr(ILABEL lab)
			      val state =
				  if (TW32.ult(i,tagcount))
				      then state
				  else add_reg (state,bound,spcon i, I r)
			       (* perform check and branch to next case *)
			      fun check lbl cmp i tag = (if in_imm_range i
						     then add_instr(CMPSI(cmp,tag,IMM(w2i i),test))
						 else 
						     let val tmp = alloc_regi(NOTRACE_INT)
						     in  add_instr(LI(i,tmp));
							 add_instr(CMPSI(cmp,tag,REG tmp,test))
						     end;
						  add_instr(BCNDI(EQ,test,lbl,true)))
			      val check_ptr_done = ref (TW32.equal(tagcount,0w0))
			      val load_tag_done = ref false
			      fun check_ptr() = 
				  (if (!check_ptr_done) 
				       then () 
				   else check nomatchl GT 0w255 r;
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
				| (_,true) => check next EQ i r
				| (_,false) => (if exhaustive then () else check_ptr();
						if one_carrier
						    then ()
						else (load_tag();
						     check next EQ (TW32.uminus(i,tagcount)) tag)));
			      let val (r,c,state) = xexp'(state,fresh_var(),body,
							  NONE,context)
			      in mv(r,c);
				  add_instr(BR afterl);
				  scan(state::newstates,next,rest)
			      end
			  end
		      val states = scan([],fresh_code_label "sumarm",arms)
		      val state = join_states states
		  in  
		      add_instr(ILABEL afterl);
		      case (!dest,!rescon) of
			  (SOME r,SOME c) => (VAR_LOC(VREGISTER(false,r)),c,state)
			| _ => error "no arms"
		  end)
	      end
      end





  and xnilprim(state : state, nilprim,clist,elist,context,traceinfo_opt) : loc_or_val * con * state = 
      let fun error' s = (print "NIL primexpression was:\n";
			  Ppnil.pp_exp (Nil.Prim_e(Nil.NilPrimOp nilprim, clist,elist));
			  print "\n";
			  error s)
      in
      (case nilprim of 
	   Nil.record labels => 
	       let fun folder(e,state) = 
		     let val (vl,c,state) = xexp(state,fresh_var(), e,NONE,NOTID)
		     in  ((vl,c),state)
		     end
		   val (vallocs_types,state) = foldl_list folder state elist
		   val types = map #2 vallocs_types
		   val vallocs = map #1 vallocs_types
		   val c = Prim_c(Record_c (labels,NONE), types)
		   val reps = map valloc2rep vallocs
		   val (lv,state) = make_record(state,NONE,reps,vallocs)
	       in  (lv, c, state)
	       end
	 | select label => 
	       let 
		   val [e] = elist 
		   val (I addr,reccon,state) = xexp'(state,fresh_var(),e,NONE,NOTID)
		   val (_,Prim_c(Record_c (labels,_),fieldcons)) = simplify_type state reccon
		   fun loop [] _ n = error' "bad select 1"
		     | loop _ [] n = error' "bad select 2"
		     | loop (l1::lrest) (c1::crest) n = if (Name.eq_label(l1,label))
							    then (n,c1)
							else loop lrest crest (n+1)
		   val (which,con) = loop labels fieldcons 0
		   val I desti = (case traceinfo_opt of
				      SOME traceinfo => alloc_reg_trace state traceinfo
				    | _ => alloc_reg state con)
		   val _ = add_instr(LOAD32I(EA(addr,which * 4), desti))
	       in  (VAR_LOC(VREGISTER(false, I desti)), con, state)
	       end
	 | inject_record known => 
		let val (lvs,state) = xexp_list (state,elist)
		in  TortlSum.xsum_record ((state,known,hd clist),lvs)
		end

	 | inject_nonrecord known => 
		let val (lvopt,state) = 
		    (case elist of
			 [] => (NONE,state)
		       | [e] => let val (lv,_,state) = xexp(state,fresh_var(),hd elist,NONE,NOTID)
				in  (SOME lv,state)
				end)
		in  TortlSum.xsum_nonrecord ((state,known,hd clist),lvopt)
		end
	 | inject known => 
		let val (e_lv,c,state) = xexp(state,fresh_var(),hd elist,NONE,NOTID)
		    val (_,c_lv,_,state) = xcon'(state,fresh_var(),hd clist,NONE)
		in  TortlSum.xsum_dynamic ((state,known,hd clist),c_lv,e_lv)
		end
	 | project_sum_record (k,field) => 
	       let val SOME traceinfo = traceinfo_opt
		   val e = hd elist
		   val (r,econ,state) = xexp'(state,fresh_var(),e,NONE,NOTID)
		   val base = coercei "" r
	       in  TortlSum.xproject_sum_record ((state,k,econ),field,clist,base,traceinfo)
	       end

	 | project_sum_nonrecord k =>
	       let val SOME traceinfo = traceinfo_opt
		   val sumcon = hd clist
		   val e = hd elist
		   val (r,ssumcon,state) = xexp'(state,fresh_var(),e,NONE,NOTID)
		   val base = coercei "" r
	       in  TortlSum.xproject_sum_nonrecord ((state,k,sumcon),base,ssumcon,traceinfo)
	       end

	 | project_sum k =>
	       let val SOME traceinfo = traceinfo_opt
		   val sumcon = hd clist
		   val e = hd elist
		   val (er,ssumcon,state) = xexp'(state,fresh_var(),e,NONE,NOTID)
		   val (cr,_,state) = xcon(state,fresh_var(),sumcon,NONE)
		   val base = coercei "" er
	       in  TortlSum.xproject_sum_dynamic ((state,k,sumcon),cr,base,traceinfo)
	       end

	 | box_float Prim.F64 => 
	       let val [e] = elist
		   val (lv,_,state) = xexp(state,fresh_var(),e,NONE,NOTID)
		   val (vl,state) = boxFloat_vl(state,lv)
	       in (vl, Prim_c(BoxFloat_c Prim.F64,[]),state)
	       end
	 | unbox_float Prim.F64 => 
	       let val [e] = elist
		   val (I ir,_,state) = xexp'(state,fresh_var(),e,NONE,NOTID)
		   val fr = alloc_regf()
		   val _ = add_instr(LOADQF(EA(ir,0),fr))
	       in (VAR_LOC(VREGISTER(false, F fr)), Prim_c(Float_c Prim.F64,[]),state)
	       end
	 | box_float Prim.F32 => error "32-bit floats not done"
	 | unbox_float Prim.F32 => error "32-bit floats not done"
	 | roll => let val ([c],[e]) = (clist,elist) 
	               (* the type returned is not right *)
		   in  xexp(state,fresh_var(),e,NONE,context)
		   end
	 | unroll => 
		   let val ([c],[e]) = (clist,elist)
		       (* the type returned is not right *)
		   in  xexp(state,fresh_var(),e,NONE,context)
		   end
	 | make_exntag => 
		   let val [c] = clist 
		       val c' = Prim_c(Exntag_c,[c])
		       val desti = alloc_regi NOTRACE_INT
		       val addr = alloc_regi TRACE
		       val tmp = alloc_regi NOTRACE_INT
		       val _ = (add_instr(LADDR(exncounter_label,0,addr));
				add_instr(LOAD32I(EA(addr,0),desti));
				add_instr(ADD(desti,IMM 1,tmp));
				add_instr(STORE32I(EA(addr,0),tmp)))
		   in  (VAR_LOC(VREGISTER (false, I desti)), c',state)
		   end
	 | inj_exn name => 
		   let val [e1,e2] = elist
		       val desti = alloc_regi NOTRACE_INT
		       val (vl1,_,state) = xexp(state,fresh_var(),e1,NONE,NOTID)
		       val (vl2,_,state) = xexp(state,fresh_var(),e2,NONE,NOTID)
		       fun char2val c = Const_e(Prim.uint(Prim.W8, TW64.fromInt (ord c)))
		       val name_array = Array.fromList (map char2val (explode name))
		       val (vl3,_,state) = xconst(state, Prim.vector(char_con,name_array))
		       val vallocs = [vl1,vl2,vl3]
		       val reps = map valloc2rep vallocs
		       val (lv,state) = make_record(state,NONE,reps,vallocs)
		   in  (lv, Prim_c(Exn_c,[]),state)
		   end
	 | make_vararg oe => 
		   let val local_xexp = fn (s,e) => coercei "vararg" (#1(xexp'(s,fresh_var(),e,NONE,NOTID)))
		       val [c1,c2] = clist
		       val (argc, _, state) = xcon(state,fresh_var(),c1, NONE)
		       val (resc, _, state) = xcon(state,fresh_var(),c2, NONE)
		       val (I function,c,state) = xexp'(state,fresh_var(),hd elist,NONE, NOTID)
		       val (state,resulti) = TortlVararg.xmake_vararg local_xexp (state,argc,resc,function)
		   in  (VAR_LOC(VREGISTER(false, I resulti)), 
			Prim_c(Vararg_c oe, clist), 
			state)
		   end
	 | make_onearg (openness,eff) => 
		   let val local_xexp = fn (s,e) => coercei "onearg" (#1(xexp'(s,fresh_var(),e,NONE,NOTID)))
		       val [c1,c2] = clist
		       val (argc, _, state) = xcon(state,fresh_var(),c1, NONE)
		       val (resc, _, state) = xcon(state,fresh_var(),c2, NONE)
		       val (I function,c,state) = xexp'(state,fresh_var(),hd elist,NONE, NOTID)
		       val (state,resulti) = TortlVararg.xmake_onearg local_xexp (state,argc,resc,function)
		   in  (VAR_LOC(VREGISTER(false, I resulti)), 
			AllArrow_c(openness,eff,[],NONE,[c1],0w0,c2),
			state)
		   end
	 | peq => error "peq not done")
      end



  and xprim(state,Prim.neg_int is,clist,elist,context) =
          xprim(state, Prim.minus_int is, clist, (Const_e(Prim.int(Prim.W32,TW64.zero)))::elist,context)
    | xprim(state : state, prim,clist,elist,context) : loc_or_val * con * state = 
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
	  fun commute (v1 as (VAR_VAL(VINT i)),v2) = 
	      if in_imm_range i then (v2,v1) else (v1,v2)
	    | commute arg = arg
	  (* ----------- integer comparisons ----------------------- *)
	  fun flip EQ = EQ
	    | flip LE = GE
	    | flip LT = GT
	    | flip GE = LE
	    | flip GT = LT
	    | flip NE = NE
	    | flip LBS = error "flip: LBS shouldn't be here"
	    | flip LBC = error "flip: LBC shouldn't be here"
	  fun swap (a,v1 as VAR_VAL(VINT i),v2) = 
	      if in_imm_range i 
		  then (flip a,v2,v1)
	      else (a,v1,v2)
	    | swap arg = arg
          fun stdcmp2i signed oper =
                   let val [vl1,vl2] = vl_list
		       val (oper,vl1,vl2) = swap(oper,vl1,vl2)
		       val a' = load_ireg_locval(vl1,NONE)
		       val b' = load_ireg_sv vl2
		       val dest = alloc_regi NOTRACE_INT
		       val cmp = if signed then CMPSI else CMPUI
		       val _ =  add_instr(cmp(oper,a',b',dest))
		   in (VAR_LOC(VREGISTER(false, I dest)),bool_con, state)
		   end

          val stdcmp2si = stdcmp2i true
          val stdcmp2ui = stdcmp2i false

          (* ----------- floatint point comparison ---------------- *)
	  fun cmpf oper =
	      let val [vl1,vl2] = vl_list
		  val a' = load_freg_locval(vl1,NONE)
		  val b' = load_freg_locval(vl2,NONE)
		  val dest = alloc_regi NOTRACE_INT
		  val _ =  add_instr(CMPF(oper,a',b',dest))
	      in (VAR_LOC(VREGISTER(false, I dest)),bool_con,state)
	      end


	  (* ----------- unary integer operations ----------------- *)
	  fun op1i oper : loc_or_val * con * state =
	      let val [vl1] = vl_list
		  val a' = load_ireg_locval(vl1,NONE)
		  val dest = alloc_regi NOTRACE_INT
		  val _ = add_instr(oper(a',dest))
	      in (VAR_LOC(VREGISTER(false, I dest)), int32, state)
	      end

	  (* ----------- binary integer operations ----------------- *)
	  fun op2i comflag oper : loc_or_val * con * state =
	      let val [vl1,vl2] = vl_list 
		  (* commute values if the first arg is a small imm *)
		  val (vl1,vl2) = if comflag then commute(vl1,vl2) else (vl1,vl2)
		  val a' = load_ireg_locval(vl1,NONE)
		  val b' = load_ireg_sv vl2
		  val dest = alloc_regi NOTRACE_INT
		  val _ = add_instr(oper(a',b',dest))
	      in (VAR_LOC(VREGISTER(false, I dest)), int32, state)
	      end

	  val commutesop2i = op2i true
	  val stdop2i = op2i false
	  fun add_ibar b1 b2 = (add_instr (b1 INT_TT); add_instr (b2 INT_TT))
	  fun trapZero result = (add_ibar SOFT_ZBARRIER HARD_ZBARRIER; result)
	  fun trapOver result = (add_ibar SOFT_VBARRIER HARD_VBARRIER; result)
	  (* ----------- binary and unary float operations ----------------- *)
	  fun op2f oper : loc_or_val * con * state =
	      (case vl_list of
		   [vl1,vl2] => 
		       let val a' = load_freg_locval(vl1,NONE)
			   val b' = load_freg_locval(vl2,NONE)
			   val dest = alloc_regf()
			   val _ = add_instr(oper(a',b',dest))
		       in (VAR_LOC(VREGISTER(false, F dest)), float64, state)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")
	  fun op1f oper : loc_or_val * con * state =
	      (case vl_list of
		   [vl] => 
		       let val a' = load_freg_locval(vl,NONE)
			   val dest = alloc_regf()
			   val _ = add_instr(oper(a',dest))
		       in (VAR_LOC(VREGISTER(false, F dest)), float64, state)
		       end
		 | _ => error "need exactly 2 arguments for this primitive")
	  fun extract_dispatch(t,state,arg,(xfloat,xint,xknown,xdynamic)) = 
	      let fun dynamic c =
		     let val (con_ir,_,state) = xcon(state,fresh_var(),c,NONE)
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
	  val unit_vvc = (#1 unit_vvc, #2 unit_vvc, state)
      in (case prim of
	      soft_vtrap tt => (add_instr(SOFT_VBARRIER(xtt tt)); unit_vvc)
	    | soft_ztrap tt => (add_instr(SOFT_ZBARRIER(xtt tt)); unit_vvc)
	    | hard_vtrap tt => (add_instr(HARD_VBARRIER(xtt tt)); unit_vvc)
	    | hard_ztrap tt => (add_instr(HARD_ZBARRIER(xtt tt)); unit_vvc)
	       

	    | float2int => 
		  let val [vl] = vl_list 
		      val src = load_freg_locval(vl,NONE)
		      val dest = alloc_regi NOTRACE_INT
		      val _ = add_instr(CVT_REAL2INT(src,dest))
		  in (VAR_LOC(VREGISTER(false, I dest)), int32, state)
		  end

	    | int2float => 
		  let val [vl] = vl_list
		      val src = load_ireg_locval(vl,NONE)
		      val dest = alloc_regf()
		      val _ = add_instr(CVT_INT2REAL(src,dest))
		  in (VAR_LOC(VREGISTER(false, F dest)), float64,state)
		  end

            (* XXX do we want overflow in some cases or are these casts? *)
	    | int2uint _ => 
		  let val [vl] = vl_list 
		  in  (vl, int32, state)
		  end

	    | uint2uint _ => 
		  let val [vl] = vl_list 
		  in (vl, int32, state)
		  end

	    | uint2int _ => 
		  let val [vl] = vl_list 
		  in (vl, int32, state)
		  end

	    | int2int _ => 
		  let val [vl] = vl_list 
		  in (vl, int32, state)
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
		  in (vl, Prim_c(Array_c, [Prim_c(Int_c is2,[])]),state)
		  end
		       
	    | (uintv2uintv (is1,is2)) =>
		  let val [vl] = vl_list 
		  in  (vl, Prim_c(Array_c, [Prim_c(Int_c is2,[])]),state)
		  end

	    | (array2vector table) => 
		  (case (table,vl_list,clist) of 
		       (IntArray is,[vl],_) => (vl,Prim_c(Vector_c, [Prim_c(Int_c is,[])]),state)
		     | (FloatArray fs,[vl],_) => (vl,Prim_c(Vector_c, [Prim_c(Float_c fs,[])]),state)
		     | (OtherArray _,[vl],[c]) => (vl,Prim_c(Vector_c, [c]),state)
		     | _ => error "illegal array2vector")

	    | (vector2array table) => 
		  (case (table,vl_list,clist) of 
		       (IntVector is,[vl],_) => (vl,Prim_c(Array_c, [Prim_c(Int_c is,[])]),state)
		     | (FloatVector fs,[vl],_) => (vl,Prim_c(Array_c, [Prim_c(Float_c fs,[])]),state)
		     | (WordVector,[vl],[c]) => (vl,Prim_c(Array_c, [c]),state)
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
		       in  extract_dispatch(t,state,(vl1,vl2),
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
		       let val vl1 = VAR_VAL(VINT 0w0)
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
		      val ir1 = load_ireg_locval(vl1,NONE)
		      val ir2 = load_ireg_locval(vl2,NONE)
		      val desti = alloc_regi NOTRACE_INT
		      val _ = add_instr(CMPUI(EQ,ir1,REG ir2,desti))
		  in  (VAR_LOC(VREGISTER (false,I desti)),NilUtil.bool_con, state)
		  end
             | _ => (print "primitive: ";
                       Ppnil.pp_prim prim;
                       print "not implemented\n";
                       raise Util.UNIMP))
      end

 

   (* ------------------- translate constructors ------------------------ *)

  and xcon arg : regi * kind * state = 
      (case xcon' arg of
	   (_,VAR_LOC var_loc, k, s) => (load_ireg_loc(var_loc,NONE),k,s)
	 | (_,VAR_VAL var_val, k, s) => (load_ireg_val(var_val,NONE),k,s))

  and xcon' (state : state,
	    name : var, (* Purely for debugging and generation of useful names *)
	    arg_con : con,     (* The expression being translated *)
	    kopt : kind option (* Caller may know kind of con being translated *)
	    ) : bool * loc_or_val * kind * state = 
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
	  val res = xcon''(state,name,arg_con,kopt)
	  val _ = if (!debug)
		      then (print "xcon ";  print (Int.toString (!con_depth));
			    print " returned\n")
		  else ()
	  val _ = con_depth := !con_depth - 1
      in  res
      end

  and xcon'' (state : state,
	    name : var, (* Purely for debugging and generation of useful names *)
	    arg_con : con,     (* The expression being translated *)
	    kopt : kind option (* Caller may know kind of con being translated *)
	    ) : bool * loc_or_val * kind * state = 
      let 
	  fun mk_ptr  i = (true, VAR_VAL (VTAG (TW32.fromInt i)), Type_k, state)
	  fun mk_ptr' i = (true, VAR_VAL (VTAG (TW32.fromInt i)), Type_k, state)
	  fun mk_sum_help (state,kinderopt,indices,cons) = 
	      let val indices' = map (fn i => VAR_VAL(VINT (TW32.fromInt i))) indices
	          fun folder (c,(const,s)) =
		       let val (const',c,k,s) = xcon'(s,fresh_named_var "xcon_sum",c, NONE)
		       in ((c,k),(const andalso const', s))
		       end
		  val (con_kinds,(const,state)) = foldl_list folder (true,state) cons
		  val cons' = map #1 con_kinds
		  val reps = (map (fn _ => NOTRACE_INT) indices') @ (map (fn _ => TRACE) cons')
		  val kind = (case kinderopt of
				  NONE => Type_k
				| SOME kinder => kinder con_kinds)
		  val (lv,state) = if const 
				       then make_record_const(state,NONE,reps, indices' @ cons', NONE)
				   else make_record(state,NONE,reps, indices' @ cons')
	      in (const, lv, kind, state)
	      end
	  fun mk_sum' (s,indices,cons) = mk_sum_help(s,NONE,indices,cons)
	  fun mk_sum (s,index,cons) = mk_sum_help(s,NONE,[index],cons)
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
	     | Prim_c(Record_c _,cons) => mk_sum_help(state,NONE,[5,length cons],cons)
	     | Prim_c(Vararg_c _,cons) => mk_sum(state,6,cons)
	     | Prim_c _ => error "ill-formed primitive type"
	     | Mu_c (is_recur,vcset) => 
		   let val (const,lv,k,state) = mk_sum_help(state,NONE,[8],[])
		       val num_mu = Sequence.length vcset
		   in  if (num_mu = 1) then (const,lv,k,state)
		       else let val kind = kind_tuple(Listops.map0count (fn _ => k) num_mu)
				val reps = Listops.map0count (fn _ => TRACE) num_mu
				val vl = Listops.map0count (fn _ => lv) num_mu
				val (lv,state) = if const (* all out mus are degenerate now *)
						     then make_record_const(state,NONE,reps,vl,NONE)
						 else make_record_const(state,NONE,reps,vl,NONE)
			    in  (const,lv,kind,state)
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
				   let val clregs = map (fn lv => load_ireg_locval(lv,NONE)) lvs
				       fun folder (((v,_),clreg),s) = 
						add_convar s (v,SOME(VREGISTER (false,I clreg)),
							    NONE,Word_k Runtime, 
							    SOME(Word_k Runtime),NONE)
				       val recstate = foldl folder state (zip vclist clregs)
				       fun do_write ((clreg,(v,c)),s) = 
					   let val (r,_,s) = xcon(s,v,c, NONE)
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
	     | AllArrow_c (Open,_,_,_,_,_,_) => error "open Arrow_c"
	     | AllArrow_c (Closure,_,_,_,clist,numfloat,c) => 
		   mk_sum_help(state,NONE,[9],[])
(*		   mk_sum_help(NONE,[9,TW32.toInt numfloat],c::clist) *)
	     | AllArrow_c (Code,_,_,_,clist,numfloat,c) => 
(*		   mk_sum_help(state,NONE,[10,TW32.toInt numfloat],c::clist) *)
		   mk_sum_help(state,NONE,[10],[])
	     | ExternArrow_c _ =>
(*		   mk_sum_help(NONE,[11,TW32.toInt numfloat],c::clist) *)
		   mk_sum_help(state,NONE,[11],[])
	     | Var_c v => 
		   let val (vl,vv,k) = 
		       (case (getconvarrep state v) of
			    (_,SOME vv, k) => (true, VAR_VAL vv,k)
			  | (SOME vl,_, k) => 
				let val const = (case vl of
							VGLOBAL _ => true
						      | VREGISTER (const,_) => const)
				in  (const, VAR_LOC vl,k)
				end
			  | (NONE,NONE,_) => error "no info on convar")
		   in  (vl,vv,k,state)
		   end
	     | Let_c (letsort, cbnds, c) => 
		   let fun folder (cbnd,s) = xconbnd s (Runtime,cbnd)
		       val s' = foldl folder state cbnds
		   in  xcon'(s',fresh_var(),c, kopt)
		   end
	     | Crecord_c lclist => 
		   let val vars = map (fn (l,_) => fresh_named_var (label2string l)) lclist
		       fun folder ((v,(l,c)),(const,state)) = 
			   let val (const',vl,k,state) = xcon'(state,v,c,NONE)
			   in ((l,v,(vl,k)), (const andalso const', state))
			   end
		       val (lvregikind,(const,state)) = foldl_list folder (true,state) (zip vars lclist)
		       val lvkList = map (fn (l,v,(_,k)) => ((l,v),k)) lvregikind
		       val kind = Record_k(Sequence.fromList lvkList)
		       val lvs = map (fn (_,_,(lv,_)) => lv) lvregikind
		       val reps = map (fn _ => TRACE) lvs
		       val (lv,state) = (case (!do_single_crecord,lclist) of
					     (true,[_]) => (hd lvs, state)
					   | _ => if const 
						    then make_record_const(state,NONE,reps,lvs,NONE)
						  else make_record(state,NONE,reps,lvs))
		   in  (const,lv,kind,state)
		   end
	     | Proj_c (c, l) => 
		   let val (const,lv,k,state) = xcon'(state,fresh_named_var "proj_c",
								   c,NONE)
		       
		       val Record_k lvk_seq = k
		       fun default() = 
			   let fun loop [] _ = error "ill-formed projection"
				 | loop (((l',_),k)::vrest) n = if (Name.eq_label(l,l')) 
							    then (n,k) else loop vrest (n+1)
		       (* fieldk won't have have dependencies *)			
			       val (which,fieldk) = loop (Sequence.toList lvk_seq) 0
			       val dest = alloc_regi TRACE
			       val ir = load_ireg_locval(lv,NONE)
			       val _ = add_instr(ICOMMENT ("Proj_c at label " ^ 
							   (Name.label2string l)))
			       val _ = add_instr(LOAD32I(EA(ir,4 * which),dest))
			   in (const,VAR_LOC(VREGISTER (const,I dest)), fieldk, state)
			   end
		   in  (case (!do_single_crecord,Sequence.toList lvk_seq) of
			    (true,[(_,k)]) => (const,lv,k,state)
			  | _ => default())
		   end
	     | Closure_c (c1,c2) => 
		   let 
		       fun kinder [(_,Arrow_k(_,vklist : (var * kind) list, k)),_] = 
			   let val vklist' = butlast vklist
			   in  Arrow_k(Closure,vklist',k)
			   end
			 | kinder _ = error "bad Closure_c"
		   in  mk_sum_help(state,SOME kinder,[],[c1,c2])
		   end
	     | Typecase_c _ => error "typecase_c not implemented"
	     | App_c (c,clist) => (* pass in env argument first *)
		   let val _ = add_instr(ICOMMENT "start making constructor call")
		       val (const_fun,lv,k,state) = xcon'(state,fresh_named_var "closure",c,NONE)
		       val clregi = load_ireg_locval(lv,NONE)
		       val resk = (case (kopt,k) of
				       (SOME k, _) => k
				     | (_,Arrow_k(_,_,resk)) => resk
				     | _ => (print "bad kind to App_c\n"; Ppnil.pp_kind k;
					     error "bad kind to App_c"))
		       val (cregsi,(const_arg,state)) = 
			   foldl_list  (fn (c,(const,state)) => 
				   let val (const',vl,_,state) = xcon'(state,fresh_named_var "clos_arg",c,NONE)
				   in  (load_ireg_locval(vl,NONE),(const andalso const', state))
				   end) (true,state) clist
		       val const = const_fun andalso const_arg
		       val coderegi = alloc_regi NOTRACE_CODE
		       val envregi = alloc_regi TRACE
		       val _ = (add_instr(LOAD32I(EA(clregi,0),coderegi));
				add_instr(LOAD32I(EA(clregi,4),envregi)))
		       val desti = alloc_regi TRACE
		       val _ = add_instr(CALL{extern_call = false,
					      func=REG' coderegi,
					      return=NONE,
					      args=(cregsi @ [envregi],[]),
					      results=([desti],[]),
					      tailcall=false,
					      save=SAVE(getLocals())})
		       val state = new_gcstate state
		       val _ = add_instr(ICOMMENT "done making constructor call")
		   in (const,VAR_LOC (VREGISTER (const,I desti)),resk,state)
		   end
	     | Annotate_c (_,c) => xcon'(state,name,c,kopt))
      end

  

  local 
      fun doconfun is_top (state,vname,vklist,body,kind) = 
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
			    val s' = add_convar "6" (s,v,k,NONE,SOME(VREGISTER (false,I r)),NONE)
			in  (r,s')
                        end
	      val (cargs,state) = foldl_list folder state vklist
	      val args = (cargs,[])
	      val return = alloc_regi(LABEL)
	      val _ = set_args(args, return)
	      val state = needgc(state,IMM 0)
	      val (ir,k,state) = xcon(state,fresh_named_var "result",body,NONE)
	      val result = getResult(fn() => I ir)
	      val I resulti = result
	      val _ = (add_instr(MV(ir,resulti));
		       add_instr(RETURN return))
	      val p = get_proc()
	  in add_proc p
	  end
     fun dofun_help is_top (state,vname,Function(effect,recur,vklist,_,vclist,vflist,body,con)) = 
	  let 
	      val name = (case (Name.VarMap.find(!exports,vname)) of
			      NONE => LOCAL_CODE (Name.var2string vname)
			    | SOME [] => error "export has no labels"
			    | SOME (l::_) => l)
	      val _ = reset_state(is_top, (vname, name))
	      val _ = if (!debug)
			  then (print "-----dofun_help : "; 
				print (Pprtl.label2s name); print "\n")
		      else ()
              fun folder ((v,k),s) = 
		  let val r = alloc_named_regi v TRACE
		      val s' = add_convar "7" (s,v,k,NONE,SOME(VREGISTER (false,I r)),NONE)
		  in  (r,s')
		  end
	      val (cargs,state) = foldl_list folder state vklist
              fun folder ((v,c),s) = let val r as (I ir) = alloc_named_reg s (c,v)
					 val s' = add_reg (s,v,c,r)
			             in  (ir, s')
                                     end
	      val (eiargs,state) = foldl_list folder state vclist
              fun folder (v,s) = let val fr = alloc_named_regf v
					       val s' = add_reg (s,v,Prim_c(Float_c Prim.F64,[]), F fr)
					   in  (fr,s')
                                           end
              val (efargs,state) = foldl_list folder state vflist

	      val args = (cargs @ eiargs, efargs)
	      val return = alloc_regi(LABEL)
	      val _ = set_args(args, return)
	      val state = needgc(state,IMM 0)
	      val (r,c,state) = xexp'(state,fresh_named_var "result",body,
				      NONE, ID return)
	      val result = getResult(fn() => r)
	      val results = (case result of
				 I ir => ([ir],[])
			       | F fr => ([],[fr]))
	      val mvinstr = (case (r,result) of
				 (I ir1,I ir2) => MV(ir1,ir2)
			       | (F fr1,F fr2) => FMV(fr1,fr2)
			       | _ => error "register mismatch")
	      val _ = (add_instr mvinstr;
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
		       val _ = if (!diag)
				   then (print "*** Working on "; print temp; print "\n")
			       else ()
		       val _ = dofun vf
		       val _ = if (!diag)
				   then (print "*** Finished "; print temp; print "\n")
			       else ()
		   in  worklist_loop()
		   end
	     | SOME (n,ConFunWork vvkck) => 
		   let val _ = curfun := n
		       val temp = "confunction " ^ (Int.toString n) ^ ": " ^ (Name.var2name (#2 vvkck))
		       val _ = if (!diag) 
				   then (print "*** Working on "; print temp; print "\n")
			       else ()
		       val _ = doconfun false vvkck
		       val _ = if (!diag)
				   then (print "*** Finished "; print temp; print "\n")
			       else ()
		   in  worklist_loop()
		   end)
  end


  (* unitname is the name of the unit; unit names are globally unique. *)

   fun translate (unitname:string) trans_params (Nil.MODULE{bnds : bnd list,
					  imports : import_entry list,
					  exports : export_entry list}) =
         let 

	     val _ = if (!debug)
			 then print "tortl - entered translate\n"
		     else ()

	     val toplevel_locals = VarSet.empty

	     val _ = if (!debug)
			 then print "tortl - handling exports now\n"
		     else ()


	     local
		 fun mapper (ExportValue(l,v)) =
		     let val lab = ML_EXTERN_LABEL(Name.label2string l)
		     in  (v,lab)
		     end
		   |  mapper (ExportType(l,v)) =
		     let val lab = ML_EXTERN_LABEL(Name.label2string l)
		     in  (v,lab)
		     end
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

		 
	     val _ = if (!debug)
			 then print "tortl - handling imports now\n"
		     else ()

	     val _ = reset_state(true, (mainCodeVar, mainCodeName))
	     fun folder (ImportValue(l,v,c),s) = 
		 (* For extern or C functions, the label IS the value rather than a pointer *)
		 let val mllab = ML_EXTERN_LABEL(Name.label2string l)
		     val lv = 			 
			 (case c of
			      ExternArrow_c _ => VAR_VAL(VCODE mllab)
			    | _ => VAR_LOC(VGLOBAL(mllab,con2rep s c)))
		 in  add_global(s,v,c,lv)
		 end
	       | folder (ImportType(l,v,k),s) = 
		 let val vl = (VGLOBAL(ML_EXTERN_LABEL(Name.label2string l),TRACE))
		 in  add_conglobal "1" (s,v,k,NONE, VAR_LOC vl)
		 end
	     val state = needgc(make_state(),IMM 0)
	     val state = foldl folder state imports
	     val return = alloc_regi(LABEL)
	     val args = ([],[])
	     val _ = set_args(args, return)
	     val (r,c,state) = xexp'(state,fresh_named_var "result",exp,
				      NONE, ID return)
	      val result = getResult(fn() => r)
	      val mvinstr = (case (r,result) of
				 (I ir1,I ir2) => MV(ir1,ir2)
			       | (F fr1,F fr2) => FMV(fr1,fr2)
			       | _ => error "register mismatch")
	      val _ = (add_instr mvinstr;
		       add_instr(RETURN return))
	      val p = get_proc()
	     val _ = add_proc p

	     val _ = if (!debug)
			 then print "tortl - calling worklist now\n"
		     else ()

	     val _ = worklist_loop()

	     val _ = print "tortl - returned from worklist now\n"
	     val _ = if (!debug)
			 then print "tortl - returned from worklist now\n"
		     else ()

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


end;
