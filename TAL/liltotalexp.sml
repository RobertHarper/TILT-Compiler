structure LilToTalExp :> LILTOTALEXP = 
  struct
    structure TE = LilToTalEnv
    structure TS = LilToTalState
    structure LTC = LilToTalCon
    structure LS = LilSubst
    structure TTD = TalTranslationDefs
    structure Eq = LilTypeEquiv
    structure LD = LilDefs
    structure LU = LilUtil
    structure Dec = Deconstruct.Dec
    structure Elim = Deconstruct.Elim
    structure TA = TalAbbrev
    structure LLU = LilLinkUnit
    structure TI = TalInstructions

    structure LO = Listops

    open Lil

    val error = fn s => Util.error "liltotalexp.sml" s
    val debug = Stats.ff "LilToTalExpDebug"
    val do_tailcall = Stats.tt "LilToTalTailcall"

    val debuglev = ref 0
    val chatlev = ref 0 

    fun indent i s = 
      let
	fun loop 0 cs = String.implode cs
	  | loop i cs = loop (i - 1) (#" "::cs)
      in loop (i+1) (String.explode s)
      end

    fun chatp i = !(chatlev) >= i
    fun chat i s = if chatp i then (print (indent i s);print "\n") else ()
    fun debugdo (i,t) = if (!debug) andalso (i <= !debuglev) then (t(); ()) else ()

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k
    fun eout (e : Lil.exp) : Lil.exp_ = #e e


    fun debug_check_state s state = 
      debugdo (7,fn () => (print ("Checking state: "^s^"\n");
			   TS.print_state state;
			   print "\n";
			   TS.sanity_check state))

    fun debug_print_state s state = 
      debugdo (7,fn () => (print ("State: "^s^"\n");
			   TS.print_state state;
			   print "\n"))
				 
    fun fresh_tal_lbl s = (TTD.E.mk_tal_lbl (Name.fresh_internal_label s))
    fun derived_tal_lbl l = (TTD.E.mk_tal_lbl (Name.fresh_internal_label (Name.label2name l)))

    (* Translate a potential extern label.  Externs should not be mangled *)
    fun unk_lbl_trans l c = 
      let
	val (_,c) = Dec.C.nary_forall c
      in
	case Dec.C.externarrow' c
	  of SOME _ => l
	   | NONE => TTD.E.mk_tal_lbl l
      end
	    
    fun find_varcon32 (env,x) = 
      (case TE.find_talvar (env,x)
	 of SOME c => TS.TalCon c
	  | NONE => TS.LilCon (TE.find_var32 (env,x))) handle any => (print "Problem with var ";
								      PpLil.pp_var x; print"\n";
								      raise any)

    fun find_varcon64 (env,x) = 
      (case TE.find_talvar (env,x)
	 of SOME c => TS.TalCon c
	  | NONE => TS.LilCon (TE.find_var64 (env,x)))  handle any => (print "Problem with var ";
								       PpLil.pp_var x; print"\n";
								       raise any)

    datatype genaddr = L of (Tal.label * (Tal.con list)) | F of (Tal.label * (Tal.con list)) | S (* label, fallthru, or step *)

    datatype dest = 
      RET of Tal.int32                     (* Function return context: return and pop i bytes below return addr *)
      | CC of genaddr * genaddr   
                              (* Condition code context: (lnonzero, lzero) *)
                              (* Invariant: at most one fallthru *)
      | EFF of genaddr        (* Effect context: value unused. *)
      | CONT of genaddr * Lil.var
                              (* Place value in genop and continue to label.
			       * NOTE: genop is reserved .  *)


    local

      fun mk_tapp a = Tal.Tapp (Tal.Con (Tal.cvar a))

      val cs_app1 = mk_tapp TTD.T.cs_tvar1
      val cs_app2 = mk_tapp TTD.T.cs_tvar2
      val cs_app3 = mk_tapp TTD.T.cs_tvar3
      val cs_app4 = mk_tapp TTD.T.cs_tvar4
      val cs_app5 = mk_tapp TTD.T.cs_tvar5
      val cs_app6 = mk_tapp TTD.T.cs_tvar6
      val s_app1 = mk_tapp TTD.T.s_tvar1
      val s_app2 = mk_tapp TTD.T.s_tvar2

      val cs_var1 = Name.fresh_named_var "cs_1"
      val cs_var2 = Name.fresh_named_var "cs_2"
      val cs_var3 = Name.fresh_named_var "cs_3"

      val ret_addr = Name.fresh_named_var "retaddr"
      val exn_ptr = Name.fresh_named_var "exnptr"
      val rvar = Name.fresh_named_var "retvar"
		   
    in 

      fun get_rvar () = rvar

      fun get_rtype env = 
	(case TE.get_ms_return env
	   of SOME rtype => rtype
	    | _ => error "No return type")

      fun get_rcont_type env = 
	(case TE.get_ms_return env
	   of SOME rtype => TTD.T.rtype (LTC.ctrans env rtype)
	    | _ => error "No return type")

      fun bind_tal_vars env = 
	let
	  val env = TE.bind_talvar (env,(cs_var1,fn _ => TTD.T.cs_con1))
	  val env = TE.bind_talvar (env,(cs_var2,fn _ => TTD.T.cs_con2))
	  val env = TE.bind_talvar (env,(cs_var3,fn _ => TTD.T.cs_con3))
	  val env = TE.bind_talvar (env,(rvar,fn env => LTC.ctrans env (get_rtype env)))
	  val env = TE.bind_talvar (env,(ret_addr,get_rcont_type))
	in env
	end

      (* instantiate a function with the input frame args.
       *)

(*
      fun init_from_reg state v r = 
	(case TS.define_var state v
	   of (state,SOME gop) => TS.init_temp state gop (Tal.Reg r,[])
	    | (state,NONE) => state)
*)
      (* Call at entry *)
      fun spill_callee_save state = 
	let
	  val state = TS.define_var_from_reg state cs_var1 Tal.Ebx
	  val state = TS.define_var_from_reg state cs_var2 Tal.Esi
	  val state = TS.define_var_from_reg state cs_var3 Tal.Edi
	  val state = TS.release_reg state Tal.Ebx
	  val state = TS.release_reg state Tal.Esi
	  val state = TS.release_reg state Tal.Edi

(*	  val state = TS.define_var_from_reg state exn_ptr Tal.Ebp*)
	  val state = TS.comment state 2 "spilling callee_saves"
	in state
	end
	
      (* Call at exit *)
      fun restore_callee_save state = 
	let
	  (* Associate the callee save variables with the callee saves
	   * registers, spilling if necessary.
	   *)
	  val state = TS.load_var state (Tal.Ebx) cs_var1 (TS.TalCon TTD.T.cs_con1)
	  val state = TS.load_var state (Tal.Esi) cs_var2 (TS.TalCon TTD.T.cs_con2)
	  val state = TS.load_var state (Tal.Edi) cs_var3 (TS.TalCon TTD.T.cs_con3)
(*	  val state = TS.load_var state (Tal.Ebp) exn_ptr (TTD.T.handler_stackptr (TTD.T.s_con2))*)
	  val state = TS.comment state 2 "spilling/restoring callee saves"
	in state
	end


      (* Emit code to load the caller save registers.
       *)
      fun spill_caller_save state = 
	let
	  val state = TS.free_reg state Tal.Eax
	  val state = TS.free_reg state Tal.Ecx
	  val state = TS.free_reg state Tal.Edx
	  val state = TS.comment state 2 "restoring caller saves"
	in state
	end

      fun kill_ret_addr state = TS.kill_var32 state ret_addr


      fun place_ret_addr' state env i = 
	let
	  val rtype = get_rcont_type env
	  val state = TS.kill_var32 state ret_addr
	  val (state,d) = TS.reserve_inarg32 state i
	  val state = TS.init_var32 state d ret_addr (TS.TalCon rtype)
	in state
	end

      fun bind_ret_addr state env i = 
	let
	  val rtype = get_rcont_type env
	in TS.bind_formal32 state ret_addr (TS.TalCon rtype) i
	end
    end

    fun emit_retn state argsz = 
      let
	val szopt = if argsz = 0w0 then NONE else SOME argsz
      in
	TS.emit state (Tal.Retn szopt)
      end

    fun tuple_field reg offset = Tal.Prjr ((reg,[]),offset * 0w4,NONE)
    fun tuple_field64 reg num32 offset = Tal.Prjr ((reg,[]),num32 * 0w4 + offset * 0w8,NONE)

    fun dec_stack state w = 
      if w = 0w0 then state
      else 
	TS.emit state (Tal.ArithBin(Tal.Sub,Tal.Reg Tal.Esp,Tal.Immed w))

    fun popn state i = 
      if i = 0w0 then state
      else TS.emit state (Tal.ArithBin(Tal.Add,Tal.Reg Tal.Esp,Tal.Immed i))
    fun mov state a b = TS.emit state (Tal.Mov(a,b))
    fun mov_r_m state r m = mov state (Tal.Reg r) (m,[])
    fun mov_m_r state m r = mov state m (Tal.Reg r,[])
    fun mov_r_r state r1 r2 = mov state (Tal.Reg r1) (Tal.Reg r2,[])

    fun pop state g = TS.emit state (Tal.Pop g)
    fun pop_r state r = pop state (Tal.Reg r)

    fun push state g = TS.emit state (Tal.Push g)
    fun push_m state m = push state (m,[])
    fun push_r state r = push state (Tal.Reg r,[])

    (* Place return address in ith 32 slot, and set callee saves *)
    fun place_exit_vars state env i = 
      let
	val state = place_ret_addr' state env i
	val state = restore_callee_save state
      in state
      end

    fun pop_frame state i = 
      let
	val state = popn state (TS.get_frame_size state - i)
(*	val state = restore_callee_save state*)
	val state = TS.comment state 2 "Popping frame"
      in state
      end

    fun jmp_label' state env (l,qs) =
      let
	val state = TS.emit state (Tal.Jmp (Tal.Addr l,qs))
	val state = TS.comment_stack state 4
      in state
      end

    fun jcc_label' state env cond lqs = 
      let
	val state = TS.emit state (Tal.Jcc (cond,lqs,NONE))
	val state = TS.comment_stack state 4
      in state
      end

    fun fallthru' state cs = 
      let
	val state = TS.emit state (Tal.Fallthru (List.rev cs))
	val state = TS.comment_stack state 4
      in state
      end


    fun internal_label_cons state env = 
      let
	val tvars = TE.get_tvars env
	val cons = map (Tal.cvar o (TE.vartrans env)) tvars
      in cons @[TTD.T.cs_con1,TTD.T.cs_con2,TTD.T.cs_con3,TTD.T.s_con1,TTD.T.s_con2]
      end

    fun cons2tapps cs = map (fn c => Tal.Tapp (Tal.Con c)) cs

    fun mk_s_con1 env = if TE.in_handler env then Tal.cempty else TTD.T.s_con1
    fun mk_s_con2 env = if TE.in_handler env then Tal.cappend TTD.T.s_con1 TTD.T.s_con2 else TTD.T.s_con2

    fun instantiate_tvars state env l = (Tal.Addr l,cons2tapps (internal_label_cons state env))


    fun abstract_tvars env c = 
      let
	val tvars = TE.get_tvars env 
	val tks = map (fn a => (TE.vartrans env a,LTC.ktrans (TE.find_cvar (env,a)))) tvars
	val tks = tks @ [(TTD.T.cs_tvar1,Tal.k4byte),
			 (TTD.T.cs_tvar2,Tal.k4byte),
			 (TTD.T.cs_tvar3,Tal.k4byte),
			 (TTD.T.s_tvar1,Tal.kstack),
			 (TTD.T.s_tvar2,Tal.kstack)]
      in List.foldl (fn (ak,c) => TTD.T.forall ak c) c tks
      end

    fun internal_label_type state env = 
      let
	val ms = TS.typeof_state state env
      in abstract_tvars env ms
      end

    fun jmp_coercions state env = rev (cons2tapps (internal_label_cons state env))

    fun jmp_label state env (l,cs) = jmp_label' state env (l,cons2tapps cs)
    fun jcc_label state env cond (l,cs) = jcc_label' state env cond (l,cons2tapps cs)

    fun fallthru state env (l,cs) = 
      let
	val state = TS.comment state 2 ("Fallthru to label "^(Name.label2string l))
	val state = fallthru' state cs
      in state
      end

    fun store_float_and_pop state gop = TS.emit state (TI.store_float_and_pop gop)
    fun load_float state gop = TS.emit state (TI.load_float gop)
    fun fpstack_pop state = TS.emitn state (TI.fpstack_pop ())


    fun fp1arg what state = 
      let
 	val state = TS.emit state (Tal.FPnoargs what)
      in state
      end

    fun fp2arg what state gop = 
      let
 	val state = TS.emit state (Tal.FPsomeargs (what,Tal.FPgenop(Lil.B8,gop)))
      in state
      end

    fun jmp_genaddr state env gd = 
      (case gd
	 of F lcs  => fallthru state env lcs
	  | L lcs => jmp_label state env lcs
	  | S   => state)

    fun b2i w = (LU.w2i w) div 4

    fun exn_raise_coercions state env = 
      let
	val outargcount = TS.get_outarg_count state
	val framecount = TS.get_frame_elt_count state

	val over_handler = mk_s_con1 env
	val handler_stack = mk_s_con2 env

	val s1 = Tal.StackSlice (Tal.Esp,outargcount,outargcount + framecount,over_handler)
	val s2 = Tal.Con (handler_stack)

	val annotes = 
	  List.rev
	  [ s1
	   ,s2]
      in map Tal.Tapp annotes
      end

    fun call_coercions state env = 
      let
	val outargcount = TS.get_outarg_count state
	val framecount = TS.get_frame_elt_count state

	val over_handler = mk_s_con1 env
	val handler_stack = mk_s_con2 env

	val s1 = Tal.StackSlice (Tal.Esp,outargcount,outargcount + framecount,over_handler)
	val s2 = Tal.Con (handler_stack)

	val ebxt = (TS.typeof_reg' state env Tal.Ebx)
	val esit = (TS.typeof_reg' state env Tal.Esi)
	val edit = (TS.typeof_reg' state env Tal.Edi)

	val annotes = 
	  List.rev
	  [Tal.Con(ebxt)
	   ,Tal.Con(esit)
	   ,Tal.Con(edit)
	   ,s1
	   ,s2]
      in map Tal.Tapp annotes
      end

    (* No tailcalls within handlers*)
    fun tailcall_coercions state env = 
      let
	val s1 = Tal.Con (TTD.T.s_con1)
	val s2 = Tal.Con (TTD.T.s_con2)
	val ebxt = (TS.typeof_reg' state env Tal.Ebx)
	val esit = (TS.typeof_reg' state env Tal.Esi)
	val edit = (TS.typeof_reg' state env Tal.Edi)

	val annotes = 
	  List.rev
	  [Tal.Con(ebxt)
	   ,Tal.Con(esit)
	   ,Tal.Con(edit)
	   ,s1
	   ,s2]
      in map Tal.Tapp annotes
      end

    fun extern_call_coercions state env = 
      let
	val outargcount = TS.get_outarg_count state
	val s = Tal.StackTail(Tal.Esp,outargcount)
	val ebxt = (TS.typeof_reg' state env Tal.Ebx)
	val esit = (TS.typeof_reg' state env Tal.Esi)
	val edit = (TS.typeof_reg' state env Tal.Edi)
	val ebpt = TTD.T.handler_frame (mk_s_con2 env)

	val annotes = 
	  List.rev
	  [Tal.Con(ebxt)
	   ,Tal.Con(esit)
	   ,Tal.Con(edit)
	   ,Tal.Con(ebpt)
	   ,s]
      in map Tal.Tapp annotes
      end


    (* No handlers yet*)
    fun coercion_call_coercions state env = 
      let
	val s = Tal.StackTail(Tal.Esp,0)
	val ebxt = (TS.typeof_reg' state env Tal.Ebx)
	val ecxt = (TS.typeof_reg' state env Tal.Ecx)
	val edxt = (TS.typeof_reg' state env Tal.Edx)
	val esit = (TS.typeof_reg' state env Tal.Esi)
	val ebpt = TTD.T.handler_frame (mk_s_con2 env)
	val edit = (TS.typeof_reg' state env Tal.Edi)

	val annotes = 
	  List.rev
	  [Tal.Con(ebxt)
	   ,Tal.Con(ecxt)
	   ,Tal.Con(edxt)
	   ,Tal.Con(esit)
	   ,Tal.Con(ebpt)
	   ,Tal.Con(edit)
	   ,s]
      in map Tal.Tapp annotes
      end

    fun call_instantiate state env (gop,qs) = (gop,(call_coercions state env)@qs)
    fun tailcall_instantiate state env (gop,qs) = (gop,(tailcall_coercions state env)@qs)

    fun coercion_call_instantiate state env (gop,qs) = (gop,(coercion_call_coercions state env)@qs)

    fun extern_call_instantiate state env (gop,qs) = (gop,(extern_call_coercions state env)@qs)

    fun exn_raise_instantiate state env (gop,qs) = (gop,(exn_raise_coercions state env)@qs)

    fun oper_returns_unit env oper = 
      let
	val t = TE.typeof_op32 env oper
      in Eq.C.equal t (LD.T.unit())
      end

    (* Find cases where the last op is the value returned.
     *)
    fun terminal_op state env (bnd,e) =
      (case bnd
	 of Exp32_b(x,oper) =>
	   (case eout e
	      of Val32_e (Var_32 x') => if Name.eq_var (x,x') then SOME (x,oper)
				       else NONE
	       | Val32_e Unit => if oper_returns_unit env oper then SOME (x,oper)
				 else NONE
	       | _ => NONE)
	  | _ => NONE)

    fun coercion env (ctag,cons) = 
      let
	fun docon c = LTC.ctrans env c
	val res = 
	  (case (ctag,cons)
	     of (Roll, [to])   => [Tal.Roll (docon to)]
	      | (Unroll, [from]) => [Tal.Unroll]
	      | (Pack, [to,hiding])  => [Tal.Pack (docon hiding,docon to)]
	      | (ForgetKnown,[from]) => [Tal.Tosum (docon (LD.COps.ksum2sum from)),Tal.Fromsum]
	      | (ProjKnown,[from]) => [Tal.Fromsum]
	      | (InjUnion, [to])   => [Tal.Tosum (docon to)]
	      | (InjForget, [ksum]) => [Tal.Tosum (docon(LD.COps.ksum2sum ksum))]
	      | _ => error "Wrong coercion args")
      in res
      end


    (* Translate a first class coercion to a code block
     * which coerces EAX appropriately and then returns. *)
    fun coercion_fun outerstate env (vks,q) =
      let
	val l = Name.fresh_internal_label "coercion_fn"
	val tvars = TE.get_tvars env 
	val tks = map (fn a => (a,TE.find_cvar (env,a))) tvars
	val c = TE.typeof_sv32 env (Coercion q)
	val c = LD.T.nary_forall (tks@vks) c

	val env = TE.empty()
	val c = LTC.ctrans env c

	val state = TS.new_state ()
	val env = TE.bind_cvars (env,tks@vks)
	val env = TE.add_tvars env (tvars @ map #1 vks)
	val qs = coercion env q
	val state = emit_retn state 0w0
	val state = 
	  case qs 
	    of [] => state
	     | _ => TS.emit state (Tal.Coerce (Tal.Reg Tal.Eax,qs))


	val state = TS.emit_block state l (SOME c)
	val state = TS.add_blocks_from_state outerstate state
      in (state,(Tal.Addr l,map (fn a => Tal.Tapp (Tal.Con (Tal.cvar (TE.vartrans env a)))) (rev tvars)))
      end

    
    fun allocate_for state env (flags as (memok,immok,coerceok)) sv32 : TS.state * Tal.genop Tal.coerce = 
      let
	val () = chat 7 "Started allocate_for"
	val () = debugdo (5,fn () => (print "Allocating sv32 :";PpLil.pp_sv32 sv32;print "\n"))
	fun temp state = 
	  let
	    val (state,gop) = TS.reserve_temp memok state
	  in (state,(gop,[]))
	  end

	fun temp_reg state = 
	  let
	    val (state,gop) = TS.reserve_temp false state
	  in (state,(gop,[]))
	  end
	
	val res = 
	  (case sv32
	     of Var_32 v => 
	       let
		 val c = find_varcon32 (env,v)
		 val (state,gop) =  TS.reserve_for_var memok state v c
	       in (state,(gop,[]))
	       end
	      | Label l  => 
	       let
		 val (state,gop) = 
		   if immok then 
		     (state,Tal.Addr (unk_lbl_trans l (TE.typeof_sv32 env sv32)))
		   else 
		     TS.reserve_temp memok state 
	       in (state,(gop,[]))
	       end
	      | Const_32 v => 
	       let
		 val (state,goc) = 
		   if immok andalso coerceok then 
		     (state,(Tal.Immed (LU.value2w32 v),[Tal.Subsume (Tal.pcbytes Tal.B4)]))
		   else temp state
	       in (state,goc)
	       end
	      | Tag w => 
	       let
		 val (state,gop) = 
		   if immok then 
		     (state,Tal.Immed w)
		   else 
		     TS.reserve_temp memok state
	       in (state,(gop,[]))
	       end
	      | Unit => 
	       let
		 val (state,gop) = 
		   if immok then 
		     (state,Tal.Addr TTD.E.l_unit)
		   else 
		     TS.reserve_temp memok state
	       in (state,(gop,[]))
	       end
	      | Coercion q => 
	       let
		 val (state,goc) = 
		   if immok andalso coerceok then 
		     let
		       val (state,goc) = coercion_fun state env ([],q)
		     in (state,goc) 
		     end
		   else temp state
	       in (state,goc)
	       end
	      | (sv as Tabs _) => 
	       let
		 val (vks,sv) = Dec.E.nary_tabs sv
		 val q = 
		   (case Dec.Q.coercion' sv
		      of SOME q => q
		       | NONE => error "Tabs only used for coercions, I thought")

		 val (state,goc) = 
		   if immok andalso coerceok then 
		     let
		       val (state,goc) = coercion_fun state env (vks,q)
		     in (state,goc) 
		     end
		   else temp state
	       in (state,goc)
	       end
	      | TApp (sv,c) => 
	       if coerceok then 
		 let
		   val (state,(oper,qs)) = allocate_for state env flags sv
		   val c = LTC.ctrans env c
		 in (state,(oper,(Tal.Tapp (Tal.Con c))::qs))
		 end
	       else temp_reg state

	      | Coerce (sv1,sv2) => 
	       let
		 val res = 
		   (case sv1
		      of Coercion q => 
			if coerceok then 
			  let
			    val (state, (r,qs)) = allocate_for state env flags sv2
			    val qs' = coercion env q
			    val qs = qs' @ qs
			    val qs = 
			      (case qs
				 of (Tal.Roll c)::(Tal.Tosum _)::qs => (Tal.RollTosum c)::qs
				  | _ => qs)
			  in (state,(r,qs))
			  end
			else temp_reg state
		       | _ =>  temp state)
	       in res
	       end)
	val () = chat 7 "Finished allocate_for"
      in res
      end
    (* Reg required *)
    fun allocate_reg_for state env sv32 = 
      (case allocate_for state env (false,false,false) sv32
	 of (state,(Tal.Reg r,[])) => (state,r)
	  | _ => error "Not a register")

    (* Reg required, but coercion allowed *)
    fun allocate_reg_for' state env sv32 = allocate_for state env (false,false,true) sv32

    fun allocate_regimm_for state env sv32 = 
      (case allocate_for state env (false,true,false) sv32
	 of (state,(gop,[])) => (state,gop)
	  | _ => error "Got coercion for regimm")

    fun allocate_regimm_for' state env sv32 = allocate_for state env (false,true,true) sv32

    fun allocate_rm_for state env sv32 =
      (case allocate_for state env (true,false,false) sv32
	 of (state,(gop,[])) => (state,gop)
	  | _ => error "Got coercion for rm")

    fun allocate_rm_for' state env sv32 = allocate_for state env (true,false,true) sv32

    (* Any genop *)
    fun allocate_gop_for state env sv32 = 
      (case allocate_for state env (true,true,false) sv32
	 of (state,(gop,[])) => (state,gop)
	  | _ => error "Got coercion")

    (* Any genop coerce *)
    fun allocate_goc_for state env sv32 = allocate_for state env (true,true,true) sv32
      
    fun is_mem gop = 
      (case gop
	 of Tal.Reg _ => false
	   | Tal.Immed _ => false
	   | Tal.Addr _ => false
	   | _ => true)

    val sv32_trans_trace = Trace.newtrace "sv32_trans"
    fun sv32_trans state env (flags as (memok,immok,coerceok)) (loc : Tal.genop) (sv32 : Lil.sv32) : TS.state = 
      let
	val _ = debugdo(7,fn () => (Trace.enter sv32_trans_trace;
				    print "Sv32 is: ";PpLil.pp_sv32 sv32;print "\n"))
	val res = 
	  (case sv32
	     of Var_32 v => TS.init_var32 state loc v (find_varcon32 (env,v))
	      | Label l  => 
	       if immok then state
	       else TS.init_temp state loc (Tal.Addr (unk_lbl_trans l (TE.typeof_sv32 env sv32)),[])
	      | Const_32 v => 
	       if immok andalso coerceok then state
	       else TS.init_temp state loc (Tal.Immed (LU.value2w32 v),[Tal.Subsume (Tal.pcbytes Tal.B4)])
	      | Tag w => 
	       if immok then state
	       else TS.init_temp state loc (Tal.Immed w,[]) 
	      | Unit => 
	       if immok then state
	       else TS.init_temp state loc (Tal.Addr TTD.E.l_unit,[])
	      | Coercion q => 
	       if immok andalso coerceok then state 
	       else
		 let
		   val (state,goc) = coercion_fun state env ([],q)
		 in TS.init_temp state loc goc 
		 end
	      | (sv as Tabs _) =>
	       let
		 val (vks,sv) = Dec.E.nary_tabs sv
		 val q = 
		   (case Dec.Q.coercion' sv
		      of SOME q => q
		       | NONE => error "Tabs only used for coercions, I thought")
		      
	       in
		 if immok andalso coerceok then state
		 else
		   let
		     val (state,goc) = coercion_fun state env (vks,q)
		   in TS.init_temp state loc goc 
		   end
	       end
	      | TApp (sv,c) => 
	       let
		 val state = 
		   if coerceok then 
		     sv32_trans state env flags loc sv
		   else
		     let
		       val memok = not (is_mem loc)
		       val (state,goc) = allocate_for state env (memok,true,true) sv32
		       val state = mov state loc goc
		       val state = sv32_trans state env (memok,true,true) (#1 goc) sv
		       val state = TS.release_temp state loc
		     in state 
		     end
	       in state
	       end
	      | Coerce (sv1,sv2) => 
	       let
		 val res = 
		   (case sv1
		      of Coercion q => 
			let
			  val state = 
			    if coerceok then sv32_trans state env flags loc sv2
			    else
			      let
				val memok = not (is_mem loc)
				val (state,goc) = allocate_for state env (memok,true,true) sv32
				val state = mov state loc goc
				val state = sv32_trans state env (memok,true,true) (#1 goc) sv2
				val state = TS.release_temp state loc
			      in state 
			      end
			in state
			end
		       | _ =>  (* Abstract coercion call.  Place arg in EAX and call coercion *)
			let
			  val state = TS.shift_temp state loc (Tal.Reg Tal.Eax)
			  val (state,goc) = allocate_regimm_for' state env sv1
			  val goc = coercion_call_instantiate state env goc
			  val state = TS.emit state (Tal.Call goc)
			  val state = sv32_trans state env (false,true,true) (#1 goc) sv1
			  val state = sv32_trans state env (false,false,false) (Tal.Reg Tal.Eax) sv2
			in state
			end)
	       in res
	       end)
	val _ = debugdo(7,fn () => Trace.exit sv32_trans_trace)
      in res
      end

    (* Reg required *)
    fun sv32_trans_reg state env r sv32 = sv32_trans state env (false,false,false) (Tal.Reg r) sv32

    (* Reg required, but coercion allowed *)
    fun sv32_trans_reg' state env (gop,_) sv32 = sv32_trans state env (false,false,true) gop sv32

    fun sv32_trans_regimm state env gop sv32 = sv32_trans state env (false,true,false) gop sv32
    fun sv32_trans_regimm' state env (gop,_) sv32 = sv32_trans state env (false,true,true) gop sv32

    fun sv32_trans_rm state env gop sv32 = sv32_trans state env (true,false,false) gop sv32
    fun sv32_trans_rm' state env (gop,_) sv32 = sv32_trans state env (true,false,true) gop sv32

    (* Any genop *)
    fun sv32_trans_gop state env gop sv32 = sv32_trans state env (true,false,false) gop sv32

    (* Any genop coerce *)
    fun sv32_trans_goc state env (gop,_) sv32 = sv32_trans state env (true,true,true) gop sv32
      

    fun sv32_place_in_temp state env (loc : Tal.genop) (sv32 : Lil.sv32) : TS.state = 
      let
	val _ = debugdo(7,fn () => (print "Starting sv32_place_in_temp\n";
				    print "Sv32 is: ";PpLil.pp_sv32 sv32;print "\n";
				    print "Loc is: ";Pptal.pp_genop loc;print "\n"
				    ))
	val res = 
	  (case sv32
	     of Var_32 v => TS.init_var32 state loc v (find_varcon32 (env,v))
	      | Label l  => TS.init_temp state loc (Tal.Addr (unk_lbl_trans l (TE.typeof_sv32 env sv32)),[])
	      | Const_32 v => TS.init_temp state loc (Tal.Immed (LU.value2w32 v),[Tal.Subsume (Tal.pcbytes Tal.B4)])
	      | Tag w => TS.init_temp state loc (Tal.Immed w,[]) 
	      | Unit => TS.init_temp state loc (Tal.Addr TTD.E.l_unit,[])
	      | Coercion q => 
	       let
		 val (state,goc) = coercion_fun state env ([],q)
	       in TS.init_temp state loc goc 
	       end
	      | (sv as Tabs _) =>
	       let
		 val (vks,sv) = Dec.E.nary_tabs sv
		 val q = 
		   (case Dec.Q.coercion' sv
		      of SOME q => q
		       | NONE => error "Tabs only used for coercions, I thought")
		      
		 val (state,goc) = coercion_fun state env (vks,q)
	       in TS.init_temp state loc goc 
	       end
	      | TApp (sv,c) => 
	       let
		 val c = LTC.ctrans env c
		 val q = Tal.Tapp (Tal.Con c)
		 val state = TS.emit state (Tal.Coerce (loc,[q]))
		 val state = sv32_place_in_temp state env loc sv
	       in state
	       end

	      | Coerce (sv1,sv2) => 
	       let
		 val res = 
		   (case sv1
		      of Coercion q => 
			let
			  val qs = coercion env q
			  val state = TS.emit state (Tal.Coerce (loc,qs))
			  val state = sv32_place_in_temp state env loc sv2
			in state
			end
		       | _ =>  (* Abstract coercion call.  Place arg in EAX and call coercion *)
			let
			  val state = TS.shift_temp state loc (Tal.Reg Tal.Eax)
			  val (state,goc) = allocate_regimm_for' state env sv1
			  val goc = coercion_call_instantiate state env goc
			  val state = TS.emit state (Tal.Call goc)
			  val state = sv32_trans state env (false,true,true) (#1 goc) sv1
			  val state = sv32_trans state env (false,false,false) (Tal.Reg Tal.Eax) sv2
			in state
			end)
	       in res
	       end)
	val _ = debugdo(7,fn () => (print "Finished sv32_place_in_temp"))

      in res
      end

    fun sv32_unpack_rm state env rm a sv32 =
      (case rm
	 of Tal.Reg r => 
	   let
	     val state = TS.emit state (Tal.Unpack (a,r,(rm,[])))
	     val state = sv32_trans_reg state env r sv32
	   in state
	   end
	  | gop => 
	   let
	     val state = TS.emit state (Tal.Sunpack (a,rm))
	     val state = sv32_trans_rm state env rm sv32
	   in state
	   end)


    fun allocate_for64 state env sv64 : TS.state * Tal.genop = 
      (case sv64
	 of Var_64 x => 
	   let
	     val c = find_varcon64 (env,x)
	     val (state,gop) =  TS.reserve_for_var64 state x c
	   in (state,gop)
	   end
	  | Const_64 _ => error "No floating point immediates")

    fun sv64_trans state env (loc : Tal.genop) (sv64 : Lil.sv64) : TS.state = 
      (case sv64
	 of Var_64 x => 
	   TS.init_var64 state loc x (find_varcon64 (env,x))
	  | Const_64 _ => error "No floating point immediates")

    fun gop2reg gop =
      (case gop
	 of Tal.Reg r => r
	  | _ => error "gop2reg:Not a register")


    fun define_var32_in_reg state x = 
      let
	val (state,gop) = TS.define_var32 false state x 
	val r = gop2reg gop
      in (state,r)
      end
    
    fun reserve_reg8 state = TS.reserve_one_of_regs state [Tal.Eax,Tal.Ebx,Tal.Ecx,Tal.Edx]

    (* Value "already" in EAX *)
    fun return state env i = 
      let
	val state = TS.comment state 1 "Returned"
	val state = emit_retn state i
	val state = pop_frame state (i + 0w4)
(*	val state = place_ret_addr state env*)
	val state = TS.comment state 1 "Begin return sequences"
      in state
      end

    fun conditional_branch state env cond labels = 
      (case labels
	 of (L d1,L d2) => 
	   let
	     val state = jmp_label state env d2 
	     val state = jcc_label state env cond d1 
	   in state
	   end
	  | (F fc, L lc) => 
	   let
	     val state = fallthru state env fc
	     val state = jcc_label state env (Tal.negate_condition cond) lc
	   in state 
	   end
	  | (L lc, F fc) => 
	   let
	     val state = fallthru state env fc
	     val state = jcc_label state env cond lc
	   in state
	   end
	  | _ => error "Bad condition code arg")

    fun jmp_dest state env dest = 
      (case dest
	 of RET i => return state env i
	  | CC args => conditional_branch state env Tal.NotEq args
	  | EFF gd => jmp_genaddr state env gd
	  | CONT (gd,_) => jmp_genaddr state env gd)

    fun dest2step state env dest = 
      let
	val state =jmp_dest state env dest
	val (state,dest) = 
	  (case dest
	     of RET i => 
	       let
		 val rvar = get_rvar()
		 val state = TS.load_var state Tal.Eax rvar (TS.LilCon (get_rtype env))
	       in (state,CONT (S,rvar))
	       end
	      | CC (g1,g2) => 
	       let
		 val rvar = Name.fresh_named_var "bvar"
		 val (state,gop) =  TS.reserve_for_var true state rvar (TS.TalCon (Tal.pcbytes Tal.B4))
		 val state = TS.emit state (Tal.Cmp ((gop,[]),(Tal.Immed 0w0,[])))
		 val state = TS.init_var32 state gop rvar (TS.TalCon (Tal.pcbytes Tal.B4))
	       in (state,CONT (S,rvar))
	       end
	      | EFF _ => (state,EFF S)
	      | CONT (g,v) => (state,CONT (S,v)))
      in (state,dest)
      end


    fun sv32_return_trans state env dest sv32 = 
      let
	val () = chat 5 "Started sv32_return_trans"
	val () = debugdo(4,fn () => TS.sanity_check state)
	val state = jmp_dest state env dest
	val state = 
	  case dest
	    of RET _ => 
	      let
		val state = TS.reserve_reg state Tal.Eax
		val state = sv32_place_in_temp state env (Tal.Reg Tal.Eax) sv32
	      in state
	      end
	     | CC args =>
	      let
		val () = chat 7 "Started sv32_return_trans CC"
		val (state,goc) = allocate_rm_for' state env sv32
		val state = TS.emit state (Tal.Cmp (goc,(Tal.Immed 0w0,[])))
		val state = sv32_trans_rm' state env goc sv32
		val () = chat 7 "Finished sv32_return_trans CC"
	      in state
	      end
	     | EFF gd => state
	     | CONT (gd,x) => 
	      let
		val (state,gop) = TS.define_var32 true state x
		val state = sv32_trans_gop state env gop sv32
	      in state
	      end
	val () = chat 5 "Finished sv32_return_trans"
	val () = debugdo(4,fn () => TS.sanity_check state)
      in state
      end
    
    fun sv64_return_trans state env dest sv64 = 
      let
	val state = jmp_dest state env dest
      in
	case dest
	  of RET _ => error "Floats don't get returned"
	   | CC args => error "Floats aren't booleans"
	   | EFF gd => state
	   | CONT (gd,xf) => 
	    let
	      val (state,gop) = TS.define_var64 state xf
	      val state = sv64_trans state env gop sv64
	    in state
	    end
      end

    fun commute_args32 (sv1,sv2) =
      (case sv1
	 of Const_32 _ => (sv2,sv1)
	  | _ => (sv1,sv2))

    fun get_primarg32 arg = 
      (case arg
	 of arg32 sv => sv
	  | slice (_,sv) => sv
	  | _ => error "Large primarg")

    fun get_primarg64 arg = 
      (case arg
	 of arg64 sv => sv
	  | _ => error "Small primarg")

    fun get_2primargs32 args =
      (case args
	 of [arg1,arg2] => (get_primarg32 arg1,get_primarg32 arg2)
	  | _ => error "Not a binary 32bit prim")

    fun get_2primargs64 args =
      (case args
	 of [arg1,arg2] => (get_primarg64 arg1,get_primarg64 arg2)
	  | _ => error "Not a binary 64bit prim")

    fun get_1primarg32 args =
      (case args
	 of [arg] => get_primarg32 arg
	  | _ => error "Not a unary 32bit prim")

    fun get_1primarg64 args =
      (case args
	 of [arg] => get_primarg64 arg
	  | _ => error "Not a unary 64bit prim")

    fun wordbin what state env dest (sv1,sv2) = 
      (case sv2
	 of Const_32 v => 
	   let
	     val v = LU.value2w32 v
	     val state = TS.emit state (Tal.ArithBin (what,dest,Tal.Immed v))
	     val state = sv32_trans_gop state env dest sv1
	   in state
	   end
	  | _ => 
	   let
	     val (state,src) = allocate_gop_for state env sv2
	     val state = TS.emit state (Tal.ArithBin (what,dest,src))
	     val state = sv32_trans_gop state env src sv2
	     val state = sv32_trans_gop state env dest sv1
	   in state
	   end)

    fun overflow32_check state env = 
      let
	val dest = exn_raise_instantiate state env (TTD.E.l_overflow,[]) 
	val state = jcc_label' state env Tal.Overflow dest
      in state
      end

    fun int32bin what state env dest args = 
      let
	val (sv1,sv2) = args
	val state = overflow32_check state env 
	val state = wordbin what state env dest (sv1,sv2)
      in state
      end

    fun word32bin what state env dest args = 
      let
	val (sv1,sv2) = args
	val state = wordbin what state env dest (sv1,sv2)
      in state
      end
(*
    fun truncate8 state env dest = 
      let
	val (state,r) = TS.reserve_temp false state
	val state = TS.emit state (Tal.Movpart (false,dest,Tal.RPe,r,Tal.Rpl))
	val state = TS.init_temp state (Tal.Reg r) dest
      in state
      end
*)
    fun word8bin what state env dest args = 
      let
	val (sv1,sv2) = args
(*	val state = truncate8 state env dest*)
	val state = wordbin what state env dest (sv1,sv2)
      in state
      end

    fun in_shift_range (w : TilWord64.word) = 
      let
	val w = TilWord64.toUnsignedHalf w
      in (w > 0w0) andalso (w < 0w32)
      end

    fun wordshift what state env dest (sv1,sv2) = 
      let
	fun default () = 
	  let
	    val state = TS.reserve_reg state Tal.Ecx
	  in (state,NONE)
	  end

	val (state,src) = 
	  (case sv2
	     of Const_32 (Prim.int (_,w)) => 
	       if in_shift_range w then 
		 (state,SOME (TilWord64.toUnsignedHalf w))
	       else default()
	     | Const_32 (Prim.uint (_,w)) => 
		 if in_shift_range w then 
		   (state,SOME (TilWord64.toUnsignedHalf w))
		 else default()

	     | _ => default())
	val state = TS.emit state (Tal.ArithSR (what,dest, src))

	val state = sv32_trans_gop state env dest sv1

	val state = 
	  (case src
	     of SOME _ => state
	      | NONE => sv32_trans_reg state env Tal.Ecx sv2)

      in state
      end


    fun gen32md state env how (sv1,sv2) = 
      let
	val state = TS.reserve_reg state Tal.Eax
	val state = TS.reserve_reg state Tal.Edx
	val (state,src) = allocate_rm_for state env sv2
	val state = TS.emit state (Tal.ArithMD(how,src))
	val state = sv32_trans_reg state env Tal.Eax sv1
	val state = TS.release_reg state Tal.Edx
	val state = sv32_trans_rm state env src sv2
      in state
      end

    fun gen32imd state env how args = 
      let
	val state = overflow32_check state env
      in gen32md state env how args
      end

    fun gen32d state env signed (sv1,sv2) = 
      let
	val div_zero = 	exn_raise_instantiate state env (TTD.E.l_div_zero,[]) 
	val nm = Name.fresh_internal_label "nm"
	val state = TS.reserve_reg state Tal.Eax
	val state = TS.reserve_reg state Tal.Edx
	val (state,src) = allocate_rm_for state env sv2
	val state = 
	  if signed then 
	    let
	      val state = TS.emit state (Tal.ArithMD(Tal.Idiv,src))
	      val state = TS.emit state (Tal.Conv(Tal.Cdq))
	    in state
	    end
	  else
	    let
	      val state = TS.emit state (Tal.ArithMD(Tal.Div,src))
	      val state = TS.emit state (Tal.Mov(Tal.Reg Tal.Edx,(Tal.Immed 0w0,[])))
	    in state
	    end
	val state = TS.emit state (Tal.Jcc(Tal.BelowEq,div_zero,NONE))
	val state = TS.emit state (Tal.Cmp((src,[]),(Tal.Immed 0w0,[])))
	val state = sv32_unpack_rm state env src nm sv2
	val state = sv32_trans_reg state env Tal.Eax sv1
	val state = TS.release_reg state Tal.Edx
      in state
      end

    (* Assume Eax, Edx already free-ed. *)
    fun intquot state env args = gen32d state env true args
    fun intrem state env args  = gen32d state env true args
    fun worddiv state env args = gen32d state env false args
    fun wordmod state env args = gen32d state env false args
    fun word32mul state env args = gen32md state env Tal.Mul args
    fun int32mul state env args = gen32imd state env Tal.Imul1 args

    fun setref state env args = 
      let
	val (state,refreg,valri,refsv,valsv) = 
	  (case args
	     of [arg32 refsv,arg32 valsv] => 
	       let
		 val (state,refreg) = allocate_reg_for state env refsv
		 val (state,valri) = allocate_regimm_for' state env valsv
	       in (state,refreg,valri,refsv,valsv)
	       end
	      | _ => error "Bad prim args")
	val md = tuple_field refreg 0w0
	val state = TS.emit state (Tal.Mov(md,valri))
	val state = sv32_trans_reg state env refreg refsv
	val state = sv32_trans_regimm' state env valri valsv
      in state
      end


    fun deref state env destop args = 
      (case destop
	 of NONE => state (* No effect, so why bother? *)
	  | SOME x => 
	   let
	     val sv = get_1primarg32 args
	     val (state,tr) = define_var32_in_reg state x
	     val md = tuple_field tr 0w0
	     val state = TS.init_temp state (Tal.Reg tr) (md,[])
	     val state = sv32_trans_reg state env tr sv
	   in state
	   end)


    fun destop2reg state destop r = 
      (case destop
	 of SOME x => TS.define_var_from_reg state x r
	  | NONE => TS.reserve_reg state r)

    fun destop2reg_any state destop = 
      (case destop
	 of SOME x => define_var32_in_reg state x
	  | NONE => TS.reserve_temp_reg state)

    fun primarglist_size_in_words args : Tal.int32 = 
      (case args
	 of [] => 0w0
	  | (arg32 _)::args => 0w1 + (primarglist_size_in_words args)
	  | (slice _)::args => 0w1 + (primarglist_size_in_words args)
	  | (arg64 _)::args =>  0w2 + (primarglist_size_in_words args))

    fun primarglist_flags args = 
      (case args
	 of [] => []
	  | (arg32 _)::args => true:: (primarglist_flags args)
	  | (slice _)::args => true:: (primarglist_flags args)
	  | (arg64 _)::args => false::(primarglist_flags args))


    fun place_extern_outargs state env args = 
      let
	val () = chat 3 "Starting outargs for ccall\n"

	fun loop (state,[]) = state
	  | loop (state,arg::args)= 
	  (case arg
	     of arg32 sv32 =>
	       let
		 val () = chat 7 "reserving outarg\n"
		 val (state,d) = TS.reserve_next_arg32 state
		 val state     = loop (state,args)
		 val () = chat 7 "initializing outarg\n"
		 val state     = sv32_trans_gop state env d sv32
	       in state
	       end
	      | slice (_,sv32) =>
	       let
		 val () = chat 7 "reserving outarg\n"
		 val (state,d) = TS.reserve_next_arg32 state
		 val state     = loop (state,args)
		 val () = chat 7 "initializing outarg\n"
		 val state     = sv32_trans_gop state env d sv32
	       in state
	       end
	      | arg64 sv64 => 
	       let
		 val (state,d) = TS.reserve_next_arg64 state
		 val state     = loop (state,args)
		 val state     = sv64_trans state env d sv64
	       in state
	       end)

	val state = loop (state,rev args)
	val () = chat 3 "Finishing outargs for ccall\n"
      in state
      end

    fun externapp_trans state env dest (f,args) = 
      let
	val state = jmp_dest state env dest
	val state = TS.comment state 1 "Extern C call finished"
	val state = 
	  (case dest
	     of CONT (d,x) => TS.define_var_from_reg state x Tal.Eax
	      | _ => TS.reserve_reg state Tal.Eax)
	     
	val state = TS.reserve_reg state Tal.Ecx
	val state = TS.reserve_reg state Tal.Edx
	val state = TS.comment state 2 "restoring caller saves from ccall"
	  
	val state = TS.pop_outargs state
	val outarg_space = primarglist_size_in_words args

	val state = popn state (0w4*outarg_space)
	val state = TS.alloc_outargs' state (primarglist_flags args)

	val (state,floc)  = allocate_goc_for state env f
	val floc = extern_call_instantiate state env floc
	val state = TS.emit state (Tal.Call floc)

	val state = TS.release_reg state Tal.Eax
	val state = TS.release_reg state Tal.Ecx
	val state = TS.release_reg state Tal.Edx
	  
	val state = sv32_trans_goc state env floc f
	val state = place_extern_outargs state env args
	val state = TS.comment state 1 "Starting Ccall"
      in state
      end


    fun externappf_trans state env dest (f,args) = 
      let
	val state = jmp_dest state env dest
	val state = TS.comment state 1 "Extern Ccall64 finished"
	val state = 
	  (case dest
	     of CONT (d,xf) => 
	       let
		 val (state,oper) = TS.define_var64 state xf 
		 val state = store_float_and_pop state oper
		 val state = TS.release_temp64 state oper
	       in state
	       end
	      | _ => state)

	val state = TS.reserve_reg state Tal.Eax
	val state = TS.reserve_reg state Tal.Ecx
	val state = TS.reserve_reg state Tal.Edx

	val state = TS.comment state 2 "restoring caller saves from ccall64"
	  
	val state = TS.pop_outargs state
	val outarg_space = primarglist_size_in_words args

	val state = popn state (0w4*outarg_space)
	(* CHEAT!  Knows how alloc_outargs is implemented *)
	val state = TS.alloc_outargs' state (primarglist_flags args)
	val (state,floc)  = allocate_goc_for state env f
	val floc = extern_call_instantiate state env floc
	val state = TS.emit state (Tal.Call floc)
	val state = sv32_trans_goc state env floc f
	val state = TS.release_reg state Tal.Eax
	val state = TS.release_reg state Tal.Ecx
	val state = TS.release_reg state Tal.Edx

	val state = place_extern_outargs state env args
	val () = chat 3 "Finishing outargs for ccall64\n"
	val state = TS.comment state 1 "Starting Ccall64"
      in state
      end

    fun mk_ref state env destop args = 
      let

	val () = chat 5 "Allocating ref\n"
	val state = TS.comment state 1 "mk_ref done"

	val state = destop2reg state destop Tal.Eax

	val nm = Name.fresh_internal_label "mk_ref"

	val state = TS.emit state (Tal.Coerce(Tal.Reg Tal.Eax,[Tal.Forgetname]))
	val state = TS.emit state (Tal.ForgetUnique nm)

	val () = chat 5 "Initializing ref\n"
	  
	val ((state,svloc),sv) = 
	  (case args
	     of [arg32 sv] => (allocate_regimm_for' state env sv,sv)
	      | _ => error "bad primargs to mk_ref")

	val refloc = Tal.Prjr ((Tal.Eax,[]),0w0,NONE)
	val state = TS.emit state (Tal.Mov (refloc,svloc))
	val state = sv32_trans_regimm' state env svloc sv

	val state = TS.release_reg state Tal.Eax
	val state = TS.free_reg state Tal.Ecx
	val state = TS.free_reg state Tal.Edx
	val state = TS.comment state 2 "restoring caller saves"

	val state = TS.emit state (Tal.Malloc (nm,0w4,NONE))
	val state = TS.comment state 1 "Allocating ref"
	val () = chat 5 "mk_ref done\n"
      in state
      end

    fun length_table state env destop args = 
      let
	(* Reserves dreg *)
	val (state,dreg) = destop2reg_any state destop
	val arreg = dreg
	val array_size_var = Name.fresh_internal_label "?sz" 
	val arrsv = get_1primarg32 args
	val (state,arr) = allocate_goc_for state env arrsv
	val state = TS.emit state (Tal.Coerce (Tal.Reg dreg,[Tal.Subsume Tal.cbyte4]))
	val state = TS.emit state (Tal.Mov(Tal.Reg dreg,(Tal.Prjr((arreg,[]),0w0,NONE),[])))
	val state = TS.emit state (Tal.Unpack (array_size_var,arreg,arr))

	val state = sv32_trans_goc state env arr arrsv
	val state = TS.release_reg state arreg
      in
	state
      end


    fun emit_bounds_check state env (arreg,arrsv) (ireg,indexsv) = 
      let
	val array_size_var = Name.fresh_internal_label "?sz" 
	val index_var      = Name.fresh_internal_label "i" 

	val (state,arr) = allocate_goc_for state env arrsv
	val (state,idx) = allocate_goc_for state env indexsv

	val dest = exn_raise_instantiate state env (TTD.E.l_raise_subscript,[]) 
	val state = jcc_label' state env Tal.AboveEq dest

	val state = TS.emit state(Tal.Cmp((Tal.Reg ireg,[]),(Tal.Prjr((arreg,[]),0w0,NONE),[])))
	val state = TS.emit state (Tal.Unpack(index_var,ireg, idx))
	val state = TS.emit state (Tal.Unpack(array_size_var, arreg, arr))

	val state = sv32_trans_goc state env arr arrsv
	val state = sv32_trans_goc state env idx indexsv

      in state
      end

    fun update size state env args = 
      let

	val (arrsv,indexsv,valarg) = 
	  (case args
	     of [arg32 arr,arg32 index,arg] => (arr,index,arg)
	      | _ => error "bad args to update")

	val (state,arreg) = TS.reserve_temp_reg state 
	val (state,ireg) = TS.reserve_temp_reg state

	val state = 
	  (case valarg
	     of (slice (Lil.B1,valsv)) => 
	       let
		 val (state,valreg) = reserve_reg8 state
		 val memloc = Tal.Prjr((arreg,[]),0w0,SOME(Tal.B1,ireg))
		 val state = TS.emit state (Tal.Movpart(true,memloc,Tal.RPl, Tal.Reg valreg, Tal.RPl))
		 val state = sv32_trans_reg state env valreg valsv
	       in state
	       end
	     | (arg32 valsv) => 
	       let
		 val (state,valreg) = allocate_reg_for state env valsv
		 val memloc = Tal.Prjr((arreg,[]),0w0,SOME(Tal.B4,ireg))
		 val state = TS.emit state (Tal.Mov(memloc,(Tal.Reg valreg,[])))
		 val state = sv32_trans_reg state env valreg valsv
	       in state
	       end
	     | (arg64 valsv) => 
	       let
		 val (state,gop) = allocate_for64 state env valsv
		 val state = store_float_and_pop state gop
		 val memloc = Tal.Prjr((arreg,[]),0w0,SOME(Tal.B8,ireg))
		 val state = load_float state memloc
		 val state = sv64_trans state env gop valsv
	       in state
	       end
	      | _ => error "Bad array update")

	val arrptr = Tal.Prjr((arreg,[]),0w4,NONE)
	val state = TS.emit state (Tal.Mov (Tal.Reg arreg,(arrptr,[])))

	val state = emit_bounds_check state env (arreg,arrsv) (ireg,indexsv)
	val state = TS.release_reg state arreg
	val state = TS.release_reg state ireg
      in
	state
      end

    fun table2size t = 
      (case t
	 of Prim.IntArray is => LU.i2size is
	  | Prim.FloatArray fs => Lil.B8
	  | Prim.FloatVector fs => Lil.B8
	  | Prim.OtherArray true => Lil.B4
	  | _ => error "Bad table")

    fun sub t state env destopt args = 
      let
	val size = table2size t
	val (arrsv,indexsv) = get_2primargs32 args
	val (state,arreg) = TS.reserve_temp_reg state 
	val (state,ireg) = TS.reserve_temp_reg state

	val state = 
	  (case (size,destopt)
	     of (Tal.B1,_) => 
	       let
		 val (state,dreg) = reserve_reg8 state
		 val state = TS.release_reg state dreg
		 val state = (case destopt
				of SOME x => TS.define_var_from_reg state x dreg
				 | _ => TS.reserve_reg state dreg)
		 val state = 
		   TS.emit state (Tal.Movpart(true, Tal.Reg dreg, Tal.RPl, 
					      Tal.Prjr((arreg,[]),0w0,SOME(Tal.B1,ireg)), Tal.RPl))
		 (* zero out dreg to avoid spurious high bits *)
		 val state = 
		   TS.emit state (Tal.Mov(Tal.Reg dreg,(Tal.Immed 0w0,[])))
		 val state = TS.release_reg state dreg
	       in state
	       end
	      | (Tal.B4,_) => 
	       let
		 val (state,dreg) = case destopt
				      of SOME x => TS.define_var32 false state x
				       | NONE => TS.reserve_temp false state
		 val memloc = Tal.Prjr((arreg,[]),0w0,SOME(Tal.B4,ireg))
		 val state = TS.emit state (Tal.Mov(dreg,(memloc,[])))
		 val state = TS.release_temp state dreg
	       in state
	       end
	      | (Tal.B8,NONE) => 
	       let
		 val memloc = Tal.Prjr((arreg,[]),0w0,SOME(Tal.B8,ireg))
		 val state = TS.emit state (Tal.FPsomeargs(Tal.Fld,Tal.FPgenop (Tal.B8,memloc)))
	       in state
	       end
	      | _ => error "Bad array subscript")

	val arrptr = Tal.Prjr((arreg,[]),0w4,NONE)
	val state = TS.emit state (Tal.Mov (Tal.Reg arreg,(arrptr,[])))

	val state = emit_bounds_check state env (arreg,arrsv) (ireg,indexsv)
	val state = TS.release_reg state arreg
	val state = TS.release_reg state ireg	  
      in
	state
      end

    fun table_type env t cs = 
      let
	val lc = 
	  (case (t,cs)
	     of (Prim.IntArray is,[]) => LD.T.intt (LU.i2size is)
	      | (Prim.FloatArray fs,[]) => LD.T.float ()
	      | (Prim.FloatVector fs,[]) => LD.T.float ()
	      | (Prim.OtherArray true,[c]) => c
	      | _ => error "table_type:Bad table")
	val c = LTC.ctrans env lc
      in c
      end

    fun create_empty_table t cs state env destop args = 
      (case destop 
	 of NONE => state
	  | SOME x => 
	   let
	     val c = table_type env t cs
	     val (state,gop) = TS.define_var32 true state x
	     val array0 = 
	       let
		 val l = case table2size t
			   of Tal.B1 => TTD.E.l_wordarray_zero
			    | Tal.B4 =>  TTD.E.l_array_zero
			    | Tal.B8 =>  TTD.E.l_floatarray_zero
			    | _ => error "bad size"
	       in
		 (Tal.Addr l,[Tal.Tapp (Tal.Con c)])
	       end
	     val state = TS.emit state (Tal.Mov (gop,array0))
	     val state = TS.release_temp state gop
	   in state
	   end)

    fun create_table t cs state env destop args = 
      let
	val dest = 
	  (case destop
	     of SOME x => CONT(S,x)
	      | NONE => EFF S)
	val f = 
	  (case (t,cs)
	     of (Prim.IntArray is,[]) => Lil.Label(TTD.E.l_intarray (LU.i2size is))
	      | (Prim.FloatArray fs,[]) => Lil.Label(TTD.E.l_floatarray)
	      | (Prim.FloatVector fs,[]) => Lil.Label (TTD.E.l_floatarray)
	      | (Prim.OtherArray true,[c]) => LD.E.tapp' c (Lil.Label TTD.E.l_ptrarray)
	      | _ => error "table_type:Bad table")
      in externapp_trans state env dest (f,args)
      end

    fun relop state env (sv1,sv2) =
      let
	val () = chat 7 "Starting relop"
	val () = debugdo(7,fn () => TS.sanity_check state)
	val () = debug_print_state "before allocate1" state
	val (state,gop1) = allocate_rm_for state env sv1
	val () = debug_print_state "before allocate2" state
	val (state,goc2) = allocate_regimm_for' state env sv2
	val () = debug_print_state "after allocation" state
	val state = TS.emit state (Tal.Cmp((gop1,[]),goc2))
	val state = sv32_trans_rm state env gop1 sv1
	val state = sv32_trans_regimm' state env goc2 sv2
	val () = chat 7 "Finished relop"
	val () = debugdo(7,fn () => TS.sanity_check state)
      in state
      end

    fun frelop state env (sv1,sv2) = 
      let
	val (state,gop1) = allocate_for64 state env sv1
	val (state,gop2) = allocate_for64 state env sv2
	val state = fpstack_pop state
	val state = TS.emit state (Tal.FPsomeargs (Tal.Fucomip,Tal.FPstack2(true,1)))
	val state = load_float state gop1
	val state = load_float state gop2
	val state = sv64_trans state env gop1 sv1
	val state = sv64_trans state env gop2 sv2
      in state
      end

    datatype prim32class = 
      Arithbin of (TS.state -> TE.env -> Tal.genop -> (sv32 * sv32) -> TS.state) * (sv32 * sv32)
      | Arithun of Tal.arithun * sv32
      | Arithmd of (TS.state -> TE.env -> (sv32 * sv32) -> TS.state) * Tal.reg * (sv32 * sv32)
      | Relop of Tal.condition * (sv32 * sv32)
      | Id of sv32
      | F2i of sv64
      | FRelop of Tal.condition * (sv64 * sv64)
      | EffPrim of (TS.state -> TE.env -> primarg list -> TS.state) * (primarg list)
      | OtherPrim of (TS.state -> TE.env -> Lil.var option -> primarg list -> TS.state) * (primarg list)

    fun classify_prim32 (p : Prim.prim,cs : Lil.con list, primargs : Lil.primarg list) : prim32class = 
      (case p
	 of Prim.plus_int Prim.W32   => Arithbin(int32bin Tal.Add, commute_args32(get_2primargs32 primargs))
	  | Prim.minus_int Prim.W32  => Arithbin(int32bin Tal.Sub, get_2primargs32 primargs)
	  | Prim.plus_uint Prim.W32  => Arithbin(word32bin Tal.Add, commute_args32(get_2primargs32 primargs))
	  | Prim.minus_uint Prim.W32 => Arithbin(word32bin Tal.Sub, get_2primargs32 primargs)
	  | Prim.plus_uint Prim.W8   => Arithbin(word8bin Tal.Add, commute_args32(get_2primargs32 primargs))
	  | Prim.minus_uint Prim.W8  => Arithbin(word8bin Tal.Sub, get_2primargs32 primargs)
	  | Prim.and_int _ => Arithbin(word32bin Tal.And, commute_args32(get_2primargs32 primargs))
	  | Prim.or_int _  => Arithbin(word32bin Tal.Or, commute_args32(get_2primargs32 primargs))
	  | Prim.xor_int _ => Arithbin(word32bin Tal.Xor, commute_args32(get_2primargs32 primargs))
	  | Prim.lshift_int Prim.W32  => Arithbin(wordshift Tal.Shl, get_2primargs32 primargs)
	  | Prim.rshift_int Prim.W32  => Arithbin(wordshift Tal.Sar, get_2primargs32 primargs)
	  | Prim.rshift_uint Prim.W32 => Arithbin(wordshift Tal.Shr, get_2primargs32 primargs)
	  | Prim.mul_int _  => Arithbin(int32bin Tal.Imul2, get_2primargs32 primargs)
	  | Prim.mul_uint _ => Arithbin(word32bin Tal.Imul2, get_2primargs32 primargs)

(*	  | Prim.mul_int _  => Arithmd(int32mul, Tal.Eax, get_2primargs32 primargs)
	  | Prim.mul_uint _ => Arithmd(word32mul, Tal.Eax, get_2primargs32 primargs)
*)
	  | Prim.quot_int _ => Arithmd(intquot, Tal.Eax, get_2primargs32 primargs)
	  | Prim.rem_int _  => Arithmd(intrem, Tal.Edx, get_2primargs32 primargs)
	  | Prim.div_uint _ => Arithmd(worddiv, Tal.Eax, get_2primargs32 primargs)
	  | Prim.mod_uint _ => Arithmd(wordmod, Tal.Edx, get_2primargs32 primargs)
	       
	  | Prim.div_int W32 =>   error "ml style div not implemented!"
	  | Prim.mod_int W32 =>   error "ml style mod not implemented!"
	       
	  (* Unops *)
	   
	  | Prim.not_int _ => Arithun(Tal.Not,get_1primarg32 primargs)
	  | Prim.neg_int Prim.W32 => Arithun(Tal.Neg,get_1primarg32 primargs)
	  | Prim.neg_int is => classify_prim32 (Prim.minus_int is, [], arg32(Const_32 (Prim.int (is,TilWord64.fromUnsignedHalf 0w0)))::primargs)
	  | Prim.abs_int is => error "abs_int should have been compiled away already"

          (* relops *)

	  (* Int only good for W32 *)
          | Prim.less_int Prim.W32      => Relop(Tal.Less,get_2primargs32 primargs)
	  | Prim.greater_int Prim.W32   => Relop(Tal.Greater,get_2primargs32 primargs)
	  | Prim.lesseq_int Prim.W32    => Relop(Tal.LessEq,get_2primargs32 primargs)
	  | Prim.greatereq_int Prim.W32 => Relop(Tal.GreaterEq,get_2primargs32 primargs)

	  (* Ok for all sizes *)
	  | Prim.eq_int _  => Relop(Tal.Eq,get_2primargs32 primargs)
	  | Prim.neq_int _ => Relop(Tal.NotEq,get_2primargs32 primargs)

	  (* Uint ok for all sizes *)
	  | Prim.less_uint _      => Relop(Tal.Below,get_2primargs32 primargs)
	  | Prim.greater_uint _   => Relop(Tal.Above,get_2primargs32 primargs)
	  | Prim.lesseq_uint _    => Relop(Tal.BelowEq,get_2primargs32 primargs)
	  | Prim.greatereq_uint _ => Relop(Tal.AboveEq,get_2primargs32 primargs)

	  (* trap instructions *)
	  | Prim.soft_vtrap _ => error "No traps"
	  | Prim.soft_ztrap _ => error "No traps"
	  | Prim.hard_vtrap _ => error "No traps"
	  | Prim.hard_ztrap _ => error "No traps"

      (* conversions amongst floats, ints, uints with w32 and f64 *)
	  | Prim.float2int => F2i(get_1primarg64 primargs)

	  | Prim.int2uint   _ => Id(get_1primarg32 primargs)
	  | Prim.uint2int   _ => Id(get_1primarg32 primargs)
	  | Prim.int2int    _ => Id(get_1primarg32 primargs)
	  | Prim.uint2uint  _ => Id(get_1primarg32 primargs)
	  | Prim.uinta2uinta _ => Id(get_1primarg32 primargs)
	  | Prim.uintv2uintv _ => Id(get_1primarg32 primargs)

	  | Prim.less_float      _ => FRelop(Tal.Below,get_2primargs64 primargs)
	  | Prim.greater_float   _ => FRelop(Tal.Above,get_2primargs64 primargs)
	  | Prim.lesseq_float    _ => FRelop(Tal.BelowEq,get_2primargs64 primargs)
	  | Prim.greatereq_float _ => FRelop(Tal.AboveEq,get_2primargs64 primargs)
	  | Prim.eq_float _       => FRelop(Tal.Eq,get_2primargs64 primargs)
	  | Prim.neq_float _      => FRelop(Tal.NotEq,get_2primargs64 primargs)


	  (* array and vectors *)
	  | Prim.array2vector _ => Id(get_1primarg32 primargs)
	  | Prim.vector2array _ => Id(get_1primarg32 primargs)

	  | Prim.update t => EffPrim(update t,primargs)
	  | Prim.setref   => EffPrim(setref,primargs)

	  | Prim.create_table t       => OtherPrim(create_table t cs,primargs)
	  | Prim.create_empty_table t => OtherPrim(create_empty_table t cs,primargs)
	  | Prim.sub t                => OtherPrim(sub t,primargs)
	  | Prim.length_table t       => OtherPrim(length_table,primargs)

	  | Prim.mk_ref => OtherPrim(mk_ref,primargs)
	  | Prim.deref => OtherPrim(deref,primargs)

	  | Prim.equal_table _ => error "equal_table should be Ptreq"
	  | Prim.eq_ref => error "eq_ref should be Ptreq"
	  | _ => 
	   (print "Bad 32 bit prim\n";
	    PpLil.pp_op32 (Prim32(p,cs,primargs));
	    error "Prim badness"))

(*
	  (* LArge *)
	  | Prim.int2float => error "No floats"

      (* floatint-point operations *)
	  | Prim.neg_float => FNoarg ()
	  | Prim.abs_float => error "No floats"
	  | Prim.plus_float => error "No floats"
	  | Prim.minus_float => error "No floats"
	  | Prim.mul_float => error "No floats"
	  | Prim.div_float => error "No floats"
*)


    fun cprim32_trans (state : TS.state) (env : TE.env) (destop : Lil.var option) (cp : prim32class) : TS.state = 
      let

	fun get_dest_for state destop = 
	  (case destop
	     of SOME x => TS.define_var32 false state x
	      | NONE => TS.reserve_temp false state)

	val state = 
	  (case cp
	     of Arithbin (f,(sv1,sv2)) => 
	       let
		 val (state,dest) = get_dest_for state destop 
	       in f state env dest (sv1,sv2)
	       end
	      | Arithun (what,sv) => 
	       let
		 val (state,dest) = get_dest_for state destop
		 val state = TS.emit state (Tal.ArithUn (what,dest))
		 val state = sv32_trans_rm state env dest sv
	       in state
	       end
	      | Arithmd (f,dreg,(sv1,sv2)) => 
	       let
		 val state = 
		   (case destop
		      of SOME x => TS.define_var_from_reg state x dreg
		       | NONE => state)
		 val state = TS.release_reg state dreg
		 val state = TS.free_reg state Tal.Edx
		 val state = TS.free_reg state Tal.Eax

	       in f state env (sv1,sv2)
	       end

	      | Relop (cond,args) =>
	       (case destop
		  of NONE => state
		   | SOME x => 
		    let (* Should try to commute args. *)
		      val (state,dest) = reserve_reg8 state
		      val state = TS.release_reg state dest
		      val state = TS.define_var_from_reg state x dest
		      val state = TS.emit state (Tal.Setcc (cond,Tal.Reg dest))
		      val state = TS.init_temp state (Tal.Reg dest) (Tal.Immed 0w0,[])
		      val state = relop state env args
		    in state
		    end)
	      | FRelop (cond,args) =>
	       (case destop
		  of NONE => state
		    | SOME x => 
		    let (* Should try to commute args. *)
		      val (state,dest) = reserve_reg8 state
		      val state = TS.release_reg state dest
		      val state = TS.define_var_from_reg state x dest
		      val state = TS.emit state (Tal.Setcc (cond,Tal.Reg dest))
		      val state = TS.init_temp state (Tal.Reg dest) (Tal.Immed 0w0,[])
		      val state = frelop state env args
		    in state
		    end)
	      | Id sv =>
	       (case destop
		  of NONE => state
		   | SOME x => 
		    let
		      val (state,d) = TS.define_var32 true state x
		    in sv32_trans_gop state env d sv
		    end)
	      | F2i sv64 =>
	       (case destop
		  of NONE => state
		   | SOME x => 
		    let
		      val (state,d) = 
			(case TS.define_var32 true state x
			   of (state,Tal.Reg r) => 
			     let
			       val (state,gop) = TS.reserve_temp_slot state
			       val state = TS.init_temp state (Tal.Reg r) (gop,[])
			     in (state,gop)
			     end
			    | other => other)
		      val (state,src) = allocate_for64 state env sv64
		      val state = TS.emit state (Tal.FPsomeargs(Tal.Fistp,Tal.FPgenop (Tal.B4,d)))
		      val state = TS.release_temp state d
		      val state = load_float state src
		      val state = sv64_trans state env src sv64
		    in state
		    end)
	      | EffPrim (f,args) => 
	       let
		 val state = 
		   (case destop
		      of NONE => state
		       | SOME x => 
			let
			  val (state,d) = TS.define_var32 true state x
			in sv32_trans_gop state env d Unit
			end)
		 val state = f state env args
	       in state
	       end
	      | OtherPrim (f,args) => 
	       let
		 val state = f state env destop args
	       in state
	       end)
      in state
      end
	 
    fun prim32_trans state env destloc (args as (p,cs,primargs)) = 
      let

	val state = 
	  (case (classify_prim32 (p,cs, primargs),destloc)
	     of (Relop (cond,args),CC cclbls) => 
	       let
		 val () = chat 6 "Starting relop/CC"
		 val state = conditional_branch state env cond cclbls
		 val state = relop state env args
		 val () = chat 6 "Finished relop/CC"
	       in state
	       end
	      | (FRelop (cond,args),CC cclbls) => 
	       let
		 val state = conditional_branch state env cond cclbls
		 val state = frelop state env args
	       in state
	       end
	      | (EffPrim (f,args), EFF gd) => 
	       let
		 val state = jmp_genaddr state env gd
		 val state = f state env args
	       in state
	       end
	      | (_,CC _) => error "CC of non-relop prim"
	      | (p , EFF gd) => 
	       let
		 val state = jmp_genaddr state env gd
		 val state = cprim32_trans state env NONE p
	       in state
	       end
	      | (p,RET i) =>
	       let
		 val state = TS.free_reg state Tal.Eax
		 val state = return state env i
		 val rvar = get_rvar()
		 val state = TS.load_var state Tal.Eax rvar (TS.TalCon (Tal.cvoid Tal.k4byte))
		 val state = cprim32_trans state env (SOME rvar) p
		 val state = TS.kill_var32 state rvar
	       in state
	       end
	      | (p,CONT (gd,dest)) =>
	       let
		 val state = jmp_genaddr state env gd
		 val state = cprim32_trans state env (SOME dest) p
	       in state
	       end)
      in state
      end

    fun tailcall_pop_frame state eargs fargs = 
      let
	val framesz = TS.get_frame_size state
	val inargsz = TS.get_inarg_size state
	val inarg_used_size = 0w4 * (LU.i2w (eargs + 2 * fargs + 1))
	val (inargsz,state) = if inargsz < inarg_used_size 
				then (inarg_used_size,TS.alloc_inargs state eargs fargs)
			      else 
				let
				  val extracount = LU.w2i ((inargsz - inarg_used_size) div 0w4 )
				in (inargsz,TS.adjust_inargs state (extracount + eargs) fargs)
				end
	val total_size = framesz - inarg_used_size
	val state = popn state total_size
      in state
      end

    fun call_trans state env dest (f,eargs,fargs) = 
      (case (!do_tailcall,dest)
	 of (true,RET i) =>  (* Tailcall.  No live out. *)
	   let
	     val state = TS.comment state 1 "Finished Tailcall"
	     val () = chat 3 "Starting Tailcall\n"
	     val state = kill_ret_addr state
	     val (state,floc)  = allocate_regimm_for' state env f
	     val floc = tailcall_instantiate state env floc
	     val state = TS.emit state (Tal.Jmp floc)

	     val state = sv32_trans_reg' state env floc f

	     val numeargs = List.length eargs 
	     val numfargs = List.length fargs

	     val () = chat 3 "Tailcall popping frames \n"
	     val state = tailcall_pop_frame state numeargs numfargs


	     val () = chat 3 "Starting Tailcall outargs\n"


	     val state = 
	       let

		 fun floop (state,[]) = (0,state)
		   | floop (state,farg::fargs) = 
		   let
		     val (idx,state) = floop (state,fargs)
		     val (state,d) = TS.reserve_inarg64 state idx
		     val state     = sv64_trans state env d farg
		   in (idx + 1,state)
		   end
		 val (_,state) = floop(state,fargs)
		 fun loop (state,[]) = (0,state)
		   | loop (state,arg::args)= 
		   let
		     val (idx,state) = loop (state,args)
		     val () = chat 7 "reserving Tailcall outarg\n"
		     val (state,d) = TS.reserve_inarg32 state idx
		     val () = chat 7 "initializing Tailcall outarg\n"
		     val state     = sv32_trans_gop state env d arg
		   in (idx + 1,state)
		   end
		 val (rstart,state) = loop (state,eargs)
		 val state = place_ret_addr' state env rstart
	       in state
	       end
	     val () = chat 3 "Finished Tailcall\n"
	     val state = TS.comment state 1 "Begin Tailcall"
	   in state
	   end
	  | _ => (* Normal call *)
	   let
	     val state = TS.comment state 1 "Finished call"
	     val state = jmp_dest state env dest

	     val state = 
		(case dest
		   of CONT (d,x) => TS.define_var_from_reg state x Tal.Eax
		    | _ => TS.reserve_reg state Tal.Eax)

	     val state = TS.reserve_reg state Tal.Ecx
	     val state = TS.reserve_reg state Tal.Edx
	     val state = TS.comment state 2 "restoring caller saves"

	     val state = TS.pop_outargs state
	     val state = TS.alloc_outargs state (List.length eargs) (List.length fargs)

	     val (state,floc)  = allocate_goc_for state env f
	     val floc = call_instantiate state env floc
	     val state = TS.emit state (Tal.Call floc)

	     val state = TS.release_reg state Tal.Eax
	     val state = TS.release_reg state Tal.Ecx
	     val state = TS.release_reg state Tal.Edx

	     val state = sv32_trans_goc state env floc f


	     val () = chat 3 "Starting outargs\n"
	     fun loop (state,[]) = state
	       | loop (state,arg::args)= 
	       let
		 val () = chat 7 "reserving outarg\n"
		 val (state,d) = TS.reserve_next_arg32 state
		 val state     = loop (state,args)
		 val () = chat 7 "initializing outarg\n"
		 val state     = sv32_trans_gop state env d arg
	       in state
	       end

	     fun floop (state,[]) = loop (state,rev eargs)
	       | floop (state,farg::fargs) = 
	       let
		 val (state,d) = TS.reserve_next_arg64 state
		 val state     = floop (state,fargs)
		 val state     = sv64_trans state env d farg
	       in state
	       end
	     val state = floop (state,rev fargs)
	     val () = chat 3 "Finishing outargs\n"
	     val state = TS.comment state 1 "Begin call"
	   in state
	   end)


    fun lilprim_trans state env dest (lp,cs,sv32s,sv64s) = 
      let
	val state = jmp_dest state env dest
      in 
	case lp
	  of Box =>  
	    let
	      val () = chat 5 "Boxing float\n"
	      val state = TS.comment state 1 "Done boxing float"
	      val state = 
		(case dest
		   of CONT (d,x) => TS.define_var_from_reg state x Tal.Eax
		    | _ => TS.reserve_reg state Tal.Eax)

	      val nm = Name.fresh_internal_label "boxfloat"

	      val state = TS.emit state (Tal.Coerce(Tal.Reg Tal.Eax,[Tal.Forgetname]))
	      val state = TS.emit state (Tal.ForgetUnique nm)

	      val () = chat 5 "Initializing float element\n"


	      val sv64 = hd sv64s
	      val (state,gop) = allocate_for64 state env sv64
	      val boxloc = Tal.Prjr ((Tal.Eax,[]),0w0,NONE)

	      val state = store_float_and_pop state boxloc
	      val state = load_float state gop
	      val state = sv64_trans state env gop sv64

	      val state = TS.release_reg state Tal.Eax
	      val state = spill_caller_save state
	      val state = TS.emit state (Tal.Malloc (nm,0w8,NONE))
	      val state = TS.comment state 1 "Boxing float"
	      val () = chat 5 "Tuple done\n"
	    in state
	    end

	   | Tuple => 
	    let
	      val () = chat 5 "Allocating tuple\n"
	      val state = TS.comment state 1 "Finished tuple"
	      val state = 
		(case dest
		   of CONT (d,x) => TS.define_var_from_reg state x Tal.Eax
		    | _ => TS.reserve_reg state Tal.Eax)

	      val nm = Name.fresh_internal_label "tuplen"

	      val state = TS.emit state (Tal.Coerce(Tal.Reg Tal.Eax,[Tal.Forgetname]))
	      val state = TS.emit state (Tal.ForgetUnique nm)

	      val () = chat 5 "Initializing elements\n"

	      fun loop state i sv32s = 
		(case sv32s
		   of [] => (i,state)
		    | sv::sv32s => 
		     let
		       val (state,rim) = allocate_regimm_for' state env sv
		       val md = tuple_field Tal.Eax i
		       val state = TS.emit state (Tal.Mov (md,rim))
		       val state = sv32_trans_regimm' state env rim sv
		     in loop state (i+0w1) sv32s
		     end)
	      val (sz,state) = loop state 0w0 sv32s
	      val state = TS.comment state 1 "Initializing tuple elements"

	      val () = chat 5 "Cleaning up\n"
	      val state = TS.release_reg state Tal.Eax
	      val state = spill_caller_save state
	      val state = TS.emit state (Tal.Malloc (nm,sz*0w4,NONE))
	      val () = chat 5 "Tuple done\n"
	      val state = TS.comment state 1 "Allocating tuple"
	    in state
	    end
	   | Select w => 
	    let
	      val sv = hd sv32s
	      val (state,r) = 
		(case dest
		   of CONT (d,x) => define_var32_in_reg state x
		    | RET _ => (TS.reserve_reg state Tal.Eax,Tal.Eax)
		    | _ => TS.reserve_temp_reg state)
	      val (state,tr) = allocate_reg_for state env sv
	      val md = tuple_field tr w
	      val state = TS.emit state (Tal.Mov(Tal.Reg r,(md,[])))
	      val state = sv32_trans_reg state env tr sv
	      val state = TS.release_reg state r
	    in state
	    end

	   | Dyntag => 
	    let
	      val () = chat 5 "Allocating dyntag\n"
	      val state = TS.comment state 1 "dyntag done"
	      val c = (case cs 
			 of [c] => c
			  | _ => error "Bad args to Dyntag")

	      val state = externapp_trans state env dest (LD.E.tapp' c (Lil.Label TTD.E.l_newdyntag),[])
	      val () = chat 5 "Dyntag done\n"
	      val state = TS.comment state 1 "Allocating dyntag"
	    in state
	    end
	   | Ptreq => 
	    let
	      val (sv1,sv2) = (case sv32s
				 of [sv1,sv2] => (sv1,sv2)
				  | _ => error "Bad number of args to Ptreq")
	      val (state,goc1) = allocate_rm_for' state env sv1
	      val (state,goc2) = allocate_regimm_for' state env sv2
	      val state = 
		(case dest
		   of CONT (_,x) => 
		     let
		       val (state,dest) = reserve_reg8 state
		       val state = TS.release_reg state dest
		       val state = TS.define_var_from_reg state x dest
		       val state = TS.emit state (Tal.Setcc (Tal.Eq,Tal.Reg dest))
		       val state = TS.init_temp state (Tal.Reg dest) (Tal.Immed 0w0,[])
		     in state 
		     end
		    | RET _ => 
		     let
		       val state = TS.emit state (Tal.Setcc(Tal.Eq,Tal.Reg (Tal.Eax)))
		       val state = TS.init_temp state (Tal.Reg Tal.Eax) (Tal.Immed 0w0,[])
		     in state
		     end
		    | _ => state)

	      val state = TS.emit state (Tal.Cmp (goc1,goc2))
	      val state = sv32_trans_rm' state env goc1 sv1
	      val state = sv32_trans_regimm' state env goc2 sv2
	    in state
	    end
      end

    fun merge_destinations state env mkmerge dest = 
      (case dest 
	 of CONT (F lc,d) => 
	   let
	     val fdest = CONT (F lc,d)
	     val jdest = CONT (L lc,d)
	   in (fdest,jdest,state)
	   end
	  | CONT (L lc,d) => 
	   let
	     val state = jmp_dest state env dest
	     val (state,l,cs) = mkmerge state env
	     val fdest = CONT (F (l,cs),d)
	     val jdest = CONT (L lc,d)
	   in (fdest,jdest,state)
	   end
	  | CONT (S ,d) => 
	   let
	     val (state,l,cs) = mkmerge state env
	     val fdest = CONT (F (l,cs),d)
	     val jdest = CONT (L (l,cs),d)
	   in (fdest,jdest,state)
	   end
	  | EFF (F lc) => 
	   let
	     val fdest = EFF (F lc)
	     val jdest = EFF (L lc)
	   in (fdest,jdest,state)
	   end
	  | EFF (L lc) => 
	   let
	     val state = jmp_dest state env dest 
	     (* We have a designated destination: go there. *)
	     val (state,l,cs) = mkmerge state env
	     val fdest = EFF (F (l,cs))
	     val jdest = EFF (L lc)
	   in (fdest,jdest,state)
	   end
	  | EFF S => 
	   let
	     (* We have a designated destination: go there. *)
	     val (state,l,cs) = mkmerge state env
	     val fdest = EFF (F (l,cs))
	     val jdest = EFF (L (l,cs))
	   in (fdest,jdest,state)
	   end
	  | CC (F _,d) => 
	   let
	     val (state,l,cs) = mkmerge state env
	     val fdest = CC (F (l,cs),d)
	     val jdest = CC (L (l,cs),d)
	   in (fdest,jdest,state)
	   end
	  | CC (d,F _) => 
	   let
	     val (state,l,cs) = mkmerge state env
	     val fdest = CC (d, F (l,cs))
	     val jdest = CC (d, L (l,cs))
	   in (fdest,jdest,state) 
	   end
	  | _ => (dest,dest,state))


    fun raise_trans state env dest (c,sv) = 
      let
	(* First clear the state of any dead code, *)
	val state = TS.reset_state state
	val nm = fresh_tal_lbl "exn_frm"

	(* Jump to handler *)
	val state = TS.emit state (Tal.Jmp (Tal.Reg Tal.Ecx,[]))
	(* Load handler code into Ecx *)
	val state = mov_r_m state Tal.Ecx (tuple_field Tal.Ecx 0w0)
	(* Load stack ptr into Esp *)
	val state = mov_r_m state Tal.Esp (tuple_field Tal.Ecx 0w1)
	(* Load spill record into Ebx *)
	val state = mov_r_m state Tal.Ebx (tuple_field Tal.Ecx 0w2)
	(* Unpack handler frame *)
	val state = TS.emit state (Tal.Unpack(nm,Tal.Ecx, (Tal.Reg Tal.Ebp,[])))
	(* Load exception into Eax *)
	val state = sv32_trans_reg state env Tal.Eax sv
      in state
      end

    fun load_var32_from_tuple state x tuple_reg i = 
      let
	val (state,r) = define_var32_in_reg state x
	val oper = tuple_field tuple_reg i
	val state = mov_r_m state r oper
	val state = TS.release_reg state r
      in state
      end

    fun load_var64_from_tuple state x tuple_reg num32 i = 
      let
	val (state,oper) = TS.define_var64 state x
	val state = store_float_and_pop state oper
	val state = TS.release_temp64 state oper
	val state = load_float state (tuple_field64 tuple_reg num32 i)
      in state
      end


    fun optional_store_float state dop = 
      (case dop
	 of NONE => fpstack_pop state
	  | SOME x => 
	   let
	     val (state,d) = TS.define_var64 state x
	     val state = store_float_and_pop state d
	     val state = TS.release_temp64 state d
	   in state 
	   end)

    fun FP1arg what state env sv dop = 
      let
	val (state,gop) = allocate_for64 state env sv
	val state = optional_store_float state dop
	val state = fp1arg what state
	val state = load_float state gop
	val state = sv64_trans state env gop sv
      in state
      end

    fun FP2arg what state env (sv1,sv2) dop = 
      let
	val (state,gop1) = allocate_for64 state env sv1
	val (state,gop2) = allocate_for64 state env sv2
	val state = optional_store_float state dop
	val state = fp2arg what state gop2
	val state = load_float state gop1
	val state = sv64_trans state env gop1 sv1
	val state = sv64_trans state env gop2 sv2
      in state
      end

    fun prim64_trans state env dest (prim,primargs) = 
      let
	val state = jmp_dest state env dest
	val destop = 
	  (case dest
	     of RET _ => error "Floats don't get returned"
	      | CC args => error "Floats aren't booleans"
	      | EFF gd => NONE
	      | CONT (gd,x) => SOME x)

	val state= 
	  (case (prim,primargs) 
	     of (Prim.int2float,[arg32 sv]) => 
	       let
		 val (state,gop) = TS.reserve_temp_slot state
		 val state = optional_store_float state destop
		 val state = TS.emit state (Tal.FPsomeargs (Tal.Fild,Tal.FPgenop(Tal.B4,gop)))
		 val state = sv32_trans_gop state env gop sv
	       in state
	       end
	      | (Prim.sub t,args) => 
	       let
		 val state = optional_store_float state destop
		 val state = sub t state env NONE args
	       in state
	       end
	      | (Prim.neg_float Prim.F64,[arg64 sv]) => FP1arg Tal.Fchs state env sv destop
	      | (Prim.abs_float Prim.F64,[arg64 sv]) => FP1arg Tal.Fabs state env sv destop
	      | (Prim.plus_float Prim.F64,[arg64 sv1,arg64 sv2]) => 
	       FP2arg Tal.Fadd state env (sv1,sv2) destop
	      | (Prim.minus_float Prim.F64,[arg64 sv1,arg64 sv2]) => 
	       FP2arg Tal.Fsub state env (sv1,sv2) destop
	      | (Prim.mul_float Prim.F64,[arg64 sv1,arg64 sv2]) => 
	       FP2arg Tal.Fmul state env (sv1,sv2) destop
	      | (Prim.div_float Prim.F64,[arg64 sv1,arg64 sv2]) => 
	       FP2arg Tal.Fdiv state env (sv1,sv2) destop
	      | _ => (print "Bad 64 bit prim";
		      PpLil.pp_op64 (Prim64 (prim,primargs));
		      error "Bad prim"))
      in state
      end

    fun op64_trans state env dest oper =
      (case oper
	 of Val_64 sv64 => sv64_return_trans state env dest sv64
	  | Unbox sv32 => 
	   let
	     val state = jmp_dest state env dest
	     val state = 
	       case dest
		 of RET _ => error "Floats don't get returned"
		  | CC args => error "Floats aren't booleans"
		  | EFF gd => state
		  | CONT (gd,xf) => 
		   let
		     val (state,r) = allocate_reg_for state env sv32
		     val dloc = Tal.Prjr((r,[]),0w0,NONE)
		     val (state,oper) = TS.define_var64 state xf
		     val state = store_float_and_pop state oper
		     val state = TS.release_temp64 state oper
		     val state = load_float state dloc
		     val state = sv32_trans_reg state env r sv32
		   in state
		   end
	   in state
	   end
	  | Prim64 args => prim64_trans state env dest args
	  | ExternAppf args => externappf_trans state env dest args)



    fun split (tagcount : w32) (arms : (w32 * label) list) = 
      let
	fun loop (tarms,varms,arms) = 
	  (case arms
	     of [] => (tarms,varms)
	      | (w,l)::arms => 
		 if w < tagcount then loop ((w,l)::tarms,varms,arms)
		 else loop (tarms,(w,l)::varms,arms))
      in loop ([],[],arms)
      end

    fun load_livesets state spill_reg live32set live64set =
      let
	fun loop32 (vars,i,state) = 
	  (case vars
	     of [] => (state,i)
	      | x::vars => 
	       let
		 val state = load_var32_from_tuple state x spill_reg i
	       in loop32 (vars,i + 0w1,state)
	       end)

	(* Load the 32 bit vars *)
	val (state,prefix) = loop32 (live32set,0w1,state)
	     
	fun loop64 (vars,i,state) = 
	  (case vars
	     of [] => state
	      | xf::vars => 
	       let
		 val state = load_var64_from_tuple state xf spill_reg prefix i
	       in loop64 (vars,i + 0w1,state)
	       end)

      (* Load the 64 bit vars *)
	val state = loop64 (live64set,0w0,state)

      in state
      end

    fun handler_trans state env dest {b : var, he : exp} = 
      let
	val state = TS.comment state 1 "End handler"
	val env = TE.bind_var32 (env, ( b,LD.T.exn()))
	val state = etrans state env dest he
	(* Exception arrives in Eax*)
	val state = TS.define_var_from_reg state b Tal.Eax

	(* spill record arrives in Ebx *)
	val spill_reg = Tal.Ebx
	val state = TS.reserve_reg state spill_reg

	val live32set = TS.get_live_vars32 state
	val live64set = TS.get_live_vars64 state

	(* Load the old handler frame from the 0 field of the spill record *)	
	val state = mov_r_m state Tal.Ebp (tuple_field spill_reg 0w0)
	(* Load the live variables from the spill record *)
	val state = load_livesets state spill_reg live32set live64set

	val state = TS.release_reg state spill_reg
	val state = TS.release_reg state Tal.Eax

	(* Allocate the frame *)
	val state = TS.pop_outargs state
	val state = TS.setup_temp64s state
	val framesize = TS.get_frame_size state 
	val state = dec_stack state framesize

	val state = TS.comment state 1 "Begin handler"

	(* Make the type of the spill_record *)
	val ts32 = map (fn x => TE.find_talvar32 LTC.ctrans (env,x)) live32set
	val ts32 = TTD.T.handler_frame (mk_s_con2 env) :: ts32
	val ts64 = map (fn x => TE.find_talvar64 LTC.ctrans (env,x)) live64set

	val tuplec = TTD.T.het_tuple_ptr ts32 ts64
	val handler_code_c = TTD.T.handler_code (Tal.cappend TTD.T.s_con1 TTD.T.s_con2) tuplec
	val closed_handler_c = abstract_tvars env handler_code_c

	val l = fresh_tal_lbl "hndlr_code"
	val state = TS.emit_block state l (SOME closed_handler_c)
	val l_app = instantiate_tvars state env l
      in (state,tuplec,l_app,live32set,live64set)
      end

    and handler_prelude state env handler tuple_con live32set live64set = 
      let

	val nm = Name.fresh_internal_label "htlen"

	val state = TS.comment state 1 "End handler prelude"
	val state = TS.reserve_reg state Tal.Eax

	(* Move the new exn handler into Epb *)
	val state = mov_r_r state Tal.Ebp Tal.Eax

	val () = chat 5 "Allocating handler tuple\n"

	(* Pack up the frame and forget the name *)
	val q = Tal.Pack (tuple_con,TTD.T.handler_frame (Tal.cappend TTD.T.s_con1 TTD.T.s_con2))
	val state = TS.emit state (Tal.Coerce(Tal.Reg Tal.Eax,[q,Tal.Forgetname]))
	val state = TS.emit state (Tal.ForgetUnique nm)

	(* Store the handler code label *)
	val state = mov state (tuple_field Tal.Eax 0w0) handler

	(* Store the handler stack ptr.  Note that Ebp can be safely trashed here *)
	val state = mov_m_r state (tuple_field Tal.Eax 0w1) Tal.Ebp
	val framesize = TS.get_frame_size state 
	val state = if framesize = 0w0 then state
		    else TS.emit state (Tal.ArithBin(Tal.Add,Tal.Reg Tal.Ebp,Tal.Immed framesize))
	val state = mov_r_r state Tal.Ebp Tal.Esp

	(*Spill remaining caller save *before* popping *)
	val state = TS.free_reg state Tal.Ecx
	val state = TS.free_reg state Tal.Edx

	(* Store the spill record *)
	val state = pop state (tuple_field Tal.Eax 0w2) 
	val sz = 0w3 * 0w4
	val state = TS.release_reg state Tal.Eax
	val state = TS.emit state (Tal.Malloc (nm,sz,NONE))
	val state = TS.comment state 2 "Allocating handler frame"
	val () = chat 5 "Allocating handler spill record \n"
	val state = TS.reserve_reg state Tal.Eax
	(* Save the spill record on the top of the stack *)	
	val state = push_r state Tal.Eax

	val nm = Name.fresh_internal_label "srlen"
	val state = TS.emit state (Tal.Coerce(Tal.Reg Tal.Eax,[Tal.Forgetname]))
	val state = TS.emit state (Tal.ForgetUnique nm)

	val () = chat 5 "Initializing handler spill reg elements\n"

	(* Spill the old exn frame *)
	val state = TS.emit state (Tal.Mov (tuple_field Tal.Eax 0w0,(Tal.Reg Tal.Ebp,[])))
	(* Spill the 32bit vars *)
	fun loop32 state i vars32 = 
	  (case vars32
	     of [] => (i,state)
	      | x::vars32 => 
	       let
		 val (state,rim) = allocate_regimm_for' state env (Var_32 x)
		 val md = tuple_field Tal.Eax i
		 val state = TS.emit state (Tal.Mov (md,rim))
		 val state = sv32_trans_regimm' state env rim (Var_32 x)
	       in loop32 state (i+0w1) vars32
	       end)

	val (prefix,state) = loop32 state 0w1 live32set

	(* Spill the 64bit vars *)
	fun loop64 state i vars64 = 
	  (case vars64
	     of [] => (i * 0w8 + prefix * 0w4 + 0w4,state)
	      | xf::vars64 => 
	       let
		 val (state,gop) = allocate_for64 state env (Var_64 xf)
		 val md = tuple_field64 Tal.Eax prefix i
		 val state = store_float_and_pop state md
		 val state = load_float state gop
		 val state = sv64_trans state env gop (Var_64 xf)
	       in loop64 state (i+0w1) vars64
	       end)
	val (sz,state) = loop64 state 0w0 live64set

	val state = TS.comment state 1 "Initializing spill_rec elements"
	  
	val state = TS.release_reg state Tal.Eax
	val state = spill_caller_save state
	val state = TS.emit state (Tal.Malloc (nm,sz,NONE))
	val () = chat 5 "Spill record done\n"
	val state = TS.comment state 2 "Allocating handler spill record"


	val state = TS.comment state 1 "Begin handler prelude"
      in state
      end

    and handle_trans state env dest {t : con, e : exp , h : {b : var, he : exp}} = 
      let
	val (state,dest) = dest2step state env dest
	val state = TS.comment state 1 "End Handler postlude"

	val old_frame_var = Name.fresh_named_var "hndlr_frm"
	val scon = mk_s_con2 env
	val env = TE.bind_talvar (env,(old_frame_var,fn _ => TTD.T.handler_frame scon))

	(* Restore the old handler frame *)
	val (state,frame_tmp) = TS.reserve_temp_slot state
	val state = TS.emit state (Tal.Mov(Tal.Reg Tal.Ebp,(frame_tmp,[])))
	val state = sv32_trans_gop state env frame_tmp (Var_32 old_frame_var)
	val state = TS.comment state 1 "Begin handler postlude"

	fun mkmerge state env = 
	  let
	    val state = TS.pop_outargs state
	    val l = fresh_tal_lbl "hndlmrg"
	    val state = TS.emit_block state l NONE
	  in (state,l,[])
	  end

	val (ft_dest,j_dest,state) = merge_destinations state env mkmerge dest
	val mergestate = state

	(* New copy of state with no blocks*)
	val handlerstate = TS.flush_blocks mergestate
	val (handlerstate,tuplecon,handler,live32set,live64set) = handler_trans handlerstate env j_dest h

	(* Add the handler blocks to the data segment (not interrupting current instruction stream) *)
	val state = TS.add_blocks_before_from_state state handlerstate
	val state = TS.grow_state handlerstate state

	(* Translate the handled code *)
	val state = etrans state (TE.enter_handler env) ft_dest e

	(* Create the handler prelude*)
	val state = handler_prelude state env handler tuplecon live32set live64set

	(* Spill the old handler *)
	val (state,frame_tmp) = TS.define_var32 true state old_frame_var
	val state = mov_m_r state frame_tmp Tal.Ebp
	val state = TS.release_temp state frame_tmp


      in state
      end

    and switch_trans state env dest switch = 
      (case switch 
	 of Intcase {arg : sv32,arms = [] :(w32 * exp) list, size : size,   default: exp,        rtype : con} => error "Empty intcase"
	  | Intcase {arg : sv32,arms = (w,e)::rest :(w32 * exp) list, size : size,   default: exp,        rtype : con} => 
	   let
	     val psize = LU.size2i size
	     val arg1 = arg32 arg
	     val arg2 = arg32  (Const_32 (Prim.int (psize,TilWord64.fromSignedHalf w)))
	     val cmpop = P.ret (Prim32 (Prim.eq_int psize,[],[arg1,arg2]))
	     val argexp = P.Lili.op_to_exp cmpop
	     val argcc = Exp_cc argexp
	     val elseArm = 
	       (case rest
		  of [] => default
		   | _ => 
		    let
		      val swp = P.ret (Switch(Intcase {arg = arg,arms=rest,size=size,default=default,rtype = rtype}))
		    in P.Lili.op_to_exp swp
		    end)
	     val switch = 
	       Ifthenelse {arg = argcc,
			   thenArm = e,
			   elseArm = elseArm,
			   rtype = rtype}
	   in switch_trans' state env dest switch
	   end
	  | _ => switch_trans' state env dest switch)
    and switch_trans' state env dest switch =
      let
	val () = chat 3 "Translating switch"
	val (fallthru_dest,jump_dest,state) = 
	  let
	    fun mkmerge state env = 
	      let
		val () = chat 6 "Making merge destination"
		val state = TS.pop_outargs state
		val l = fresh_tal_lbl "merge"
		val c = internal_label_type state env
		val cs = internal_label_cons state env
		val state = TS.emit_block state l (SOME c)
		val () = chat 6 "Done making merge destination"
	      in (state,l,cs)
	      end
	  in merge_destinations state env mkmerge dest
	  end
		    
      in
	(case switch 
	   of Sumcase {arg : sv32,arms :(w32  * var * exp) list,         default: exp option, rtype : con} =>
	     let
	       val () = chat 6 "Translating Sumcase"
	       val t = TE.typeof_sv32 env arg
	       val (tagcount,carriers) = Dec.C.sum_ml t
	       val nontagcount = List.length carriers

	       fun tag_map (w,_,_) = 
		 let
		   val l = fresh_tal_lbl "arm"
		 in (w,l)
		 end
	       val tags = map tag_map arms

	       val (tarms,varms) = split tagcount tags

	       val hasvarms = not (null varms)
	       val hastarms = not (null tarms)
	       val hastags = tagcount > 0w0
	       val hascarriers = nontagcount > 0
	       val hasdefault = isSome default
	       val singlecarrier = nontagcount = 1
	       val carriertagged = 
		 (case carriers
		    of [c] => not (isSome(Dec.C.ptr' c))
		     | _ => true)

	       val name = Name.fresh_internal_label "sumtg"

	       local
		 val rargo = ref NONE
		 val rtago = ref NONE
		 fun get state ro =
		   (case !ro
		      of NONE => 
			let 
			  val (state,r) = TS.free_temp_reg state 
			  val () = ro := (SOME r)
			in (state,r)
			end
		       | SOME r => (state,r))
		 fun get_rarg state = get state rargo 
		 fun get_rtag state = get state rtago 

		 fun clear_rtag state = 
		   if hasvarms then 
		     let
		       val (state,rtag) = get_rtag state 
		     in TS.free_reg state rtag
		     end
		   else state
	       in

		 fun top_cleanup bound state = 
		   let		
		     val (state,rarg) = get_rarg state
		     val state = TS.define_var_from_reg state bound rarg
		     val state = clear_rtag state
		     val state = TS.emit state (Tal.RemoveName name)
		     val state = TS.emit state (Tal.Coerce (Tal.Reg rarg,[Tal.Forgetname]))
		     val state = TS.kill_var32 state bound
		     val state = TS.release_reg state rarg
		   in state
		   end

		 fun default_cleanup state = 
		   let		
		     val (state,rarg) = get_rarg state
		     val state = TS.reserve_reg state rarg
		     val state = clear_rtag state
		     val state = TS.release_reg state rarg
		   in state
		   end

		 fun get_rarg () = valOf(!rargo)
		 fun get_rtag () = valOf(!rtago)
	       end

	       fun body_map ((w,v,e),(_,l) )= 
		 let 
		   val env = TE.bind_var32 (env,(v,LD.COps.sum2ksum' w t))
		 in (env,top_cleanup v,l,e)
		 end

	       val bodies = ListPair.map body_map (arms,tags)

	       val default_l = fresh_tal_lbl "default"

	       val bodies = 
		 (case default
		    of SOME e => bodies @ [(env,default_cleanup,default_l,e)]
		     | NONE => bodies)

	       val args = 
		 (case bodies
		    of [] => error "no arms"
		     | (env,cleanup,l,e)::rest => (env,cleanup,fallthru_dest,l,e)::(map (fn (env,cleanup,l,e) => (env,cleanup,jump_dest,l,e)) rest))


	       val state = switch_arms state env args

	       val state = fallthru' state []

	       (* do branch comparisons.  If complete, don't do last comparison.  *)
	       fun dobranches (state,rtag,arms,complete) = 
		 let
		   fun cmp_jmp state (w,l) = 
		      let
			val state = jcc_label state env Tal.Eq (l,[])
			val state = TS.emit state (Tal.Cmp((Tal.Reg rtag,[]),(Tal.Immed w,[])))
		      in state
		      end
		   fun loop(state,arms) = 
		     (case arms 
			of [] => state
			 | a::arms => loop(cmp_jmp state a,arms))
		 in
		   (case arms 
		      of [] => state 
		       | a::arms => 
			let
			  val state = if complete then state else cmp_jmp state a
			in loop (state,arms)
			end)
		 end

	       val varms_l = fresh_tal_lbl "carriers"	 
	       val tarms_l = fresh_tal_lbl "tags"	 

	       val varms_dest = if hasvarms then varms_l else default_l

	       (*  cleanup guarantees that this is available.  *)
	       val rarg = get_rarg()
	       val state = TS.reserve_reg state rarg
		 
	       val state =
		 if hasvarms then
		   if carriertagged then 
		     let
		       (*  cleanup guarantees that this is available.  *)
		       val rtag = get_rtag()
		       val state = TS.reserve_reg state rtag
		       val complete = nontagcount = (List.length varms)
		       val state = dobranches (state,rtag,varms,complete andalso (not hasdefault))
		       val state = TS.emit state (Tal.Mov (Tal.Reg rtag,(Tal.Prjr((rarg,[]),0w0,NONE),[])))
		       val state = TS.emit_block state varms_l NONE
		       val state = TS.release_reg state rtag
		     in state
		     end
		   else 
		     let
		       val state = 
			 if hasdefault then 
			   let
			     val (w,l) = hd varms
			     val state = jmp_label state env (l,[])
			   in state
			   end
			 else state
		       val state = TS.emit_block state varms_l NONE
		     in state
		     end
		 else state

	       val state = 
		  let
		    val complete = (LU.w2i tagcount) = (List.length tarms)
		    val state = if complete orelse (not hasvarms) then state
				else jmp_label state env (default_l,[])
		    val state = dobranches (state,rarg,tarms,complete andalso (nontagcount = 0))
		  in state
		  end

	       val state = 
		 if tagcount = 0w0 then state
		 else if nontagcount = 0 then state
		 else 
		   let
		     val state = jcc_label state env Tal.Above (varms_dest,[])
		     val state = TS.emit state (Tal.Cmp((Tal.Reg rarg,[]),(Tal.Immed 0w255,[])))
		   in state
		   end

	       val state = TS.emit state (Tal.Nameobj(name,Tal.Reg rarg));
	       val state = sv32_trans_reg state env rarg arg
	       val state = TS.comment state 1 ("Sumcase has "^(Int.toString (List.length tarms))^" tag arms and "^(Int.toString (List.length varms))^" carrier arms")
	       val state = TS.comment state 1 ("Sumcase for sum with "^(TilWord32.toDecimalString tagcount)^" tags and "^(Int.toString nontagcount)^" carriers")
	       val () = chat 6 "Finished Sumcase"
	     in state
	     end
	    | Dyncase {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        rtype : con} =>
	     let
	       val state = TS.comment state 1 "End Dyncase"
	       val () = chat 4 "Starting Dyncase"
	       fun tag_map (sv,_,_) = 
		 let
		   val l = fresh_tal_lbl "exnarm"
		 in (sv,l)
		 end
	       val tags = map tag_map arms


	       (* Exn packet always arrives in Eax *)
	       fun top_cleanup bound state = 
		 let
		   val state = TS.define_var_from_reg state bound Tal.Eax
		   val state = TS.kill_var32 state bound
		   val state = TS.release_reg state Tal.Eax
		   val state = TS.free_reg state Tal.Ebx
		 in state
		 end

	       fun default_cleanup state = 
		 let		
		   val state = TS.free_reg state Tal.Eax
		   val state = TS.free_reg state Tal.Ebx
		 in state
		 end

	       fun body_map ((sv,(v,c),e),(_,l) )= 
		 let 
		   val env = TE.bind_var32 (env,(v,c))
		 in (env,top_cleanup v,l,e)
		 end

	       val bodies = ListPair.map body_map (arms,tags)

	       val default_l = fresh_tal_lbl "default"

	       val bodies = bodies @ [(env,default_cleanup,default_l,default)]

	       val args = 
		 (case bodies
		    of [] => error "no arms"
		     | (env,cleanup,l,e)::rest => (env,cleanup,fallthru_dest,l,e)::(map (fn (env,cleanup,l,e) => (env,cleanup,jump_dest,l,e)) rest))

	       val state = switch_arms state env args

	       val state = fallthru' state []


	       (* do branch comparisons. *)
	       fun dobranches (state,arms) = 
		 let
		   fun cmp_jmp state (sv,l) = 
		     let
		       val state = jcc_label state env Tal.Eq (l,[])
		       val state = TS.reserve_reg state Tal.Ebx
		       val state = TS.emit state (Tal.Cmp((tuple_field Tal.Eax 0w0,[]),(Tal.Reg Tal.Ebx,[])))		       val state = sv32_place_in_temp state env (Tal.Reg Tal.Ebx) sv
		     in state
		     end
		 in
		   case arms 
		     of [] => state
		      | a::arms => dobranches(cmp_jmp state a,arms)
		 end

	       (*  cleanup guarantees that this is available.  *)
	       val state = TS.reserve_reg state Tal.Eax

	       val state = dobranches (state,tags)

	       (*Uggh.  Use fallthru to make the dyntag arg a refinable variable *)
	       val t  = TE.typeof_sv32 env arg
	       val ts = Dec.C.tuple_ptr_ml t
	       val t = List.nth (ts,1)
	       val unpack_var' = Dec.C.var t
	       val unpack_var = TE.vartrans env unpack_var'
	       val unpack_mem_var' = Name.derived_var unpack_var'
	       val unpack_mem_var = TE.vartrans env unpack_mem_var'
	       val c = TTD.T.exn_body' (Tal.cvar unpack_mem_var)
	       val ms = TS.typeof_state' state env
	       val ms = Tal.set_ms_eax ms (SOME c)
	       val ccode = Tal.ccode_ms ms
	       val lt = abstract_tvars env ccode
	       val lt = TTD.T.forall (unpack_mem_var,Tal.kmem) lt
	       val state = TS.emit_block state (fresh_tal_lbl "exnr") (SOME lt)
	       val cargs = internal_label_cons state env
	       val cargs = cargs@[(Tal.cfield (Tal.cvar unpack_var) Tal.Read)]
	       val state = fallthru' state cargs

	       val state = sv32_trans_reg state env Tal.Eax arg
	       val state = TS.comment state 1 ("Begin Dyncase")
	       val () = chat 4 "Dyncase Done"
	     in state
	     end


	    | Intcase {arg : sv32,arms :(w32 * exp) list, size : size,   default: exp,        rtype : con} =>
	     error "Intcases compiled away above"
	    | Ifthenelse {arg : conditionCode,  thenArm : exp, elseArm : exp, rtype : con} => 
	     let
	       val () = chat 5 "Ifthenelse started"
	       val l_then = fresh_tal_lbl "then"
	       val l_else = fresh_tal_lbl "else"
	       val cleanup = fn s => s
	       val thenArg = (env,cleanup,jump_dest,l_then,thenArm)
	       val elseArg = (env,cleanup,fallthru_dest,l_else,elseArm)

	       val state = switch_arms state env [elseArg,thenArg]
	       val state = cc_trans state env (F (l_then,[]),L (l_else,[])) arg
	       val () = chat 5 "Ifthenelse finished"
	     in state
	     end)
      end
    and switch_arms bottomstate env args = 
      let
	val () = chat 5 "Starting switch arms"
	fun loop (topstate,args) = 
	  (case args
	     of [] => topstate
	      | ((env,cleanup,dest,l,exp)::args) => 
	       let
		 val () = chat 7 "next switch arm"
		 val state = TS.revert_state bottomstate topstate
		 val state = etrans state env dest exp
		 val state = TS.pop_outargs state
		 val state = TS.match_state topstate state
		 val state = cleanup state
		 val state = TS.comment_stack state 4
		 val state = TS.emit_block state l NONE
	       in loop (state,args)
	       end)
	val state = TS.copy_state bottomstate
	val state =
	  (case args
	     of [] => state
	      | (env,cleanup,dest,l,exp)::args => 
	       let
		 val () = chat 7 "first switch arm"
		 val state = etrans state env dest exp
		 val state = TS.pop_outargs state
		 val state = cleanup state
		 val state = TS.comment_stack state 4
		 val topstate = TS.emit_block state l NONE
	       in loop (topstate,args)
	       end)
	val () = chat 5 "Finished switch arms"
      in state
      end
    and cc_trans state env (nonzero,zero) cc = 
      let
	val () = chat 5 "Starting cc trans"
	fun mk_jmp cont = 
	  (case cont 
	     of F arg => L arg
	      | L _ => cont
	      | _ => error "Shouldn't see steps here")

	val state = 
	  (case cc
	     of Exp_cc e => 
	       let
		 val state = etrans state env (CC (nonzero,zero)) e
	       in state
	       end
	      | And_cc (cc1,cc2) => 
	       let
		 val bottomstate = state
		 val state = cc_trans state env (nonzero,zero) cc2
		 val state = TS.pop_outargs state
		 val state = TS.match_state bottomstate state
		 val l_and = fresh_tal_lbl "andalso"
		 val state = TS.emit_block state l_and NONE
		 val state = cc_trans state env (F (l_and,[]),mk_jmp zero) cc1
		 val state = TS.pop_outargs state
		 val state = TS.match_state bottomstate state
	       in state
	       end
	      | Or_cc (cc1,cc2) =>
	       let
		 val bottomstate = state
		 val state = cc_trans state env (nonzero,zero) cc2
		 val state = TS.pop_outargs state
		 val state = TS.match_state bottomstate state
		 val l_or = fresh_tal_lbl "orelse"
		 val state = TS.emit_block state l_or NONE
		 val state = cc_trans state env (mk_jmp nonzero,F (l_or,[])) cc1
		 val state = TS.pop_outargs state
		 val state = TS.match_state bottomstate state
	       in state
	       end
	      | Not_cc cc => cc_trans state env (zero,nonzero) cc)
	val () = chat 5 "Finished cc trans"
      in state
      end


    and op32_trans state env dest oper = 
      (case oper
	 of Val sv32 => sv32_return_trans state env dest sv32
	  | Prim32 args => prim32_trans state env dest args
	  | PrimEmbed (sz,p,args) => prim32_trans state env dest (p,[],args)
	  | LilPrimOp32 args => lilprim_trans state env dest args
	  | ExternApp args => externapp_trans state env dest args
	  | Call args => call_trans state env dest args
	  | Switch switch => switch_trans state env dest switch
	  | Raise args => raise_trans state env dest args
	  | Handle args => handle_trans state env dest args
	  | App (f,eargs,fargs) => error "No Apps allowed!")



    and bnd_trans state env bnd = 
      let
	val () = debugdo (2,fn () => (print "Translating bnd:\n";
				      PpLil.pp_bnd bnd;
				      print "\n"))
	val state = 
	  (case bnd
	     of Exp32_b (x,oper) => 
	       let
		 val state = TS.comment_stack state 7 
		 val (env,dest) = 
		   if TS.var32_used state x
		     then (TE.bind_bnd env bnd,CONT(S,x))
		   else (env,EFF S)

		 val () = debugdo (3,fn () => (print "Exp32_b: calling op32_trans\n"))
		 val state = op32_trans state env dest oper
		 val state = TS.kill_var32 state x
		 val () = debugdo (3,fn () => (print "Exp32_b: finished\n"))
		 val state = TS.comment state 7 ("Defining var32: " ^ (Name.var2string x))
	       in state
	       end
	   | Exp64_b (xf,oper) =>
	       let
		 val (env,dest) = 
		   if TS.var64_used state xf
		     then (TE.bind_bnd env bnd,CONT(S,xf))
		   else (env,EFF S)

		 val state = op64_trans state env dest oper
		 val state = TS.kill_var64 state xf
	       in state
	       end
	   | Unpack_b (a,x,sv32) => 
	       let
		 val (state,gop) = TS.define_var32 true state x
		 val state = sv32_unpack_rm state env gop (TE.vartrans env a) sv32
		 val state = TS.kill_var32 state x
	       in state
	       end
	   | Split_b (a1,a2,c) => TS.emit state (Tal.Letprod ([TE.vartrans env a1,TE.vartrans env a2],LTC.ctrans env c))
	   | Unfold_b (a,c) => TS.emit state (Tal.Letroll (TE.vartrans env a,LTC.ctrans env c))
	   | Inj_b (w,a,c,sv) => 
	       let
		 val (state,goc) = allocate_goc_for state env sv
		 val c = LTC.ctrans env c
		 val state = TS.emit state (Tal.Vcase (w,c,TE.vartrans env a,goc))
		 val state= sv32_trans_goc state env goc sv
	       in state
	       end
	   | Fixcode_b _ => error "No functions allowed!!")
      in state
      end

    and etrans (state : TS.state) (env : TE.env) (dest : dest) (exp : Lil.exp) : TS.state = 
      let 
	val () = debugdo(3,fn () => TS.sanity_check state)
	val state = 
	  (case eout exp 
	     of Val32_e sv32 => sv32_return_trans state env dest sv32
	     | Let_e (bnds,e) =>
	       let
		 fun workloop outerenv bnds = 
		   (case bnds
		      of [] => etrans state outerenv dest e
		       | [bnd] => 
			(case terminal_op state outerenv (bnd,e)
			   of SOME (x,oper) =>
			     op32_trans state outerenv dest oper
			    | NONE => 
			     let
			       val env = TE.bind_bnd outerenv bnd
			       val state = etrans state env dest e
			       val () = debugdo(1,fn () => TS.sanity_check state)
			       val state = bnd_trans state outerenv bnd
			     in state
			     end)
		       | bnd::bnds => 
			   let
			     val env = TE.bind_bnd outerenv bnd
			     val state = workloop env bnds
			     val () = debugdo(1,fn () => TS.sanity_check state)
			     val state = bnd_trans state outerenv bnd
			   in state
			   end)
	       in workloop env bnds
	       end)
	val () = debugdo(3,fn () => TS.sanity_check state)
      in state
      end

    val etrans = fn (state : TS.state) => fn (env : TE.env) => fn (dest : dest) => fn (exp : Lil.exp) => etrans state env dest (CFlatten.rewrite_exp exp)

    fun value_data_item_trans env v : Tal.data_item = 
      (case v
	 of Prim.int(Prim.W64,_) => error "64 bit ints not done"
	  | Prim.uint(Prim.W64,_) => error "64 bit ints not done"
	  | Prim.int(ws,w64) => Tal.D4bytes (TilWord64.toUnsignedHalf w64,[])
	  | Prim.uint(ws,w64) => Tal.D4bytes (TilWord64.toUnsignedHalf w64,[])
	  | (Prim.float (Prim.F64, s)) => error "data:No floats yet"
	  | (Prim.float (Prim.F32, _)) => error "32 bit floats not done"
	  | _ => error "Unexpected constant")

    fun sv32_data_item_trans env sv32 : Tal.data_item = 
      (case sv32
	 of Var_32 v => error "No vars in data_items"
	  | Label l  => Tal.Dlabel ((TTD.E.mk_tal_lbl l),[])
	  | Const_32 v => value_data_item_trans env v
	  | Tag w => Tal.D4bytes (w,[])
	  | Unit => Tal.Dlabel(TTD.E.l_unit,[])
	  | Coercion q => error "Statically allocated coercion fields not yet handled"
	  | Tabs _ => error "Statically allocated coercion fields not yet handled"
	  | TApp _ => error "Statically allocated TApps not yet handled"
	  | Coerce _ => error "Statically allocated coercions not yet handled")


    fun place_formals state env (eFormals,fFormals) = 
      let
	val state = TS.adjust_inargs state (List.length eFormals + 1) (List.length fFormals)
	fun floop [] = (0,state)
	  | floop ((xf,c)::fFormals) = 
	  let
	    val (i,state) = floop fFormals
	    val state     = TS.bind_formal64 state xf (TS.LilCon c) i
	  in (i + 1,state)
	  end
	val (estart,state) = floop fFormals
	fun loop [] = (0,state)
	  | loop ((x,c)::eFormals) = 
	  let
	    val (i,state)  = loop eFormals
	    val state     = TS.bind_formal32 state x (TS.LilCon c) i
	  in (i + 1, state)
	  end
	val (i,state) = loop eFormals
	val state = bind_ret_addr state env i
      in state
      end

    fun body_trans env eFormals fFormals body = 
      let

	val num_eargs = List.length eFormals
	val num_fargs = List.length fFormals
	val bytes_below_ret = LU.i2w(num_eargs*4 + num_fargs*8)
	(* Once with a frame pointer *)
	val state = TS.new_state ()
	val state = TS.alloc_inargs state num_eargs num_fargs
	val state = place_exit_vars state env num_eargs
	val () = chat 3 "Translating body with virtual fp\n"
	val state = etrans state env (RET bytes_below_ret) body
	val () = chat 3 "Placing code formals\n"
	val state = place_formals state env (eFormals,fFormals)
	val state = spill_callee_save state
	val state = TS.setup_temp64s state
	val state = TS.pop_outargs state

	val () = chat 3 "Re-Translating body\n"
	(* And now without a frame pointer *)
	val inargsz = TS.get_inarg32_size state
	val inarg64sz = TS.get_inarg64_size state
	val tempsz = TS.get_temp_size state 
	val temp64sz = TS.get_temp64_size state 
	val outargsz = 0w0
	val state = TS.new_state' (inarg64sz,inargsz,tempsz,temp64sz,outargsz) 
	val state = place_exit_vars state env num_eargs
	val () = debugdo (3,fn () => (print "Initial frame sizes:\n";
				      print "\t";print "inargsz = ";print (TilWord32.toDecimalString inargsz);print "\n";
				      print "\t";print "inarg64sz = ";print (TilWord32.toDecimalString inarg64sz);print "\n";
				      print "\t";print "tempsz = ";print (TilWord32.toDecimalString tempsz);print "\n";
				      print "\t";print "temp64sz = ";print (TilWord32.toDecimalString temp64sz);print "\n"))
	val state = etrans state env (RET bytes_below_ret) body
	val () = chat 3 "Placing code formals\n"
	val state = place_formals state env (eFormals,fFormals)
	val state = spill_callee_save state
	val state = TS.setup_temp64s state
	val state = TS.pop_outargs state
      in state
      end 

    fun codetrans env (l,f as Function {tFormals    : (var * kind) list,
					eFormals    : (var * con) list,
					fFormals    : (var * con) list,
					rtype       : con,
					body        : exp}) : Tal.code_block list = 
      let

	val () = chat 1 ("Translating code function: "^(Name.label2string l)^"\n")

	val () = chat 3 ("Translating code type : \n")
	val ltype = LTC.ctrans env (TE.typeof_code env f)

	val env = TE.bind_cvars (env,tFormals)
	val env = TE.add_tvars env (map #1 tFormals)
	val env = TE.bind_vars32 (env,eFormals)
	val env = TE.bind_vars64 (env,fFormals)
	val () = chat 3 ("Translating code return type : \n")

	val env = TE.set_ms_return env rtype

	val env = bind_tal_vars env


	val () = chat 3 ("Translating code body: \n")
	val state = body_trans env eFormals fFormals body 

	val state = TS.comment_stack state 1

	val framesize = TS.get_frame_size state
	val inarg_sz =  LU.i2w(4 * List.length eFormals + 8 * List.length fFormals + 4)
	val state = dec_stack state (framesize - inarg_sz)
	val state = TS.emit_block state (TTD.E.mk_tal_lbl l) (SOME ltype)
	val blocks = TS.get_blocks state

	val () = chat 3 "Finished with code block\n"
      in blocks
      end

    fun tuple_trans env (l,t,qs,svs) : Tal.data_block list = 
      let
	val t = LTC.ctrans env t
	val qs = List.concat (map (coercion env) qs)
	val ditems = map (sv32_data_item_trans env) svs
      in [(TTD.E.mk_tal_lbl l,0w4,SOME t,(ditems,qs))]
      end

    fun array_trans env (l,sz,t,svs) : Tal.data_block list = 
      let
	val l_string = derived_tal_lbl l
	val l_ref = TTD.E.mk_tal_lbl l
	fun sv_to_char sv = 
	  (case sv
	     of Prim.uint(W8,c) => chr(TilWord64.toInt c)
	      | _ => error "bad vector value: corrupt string")
	val s = String.implode (map sv_to_char svs)
	val d = Tal.Dbytes s
	val i = (LU.i2w(List.length svs))
	val len = Tal.pcint i
	val t = LTC.ctrans env t
	val arrayq = Tal.Toarray (0w0,0,Tal.cfield t Tal.ReadWrite)
	val string_block = (l_string,0w4,SOME (TTD.T.array_elts sz len t),([d],[arrayq]))
	val string_items = [Tal.D4bytes (i,[]),Tal.Dlabel (l_string,[])]
	val arrt = TTD.T.array sz t
	val packq = Tal.Pack (len,arrt)
	val string_ref_block = (l_ref,0w4,SOME arrt,(string_items,[packq]))
      in [string_block,string_ref_block]
      end
    fun boxed_trans env (l,sv64) : Tal.data_block list = 
      let
	val l = TTD.E.mk_tal_lbl l
	val s = 
	  (case sv64
	     of Const_64(Prim.float(F64,f)) => f
	      | _ => error "bad float value")
	val d = Tal.Dfloat64 s
	val c = LTC.ctrans env (LD.T.ptr (LD.T.boxed_float()))
	val float_block = (l,0w8,SOME c,([d],[]))
      in [float_block]
      end

    fun data_block env d : Tal.data_block list option = 
      (case d
	 of Dboxed args => SOME(boxed_trans env args)
	  | Dtuple args => SOME(tuple_trans env args)
	  | Darray args => SOME(array_trans env args)
	  | Dcode (l,f) => NONE)

    fun code_block env d : Tal.code_block list option = 
      (case d
	 of Dcode (l,f) => SOME (codetrans env (l,f))
	  | _ => NONE)

    fun data_blocks env data : Tal.data_block vector = Vector.fromList (List.concat (List.mapPartial (data_block env) data))
    fun code_blocks env data : Tal.code_block vector = Vector.fromList (List.concat (List.mapPartial (code_block env) data))
    
    fun add_data env data = 
      let
	fun add_dtype (d,env) = 
	  (case d
	     of Dboxed (l,sv64) => TE.bind_label(env,(l,LD.T.ptr (LD.T.boxed_float())))
	      | Dtuple (l,t,qs,svs) => TE.bind_label(env,(l,t))
	      | Darray (l,sz,c,svs) => TE.bind_label (env,(l,LD.T.ptr (LD.T.array sz c)))
	      | Dcode (l,f) => TE.bind_label(env,(l,TE.typeof_code env f)))
	val env = foldl add_dtype env data
      in env
      end

    fun data_trans env data = 
      let
	val () = chat 2 "Adding data types\n"
	val env = add_data env data
	val () = chat 2 "Translating data blocks\n"
	val data_blocks = data_blocks env data
	val () = chat 2 "Translating code blocks\n"
	val code_blocks = code_blocks env data
      in (env,code_blocks,data_blocks)
      end

    fun get_entry_type env data l =
      (case LU.get_data_entry data l
	 of Dcode (l,f) => TE.typeof_code env f
	  | _ => error "Entry label not in data segment")


    fun timports_trans env timports : TE.env  =
      let
	fun loop (env,timports) = 
	  (case timports 
	     of [] => env
	      | (l,a,lk)::timports => 
	       let
		 val env = TE.bind_cvar (env,(a,lk))
		 val l = TTD.E.mk_tal_lbl l
		 val env = TE.label_var env a l
		 val () = TA.register_global a
	       in loop(env,timports)
	       end)
      in loop(env,timports)
      end

    (* <DirtyUglyHack> *)
    fun link_timports_trans env timports : TE.env  =
      let
	fun loop (env,timports) = 
	  (case timports 
	     of [] => env
	      | (l,a,lk)::timports => 
	       let
		 val env = TE.bind_cvar (env,(a,lk))
		 val l = TTD.E.mk_tal_lbl l
		 val al = TE.vartrans env a
		 val () = TA.C.abbreviate al (Tal.cvar l)
		 val () = TA.register_global a
	       in loop(env,timports)
	       end)
      in loop(env,timports)
      end
    (* </DirtyUglyHack> *)

    fun vimports_trans env vimports = 
      let
	fun loop (env,vimports) = 
	  (case vimports 
	     of [] => env
	      | (l,lc)::vimports => 
	       let
		 val env = TE.bind_label (env,(l,lc))
	       in loop(env,vimports)
	       end)

      in loop(env,vimports)
      end

    fun add_stdlib_global_types env = 
      let
	val env = TE.bind_label (env,(TTD.E.l_newdyntag,TTD.T.new_dyntag_type))
	val env = TE.bind_label (env,(TTD.E.l_ptrarray,TTD.T.new_ptrarray_type))
	val env = TE.bind_label (env,(TTD.E.l_intarray Lil.B1,TTD.T.new_intarray_type Lil.B1))
	val env = TE.bind_label (env,(TTD.E.l_intarray Lil.B4,TTD.T.new_intarray_type Lil.B4))
	val env = TE.bind_label (env,(TTD.E.l_floatarray,TTD.T.new_floatarray_type))
      in env
      end
    fun mk_imp link (entry_c,timports,vimports,data,confun) : Tal.tal_imp = 
      let
	val () = TA.reset ()

	val _ = chat 1 "Translating timports\n"

	val env = if link then link_timports_trans (TE.empty()) timports
		  else timports_trans (TE.empty()) timports
	val env = vimports_trans env vimports
	val env = add_stdlib_global_types env 

	val _ = chat 1 "Translating confun\n"
	val c = LTC.ctrans env confun

	val (entry_c_l,entry_c_a,entry_c_k) = entry_c
	val k = LTC.ktrans entry_c_k

	val con_block = (TTD.E.mk_tal_lbl entry_c_l,k,c)
	val con_blocks = Vector.fromList [con_block]

	val _ = chat 1 "Translating Data\n"
	val (env,code_blocks,data_blocks) = data_trans env data
	val imp_abbrevs = Vector.fromList (TalAbbrev.C.abbrevs ())
	val imp_kindabbrevs = Vector.fromList (TalAbbrev.K.abbrevs ())

	val tal_imp : Tal.tal_imp = 
	  { 
	   imp_abbrevs = imp_abbrevs,
	   imp_kindabbrevs = imp_kindabbrevs,
	   con_blocks = con_blocks,
	   code_blocks = code_blocks,
	   data_blocks = data_blocks
	   } 
	val () = TA.reset ()
      in tal_imp
      end

    fun mk_i_int (timports,vimports) = 
      let
	val () = TA.reset ()

	val _ = chat 1 "Creating extern import interface for module\n"

	val env = timports_trans (TE.empty()) timports
	fun loop (vimports,acc) = 
	  (case vimports 
	     of [] => acc
	      | (l,lc)::vimports => 
	       let
		 val l = unk_lbl_trans l lc
		 val c = LTC.ctrans env lc
	       in loop (vimports,(l,c)::acc)
	       end)
	val vimports = loop (vimports,[])
	val int_abbrevs : Tal.con_abbrev vector = Vector.fromList (TalAbbrev.C.abbrevs ())
	val int_kindabbrevs : Tal.kind_abbrev vector = Vector.fromList (TalAbbrev.K.abbrevs ())

	val tal_i_int : Tal.tal_int = 
	  { 
	   int_abbrevs = int_abbrevs,
	   int_kindabbrevs = int_kindabbrevs,
	   int_cons = Vector.fromList [],
	   int_vals = Vector.fromList vimports
	   } 
	val () = TA.reset ()
      in tal_i_int
      end

    fun mk_e_int (unitname,entry_c,entry_r,timports) = 
      let
	val () = TA.reset ()

	val _ = chat 1 "Creating link interface for module\n"

	val env = timports_trans (TE.empty()) timports
	val (entry_c_l,entry_c_a,entry_c_k) = entry_c
	val env = TE.bind_cvar (env,(entry_c_a,entry_c_k))
	val entry_c_l = TTD.E.mk_tal_lbl entry_c_l
	val env = TE.label_var env entry_c_a entry_c_l
	val () = TA.register_global entry_c_a

	val (entry_r_l,entry_r_t) = entry_r

	val (_,_,return_type) = Dec.C.code entry_r_t
	val () = 
	  (case Dec.C.tuple_ptr_ml' return_type
	     of SOME [rep_type,term_type] => 
	       let
		 val rep_type = LTC.ctrans env rep_type
		 val term_type = LTC.ctrans env term_type
		 val rtlbl = TTD.E.mk_tal_lbl (LLU.unit_rtype_label unitname)
		 val tlbl  =  TTD.E.mk_tal_lbl (LLU.unit_type_label unitname)
		 val () = TalAbbrev.C.abbreviate rtlbl rep_type
		 val () = TalAbbrev.C.abbreviate tlbl term_type
	       in ()
	       end
	      | _ => ())
	val entry_r_t = LTC.ctrans env entry_r_t

	val int_abbrevs : Tal.con_abbrev vector = Vector.fromList (TalAbbrev.C.abbrevs ())
	val int_kindabbrevs : Tal.kind_abbrev vector = Vector.fromList (TalAbbrev.K.abbrevs ())

	val tal_e_int : Tal.tal_int = 
	  { 
	   int_abbrevs = int_abbrevs,
	   int_kindabbrevs = int_kindabbrevs,
	   int_cons = Vector.fromList [],
	   int_vals = Vector.fromList [(TTD.E.mk_tal_lbl entry_r_l,entry_r_t)]
	   } 
	val () = TA.reset ()
      in tal_e_int
      end

    fun modtrans link (MODULE {unitname : string,
			       parms : Name.LabelSet.set,
			       entry_r : label * con,
			       entry_c : label * var * kind,
			       timports : timport list,
			       vimports : vimport list,
			       data   : data list,
			       confun : con}) : (string * Tal.tal_imp * Tal.tal_int * Tal.tal_int) = 
      let
	val _ = chat 1 "Beginning translating module to TAL\n"
	val tal_imp = mk_imp link (entry_c,timports,vimports,data,confun)
	val tal_i_int = mk_i_int (timports,vimports)
	val tal_e_int = mk_e_int (unitname,entry_c,entry_r,timports)
	val _ = chat 1 "Finished translating module to TAL\n"

      in (unitname,tal_imp,tal_i_int,tal_e_int)
      end

    fun inttrans (INTERFACE {unitname : string,
			     timports : timport list,
			     entry_c : label * var * kind,
			     entry_r : label * con}) : string * Tal.tal_int = 
      let

	val () = TA.reset ()

	fun folder ((l,a,k),env) =
	  let
	    val env = TE.bind_cvar (env,(a,k))
	    val l = TTD.E.mk_tal_lbl l
	    val env = TE.label_var env a l
	    val () = TA.register_global a
	  in env
	  end

	val env = foldl folder (TE.empty()) timports

	val _ = chat 1 "Creating con exports\n"
	val env = foldl folder env [entry_c]
	val (l_c,a,lk) = entry_c 
	val k = LTC.ktrans lk

(*	val _ = chat 1 "Creating term exports\n"
	val (l_r,lc) = entry_r

	val c = LTC.ctrans env lc
*)	val int_vals = Vector.fromList [(*(TTD.E.mk_tal_lbl l_r,c)*)]

	val con_entry = (TTD.E.mk_tal_lbl l_c,k,Tal.AbsCon)
	val int_cons = Vector.fromList [con_entry]

	val int_abbrevs = Vector.fromList (TalAbbrev.C.abbrevs ())
	val int_kindabbrevs = Vector.fromList (TalAbbrev.K.abbrevs ())
	val tal_int : Tal.tal_int =
	  { 
	   int_abbrevs = int_abbrevs,
	   int_kindabbrevs = int_kindabbrevs,
	   int_cons = int_cons,
	   int_vals = int_vals
	   }

	val _ = chat 1 "Finished translating interface to TAL\n"

	val () = TA.reset ()

      in (unitname,tal_int)
      end

end  (* LilToTalExp *)