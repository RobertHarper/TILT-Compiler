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

    structure LO = Listops

    open Lil

    val error = fn s => Util.error "liltotalexp.sml" s
    val debug = Stats.ff "LilToTalExpDebug"

    val debuglev = ref 0
    val chatlev = ref 0 

    fun indent i s = 
      let
	fun loop 0 cs = String.implode cs
	  | loop i cs = loop (i - 1) (#" "::cs)
      in loop (i+1) (String.explode s)
      end

    fun chatp i = !(chatlev) >= i
    fun chat i s = if chatp i then print (indent i s) else ()
    fun debugdo (i,t) = if (!debug) andalso (i <= !debuglev) then (t(); ()) else ()

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k
    fun eout (e : Lil.exp) : Lil.exp_ = #e e


    fun fresh_tal_lbl s = (TTD.E.mk_tal_lbl (Name.fresh_internal_label s))

    datatype genaddr = L of (Tal.label * bool) | F of (Tal.label * bool) | S (* label, fallthru, or step *)

    datatype dest = 
      RET                     (* Function return context *)
      | CC of genaddr * genaddr   
                              (* Condition code context: (lnonzero, lzero) *)
                              (* Invariant: at most one fallthru *)
      | EFF of genaddr        (* Effect context: value unused. *)
      | CONT of genaddr * Tal.genop  
                              (* Place value in genop and continue to label *)



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
    in 

      (* instantiate a function with the input frame args.
       *)
      fun tailcall_instantiate (gop,qs) = (gop,s_app2::s_app1::cs_app3::cs_app2::cs_app1::qs)


      (* Call before the jump *)
      fun restore_callee_save state = 
	let
	  (* Release the callee save registers locked by spill_callee_save
	   *)
	  val state = TS.init_var32 state (Tal.Reg Tal.Ebx) cs_var1 (TTD.T.cs_con1)
	  val state = TS.init_var32 state (Tal.Reg Tal.Esi) cs_var2 (TTD.T.cs_con2)
	  val state = TS.init_var32 state (Tal.Reg Tal.Edi) cs_var3 (TTD.T.cs_con3)

	  val state = TS.init_var32 state (Tal.Reg Tal.Ebp) exn_ptr (TTD.T.handler_stackptr (TTD.T.s_con2))
	in state
	end
	
      (* Call after the jump *)
      fun spill_callee_save state = 
	let
	  (* Associate the callee save variables with the callee saves
	   * registers, spilling if necessary.
	   *)
	  val state = TS.load_var state (Tal.Ebx) cs_var1 (TTD.T.cs_con1)
	  val state = TS.load_var state (Tal.Esi) cs_var2 (TTD.T.cs_con2)
	  val state = TS.load_var state (Tal.Edi) cs_var3 (TTD.T.cs_con3)
	  val state = TS.load_var state (Tal.Ebp) exn_ptr (TTD.T.handler_stackptr (TTD.T.s_con2))
	in state
	end

      (* Emit code to load the caller save registers.
       *)
      fun spill_caller_save state = 
	let
	  val state = TS.free_reg state Tal.Eax
	  val state = TS.free_reg state Tal.Ecx
	  val state = TS.free_reg state Tal.Edx
	in state
	end

      fun place_ret_addr state env d = 
	(case TE.get_ms_return env
	   of SOME rtype => TS.init_var32 state d ret_addr rtype
	    | _ => error "No return type")

      fun bind_ret_addr state env = 
	(case TE.get_ms_return env
	   of SOME rtype => TS.bind_next_formal32 state ret_addr rtype
	    | _ => error "No return type")

    end

    fun tuple_field reg offset = Tal.Prjr ((reg,[]),offset * 0w4,NONE)

    fun popn state i = 
      if i = 0w0 then state
      else TS.emit state (Tal.ArithBin(Tal.Add,Tal.Reg Tal.Esp,Tal.Immed (i * 0w4)))

    fun pop_frame state = 
      let
	val state = restore_callee_save state
	val state = popn state (TS.get_frame_size state)
      in state
      end

    fun jmp_label' state l qs =
      TS.emit state (Tal.Jmp (Tal.Addr l,qs))

    fun jcc_label' state cond l qs = 
      TS.emit state (Tal.Jcc (cond,(l,qs),NONE))

    fun fallthru' state cs = 
      TS.emit state (Tal.Fallthru cs)



    fun fallthru_cons state env = 
      let
	val tvars = TE.get_tvars env
	val cons = map Tal.cvar tvars
      in cons @[TTD.T.cs_con1,TTD.T.cs_con2,TTD.T.cs_con3,TTD.T.s_con1,TTD.T.s_con2]
      end

    fun jmp_coercions state env = map (fn c => Tal.Tapp (Tal.Con c)) (fallthru_cons state env)

    fun jmp_label state env l coerce = jmp_label' state l (if coerce then jmp_coercions state env else [])
    fun jcc_label state env cond l coerce = jcc_label' state cond l (if coerce then jmp_coercions state env else [])
    fun fallthru state env coerce = fallthru' state (if coerce then fallthru_cons state env else [])

    fun jmp_genaddr state env gd = 
      (case gd
	 of F (l,coerce)  => fallthru state env coerce
	  | L (l,coerce) => jmp_label state env l coerce
	  | S   => state)

    (* No handlers yet*)
    fun call_coercions state env = 
      let
	val s1 = Tal.StackSlice (Tal.Esp,0,LU.w2i(TS.get_frame_size state + TS.get_inarg_size state),TTD.T.s_con1)
	val s2 = Tal.Con (TTD.T.s_con2)
	val annotes = 
	  [Tal.Con(TTD.T.cs_con1)
	  ,Tal.Con(TTD.T.cs_con2)
	  ,Tal.Con(TTD.T.cs_con3)
	  ,s1
	  ,s2]
      in map Tal.Tapp annotes
      end

    fun call_instantiate state env (gop,qs) = (gop,(call_coercions state env)@qs)

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
	      | (ForgetKnown,[from]) => [Tal.Tosum (docon (LD.COps.ksum2sum from))]
	      | (ProjKnown,[from]) => []
	      | (InjUnion, [to])   => []
	      | (InjForget, [ksum]) => [Tal.Tosum (docon(LD.COps.ksum2sum ksum))]
	      | _ => error "Wrong coercion args")
      in res
      end


    (* Translate a first class coercion to a code block
     * which coerces EAX appropriately and then returns. *)
    fun coercion_fun outerstate env (vks,q) =
      let
	val state = TS.new_state ()
	val env = TE.bind_cvars (env,vks)
	val qs = coercion env q
	val state = TS.emit state (Tal.Retn NONE)
	val state = TS.emit state (Tal.Coerce (Tal.Reg Tal.Eax,qs))


	val l = Name.fresh_internal_label "coercion_fn"
	val c = LTC.ctrans env (TE.typeof_sv32 env (LD.E.nary_tabs' vks (Coercion q)))
	val state = TS.emit_block state l (SOME c)
	val state = TS.add_blocks_from_state outerstate state
      in (state,l)
      end


    fun allocate_for state env (flags as (memok,immok,coerceok)) sv32 : TS.state * Tal.genop Tal.coerce = 
      (case sv32
	 of Var_32 v => 
	   let
	     val lc = TE.find_var32 (env,v)
	     val c = LTC.ctrans env lc
	     val (state,gop) =  TS.reserve_for_var memok state v c
	   in (state,(gop,[]))
	   end
	  | Label l  => 
	   let
	     val (state,gop) = 
	       if immok then 
		 (state,Tal.Addr (TTD.E.mk_tal_lbl l))
	       else 
		 TS.reserve_temp memok state 
	   in (state,(gop,[]))
	   end
	  | Const_32 v => 
	   let
	     val (state,gop) = 
	       if immok then 
		 (state,Tal.Immed (LU.value2w32 v))
	       else 
		 TS.reserve_temp memok state
	   in (state,(gop,[]))
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
	     val (state,gop) = 
	       if immok then 
		 let
		   val (state,l) = coercion_fun state env ([],q)
		 in (state,Tal.Addr l) 
		 end
	       else
		 TS.reserve_temp memok state
	   in (state,(gop,[]))
	   end
	  | (sv as Tabs _) => 
	   let
	     val (vks,sv) = Dec.E.nary_tabs sv
	     val q = 
	       (case Dec.Q.coercion' sv
		  of SOME q => q
		   | NONE => error "Tabs only used for coercions, I thought")

	     val (state,gop) = 
	       if immok then 
		 let
		   val (state,l) = coercion_fun state env (vks,q)
		 in (state,Tal.Addr l) 
		 end
	       else
		 TS.reserve_temp memok state
	   in (state,(gop,[]))
	   end
	  | TApp (sv,c) => 
	   let
	     val (state,(oper,qs)) = allocate_for state env flags sv
	   in
	     if coerceok then 
	       let
		 val c = LTC.ctrans env c
	       in (state,(oper,(Tal.Tapp (Tal.Con c))::qs))
	       end
	     else (state,(oper,qs))
	   end

	  | Coerce (sv1,sv2) => 
	   let
	     val res = 
	       (case sv1
		  of Coercion q => 
		    let
		      val (state, (r,qs)) = allocate_for state env flags sv2
		    in 
		      if coerceok then 
			let
			  val qs' = coercion env q
			  val qs = qs' @ qs
			  val qs = 
			    (case qs
			       of (Tal.Roll c)::(Tal.Tosum _)::qs => (Tal.RollTosum c)::qs
				| _ => qs)
			in (state,(r,qs))
			end
		      else
			(state, (r,qs))
		    end
		   | _ =>  
		    let
		      val (state,gop) = TS.reserve_temp memok state 
		    in (state,(gop,[]))
		    end)
	   in res
	   end)

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
	  | _ => error "Got coerciond for regimm")

    fun allocate_rm_for state env sv32 =
      (case allocate_for state env (true,false,false) sv32
	 of (state,(gop,[])) => (state,gop)
	  | _ => error "Got coercion for rm")

    (* Any genop *)
    fun allocate_gop_for state env sv32 = 
      (case allocate_for state env (true,true,false) sv32
	 of (state,(gop,[])) => (state,gop)
	  | _ => error "Got coercion")

    (* Any genop coerce *)
    fun allocate_goc_for state env sv32 = allocate_for state env (true,true,true) sv32
      

    fun sv32_trans state env (flags as (memok,immok,coerceok)) (loc : Tal.genop) (sv32 : Lil.sv32) : TS.state = 
      (case sv32
	 of Var_32 v => TS.init_var32 state loc v (LTC.ctrans env (TE.find_var32 (env,v)))
	  | Label l  => 
	   if immok then state
	   else TS.init_temp state loc (Tal.Addr (TTD.E.mk_tal_lbl l),[])
	  | Const_32 v => 
	   if immok then state
	   else TS.init_temp state loc (Tal.Immed (LU.value2w32 v),[]) 
	  | Tag w => 
	   if immok then state
	   else TS.init_temp state loc (Tal.Immed w,[]) 
	  | Unit => 
	   if immok then state
	   else TS.init_temp state loc (Tal.Addr TTD.E.l_unit,[])
	  | Coercion q => 
	   if immok then state 
	   else
	     let
	       val (state,l) = coercion_fun state env ([],q)
	     in TS.init_temp state loc (Tal.Addr l,[]) 
	     end
	  | (sv as Tabs _) =>
	   let
	     val (vks,sv) = Dec.E.nary_tabs sv
	     val q = 
	       (case Dec.Q.coercion' sv
		  of SOME q => q
		   | NONE => error "Tabs only used for coercions, I thought")
		  
	   in
	     if immok then state
	     else
	       let
		 val (state,l) = coercion_fun state env (vks,q)
	       in TS.init_temp state loc (Tal.Addr l,[]) 
	       end
	   end
	  | TApp (sv,c) => 
	   let
	     val state = 
	       if coerceok then state
	       else
		 let
		   val c = LTC.ctrans env c
		   val state = TS.emit state (Tal.Coerce (loc,[Tal.Tapp (Tal.Con c)]))
		 in state 
		 end
	     val state = sv32_trans state env flags loc sv
	   in state
	   end
	  | Coerce (sv1,sv2) => 
	   let
	     val res = 
	       (case sv1
		  of Coercion q => 
		    let
		      val state = 
			if coerceok then state
			else
			  let
			    val qs = coercion env q
			    val state = TS.emit state (Tal.Coerce (loc,qs))
			  in state
			  end
			val state = sv32_trans state env flags loc sv2
		    in state
		    end
		   | _ =>  (* Abstract coercion call.  Place arg in EAX and call coercion *)
		    let
		      val state = TS.reserve_reg state Tal.Eax

		      val (state,goc1) = allocate_goc_for state env sv1
		      val (state,goc2) = allocate_goc_for state env sv2

		      val state = TS.init_temp state loc (Tal.Reg Tal.Eax,[])
		      val state = TS.emit state (Tal.Call goc1)
		      val state = TS.init_reg state Tal.Eax goc2
		      val state = sv32_trans state env flags (#1 goc1) sv1
		      val state = sv32_trans state env flags (#1 goc2) sv2
		    in state
		    end)
	   in res
	   end)


    (* Reg required *)
    fun sv32_trans_reg state env r sv32 = sv32_trans state env (false,false,false) (Tal.Reg r) sv32

    (* Reg required, but coercion allowed *)
    fun sv32_trans_reg' state env (gop,_) sv32 = sv32_trans state env (false,false,true) gop sv32

    fun sv32_trans_regimm state env gop sv32 = sv32_trans state env (false,true,false) gop sv32

    fun sv32_trans_rm state env gop sv32 = sv32_trans state env (true,false,false) gop sv32

    (* Any genop *)
    fun sv32_trans_gop state env gop sv32 = sv32_trans state env (true,true,false) gop sv32

    (* Any genop coerce *)
    fun sv32_trans_goc state env (gop,_) sv32 = sv32_trans state env (true,true,true) gop sv32
      

    fun sv64_trans_gop state env dest farg = error "Not floats yet"

    fun goc2gop (state,goc) = 
      (case goc
	 of (r,[]) => (state,r)
	  | (r,_) => 
	   let
	     val state = TS.emit state (Tal.Coerce goc)
	   in (state,r)
	   end)

    fun gop2reg (state,gop) =
      (case gop
	 of Tal.Reg r => r
	  | _ => error "Not a register")

    (* Value "already" in EAX *)
    fun return state = 
      let
	val state = spill_callee_save state
	val state = TS.emit state (Tal.Retn (SOME(TS.get_inarg_size state - 0w1)))
	val state = pop_frame state
      in state
      end

    fun conditional_branch state env cond labels = 
      (case labels
	 of (L (l1,coerce1),L (l2,coerce2)) => 
	   let
	     val state = jmp_label state env l1 coerce1
	     val state = jcc_label state env cond l2 coerce2
	   in state
	   end
	  | (F (_,coercef), L (l,coercel)) => 
	   let
	     val state = fallthru state env coercef
	     val state = jcc_label state env cond l coercel
	   in state 
	   end
	  | (L (l,coercel), F (_,coercef)) => 
	   let
	     val state = fallthru state env coercef
	     val state = jcc_label state env (Tal.negate_condition cond) l coercel
	   in state
	   end
	  | _ => error "Bad condition code arg")

    fun jmp_dest state env dest = 
      (case dest
	 of RET => return state
	  | CC args => conditional_branch state env Tal.Eq args
	  | EFF gd => jmp_genaddr state env gd
	  | CONT (gd,_) => jmp_genaddr state env gd)


    fun sv32_return_trans state env dest sv32 = 
      let
	val state = jmp_dest state env dest
      in
	case dest
	  of RET => sv32_trans_reg state env Tal.Eax sv32
	   | CC args =>
	    let
	      val (state,goc) = allocate_goc_for state env sv32
	      val state = TS.emit state (Tal.Cmp (goc,(Tal.Immed 0w0,[])))
	      val state = sv32_trans_goc state env goc sv32
	    in state
	    end
	   | EFF gd => state
	   | CONT (gd,oper) => sv32_trans_gop state env oper sv32
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
      let
	val (state,src) = allocate_gop_for state env sv2
	val state = TS.emit state (Tal.ArithBin (what,dest,src))
	val state = sv32_trans_gop state env src sv2
	val state = sv32_trans_gop state env dest sv1
      in state
      end

    fun overflow32_check state env = 
      let
	val state = jcc_label state env Tal.Overflow (TTD.E.l_overflow) true
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
	val (state,dest) = allocate_gop_for state env sv1
	val state = TS.emit state (Tal.ArithSR (what,dest, src))

	val state = sv32_trans_gop state env dest sv1

	fun default () = (sv32_trans_reg state env Tal.Ecx sv2)

	val state = 
	  (case src
	     of SOME _ => state
	      | NONE => sv32_trans_reg state env Tal.Ecx sv2)

      in state
      end

    fun gen32mul state env dest how (sv1,sv2) = error "gen32mul:Unimplemented"

    fun word32mul state env dest args = 
      let
	val (sv1,sv2) = args
	val state = gen32mul state env dest Tal.Mul (sv1,sv2)
      in state
      end

    fun int32mul state env dest (sv1,sv2) = 
      let
	val state = overflow32_check state env
	val state = gen32mul state env dest Tal.Imul1 (sv1,sv2)
      in state
      end

    fun setref state env args = error "setref:Unimplemented"
    fun update state env args = error "update:Unimplemented"

    fun deref state env gop args = error "deref:Unimplemented"
    fun mk_ref state env gop args = error "mk_ref:Unimplemented"
    fun length_table state env gop args = error "length_table:Unimplemented"
    fun sub state env gop args = error "sub:Unimplemented"

    fun create_empty_table t cs state env gop args = 
      let
	val lc = 
	  (case (t,cs)
	     of (Prim.IntArray is,[]) => LD.T.intt (LU.i2size is)
	      | (Prim.FloatArray fs,[]) => LD.T.float ()
	      | (Prim.OtherArray true,[c]) => c
	      | _ => error "Bad table")
	val c = LTC.ctrans env lc
	val array0 = (Tal.Addr TTD.E.l_array_zero,[Tal.Tapp (Tal.Con c)])
	val state = TS.emit state (Tal.Mov (gop,array0))
      in state
      end

    fun create_table t cs state env gop args = error "create_table:Unimplemented"

    fun relop state env (sv1,sv2) =
      let
	val (state,gop1) = allocate_rm_for state env sv1
	val (state,gop2) = allocate_regimm_for state env sv2
	val state = TS.emit state (Tal.Cmp((gop1,[]),(gop2,[])))
      in state
      end

    fun frelop state env (sv1,sv2) = error "frelop: No floats yet"

    datatype prim32class = 
      Arithbin of (TS.state -> TE.env -> Tal.genop -> (sv32 * sv32) -> TS.state) * (sv32 * sv32)
      | Arithun of Tal.arithun * sv32
      | Relop of Tal.condition * (sv32 * sv32)
      | Id of sv32
      | FRelop of Tal.condition * (sv64 * sv64)
      | EffPrim of (TS.state -> TE.env -> primarg list -> TS.state) * (primarg list)
      | OtherPrim of (TS.state -> TE.env -> Tal.genop -> primarg list -> TS.state) * (primarg list)

    fun classify_prim32 (p : Prim.prim,cs : Lil.con list, primargs : Lil.primarg list) : prim32class = 
      (case p
	 of Prim.plus_int Prim.W32   => Arithbin(int32bin Tal.Add, get_2primargs32 primargs)
	  | Prim.minus_int Prim.W32  => Arithbin(int32bin Tal.Sub, get_2primargs32 primargs)
	  | Prim.plus_uint Prim.W32  => Arithbin(word32bin Tal.Add, get_2primargs32 primargs)
	  | Prim.minus_uint Prim.W32 => Arithbin(word32bin Tal.Sub, get_2primargs32 primargs)
	  | Prim.plus_uint Prim.W8   => Arithbin(word8bin Tal.Add, get_2primargs32 primargs)
	  | Prim.minus_uint Prim.W8  => Arithbin(word8bin Tal.Sub, get_2primargs32 primargs)
	  | Prim.and_int _ => Arithbin(word32bin Tal.And, get_2primargs32 primargs)
	  | Prim.or_int _  => Arithbin(word32bin Tal.Or, get_2primargs32 primargs)
	  | Prim.xor_int _ => Arithbin(word32bin Tal.Xor, get_2primargs32 primargs)
	  | Prim.lshift_int Prim.W32  => Arithbin(wordshift Tal.Shl, get_2primargs32 primargs)
	  | Prim.rshift_int Prim.W32  => Arithbin(wordshift Tal.Sar, get_2primargs32 primargs)
	  | Prim.rshift_uint Prim.W32 => Arithbin(wordshift Tal.Shr, get_2primargs32 primargs)
	  | Prim.mul_int Prim.W32  => Arithbin(int32mul, get_2primargs32 primargs)
	  | Prim.mul_uint Prim.W32 => Arithbin(word32mul, get_2primargs32 primargs)
	  | Prim.quot_int Prim.W32 => error "Unimplemented"
	  | Prim.rem_int Prim.W32 => error "Unimplemented"
	  | Prim.div_uint Prim.W32 => error "Unimplemented"
	  | Prim.mod_uint Prim.W32 => error "Unimplemented"
	       
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
	  | Prim.float2int => error "f2int: No floats"

	  | Prim.int2uint   _ => Id(get_1primarg32 primargs)
	  | Prim.uint2int   _ => Id(get_1primarg32 primargs)
	  | Prim.int2int    _ => Id(get_1primarg32 primargs)
	  | Prim.uint2uint  _ => Id(get_1primarg32 primargs)
	  | Prim.uinta2uinta _ => Id(get_1primarg32 primargs)
	  | Prim.uintv2uintv _ => Id(get_1primarg32 primargs)

	  | Prim.less_float      _ => FRelop(Tal.Less,get_2primargs64 primargs)
	  | Prim.greater_float   _ => FRelop(Tal.Greater,get_2primargs64 primargs)
	  | Prim.lesseq_float    _ => FRelop(Tal.LessEq,get_2primargs64 primargs)
	  | Prim.greatereq_float _ => FRelop(Tal.GreaterEq,get_2primargs64 primargs)
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
	  | Prim.length_table t       => OtherPrim(length_table t,primargs)

	  | Prim.mk_ref => OtherPrim(mk_ref,primargs)
	  | Prim.deref => OtherPrim(deref,primargs)

	  | Prim.equal_table _ => error "equal_table should be Ptreq"
	  | Prim.eq_ref => error "eq_ref should be Ptreq"
	  | _ => error "Bad 32 bit prim")

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


    fun cprim32_trans (state : TS.state) (env : TE.env) (destop : Tal.genop option) (cp : prim32class) : TS.state = 
      let

	(* I'm not sure that these should reserve. *)
	fun get_dest_for state destop sv = 
	  (case destop
	     of SOME d => (state,d)
	      | NONE => allocate_rm_for state env sv)
	fun get_dest memok state destop  = 
	  (case destop
	     of SOME d => (state,d)
	      | NONE => TS.reserve_temp memok state )
	val state = 
	  (case cp
	     of Arithbin (f,(sv1,sv2)) => 
	       let
		 val (sv1,sv2) = commute_args32 (sv1,sv2)
		 val (state,dest) = get_dest_for state destop sv1
	       in f state env dest (sv1,sv2)
	       end
	      | Arithun (what,sv) => 
	       let
		 val (state,dest) = get_dest_for state destop sv
	       in TS.emit state (Tal.ArithUn (what,dest))
	       end
	      | Relop (cond,args) =>
	       let (* Should try to commute args. *)
		 val (state,dest) = get_dest true state destop
		 val state = TS.emit state (Tal.Setcc (cond,dest))
		 val state = TS.init_temp state dest (Tal.Immed 0w0,[])
		 val state = relop state env args
	       in state
	       end
	      | FRelop (cond,args) =>
	       let (* Should try to commute args. *)
		 val (state,dest) = get_dest true state destop
		 val state = TS.emit state (Tal.Setcc (cond,dest))
		 val state = TS.init_temp state dest (Tal.Immed 0w0,[])
		 val state = frelop state env args
	       in state
	       end
	      | Id sv =>
	       (case destop
		  of NONE => state
		   | SOME d => sv32_trans_gop state env d sv)
	      | EffPrim (f,args) => 
	       let
		 val (state,dest) = get_dest true state destop
		 val state = f state env args
		 val state = sv32_trans_gop state env dest Unit
	       in state
	       end
	      | OtherPrim (f,args) => 
	       let
		 val (state,dest) = get_dest true state destop
		 val state = f state env dest args
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
		 val state = conditional_branch state env cond cclbls
		 val state = relop state env args
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
	      | (p,RET) =>
	       let
		 val state = TS.reserve_reg state Tal.Eax
		 val state = return state
		 val state = cprim32_trans state env (SOME(Tal.Reg (Tal.Eax))) p
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

    fun call_trans state env dest (f,eargs,fargs) = 
      (case dest
(*	 of RET =>  (* Tailcall.  No live out. *)
	   let
	     val state = spill_callee_saves state
	     val floc  = allocate_goc_for state env f
	     val state = TS.emit (Tal.Jmp floc)
	     val state = pop_frame state
	     val state = sv32_trans_goc state env f floc

	     fun floop [] => state
	       | floop farg::fargs = 
	       let
		 val state     = floop fargs
		 val (state,d) = TS.next_tailarg64 state
		 val state     = sv64_trans_gop state env d farg
	       in state
	       end
	     val state = floop fargs
	     fun loop [] => state
	       | loop arg::args = 
	       let
		 val state     = loop args
		 val (state,d) = TS.next_tailarg32 state
		 val state     = sv32_trans_gop state env d arg
	       in state
	       end
	     val state = loop args
	     val state = restore_callee_saves state

	     val (state,d) = TS.next_tailarg32 state
	     val state = place_ret_addr state env d

	   in state
	   end
	  |*)of _ => (* Normal call *)
	   let
	     (* spill caller save registers *)
	     val state = TS.pop_outargs state
	     val state = TS.alloc_outargs state (List.length eargs) (List.length fargs)
	     val state = spill_caller_save state
	     val (state,floc)  = allocate_goc_for state env f
	     val floc = call_instantiate state env floc
	     val state = TS.emit state (Tal.Call floc)
	     val state = sv32_trans_goc state env floc f

	     fun floop [] = state
	       | floop (farg::fargs) = 
	       let
		 val state     = floop fargs
		 val (state,d) = TS.reserve_next_arg64 state
		 val state     = sv64_trans_gop state env d farg
	       in state
	       end
	     val state = floop fargs
	     fun loop [] = state
	       | loop (arg::args)= 
	       let
		 val state     = loop args
		 val (state,d) = TS.reserve_next_arg32 state
		 val state     = sv32_trans_gop state env d arg
	       in state
	       end
	     val state = loop eargs
	   in state
	   end)


    fun lilprim_trans state env dest (lp,cs,sv32s,sv64s) = 
      let
	val state = jmp_dest state env dest
      in 
	case lp
	  of Box =>  error "Box: No floats"
	   | Tuple => 
	    let
	      val () = chat 5 "Allocating tuple\n"
	      val state = TS.reserve_reg state Tal.Eax
	      val state = 
		(case dest
		   of CONT (d,gd) => TS.init_temp state gd (Tal.Reg Tal.Eax,[])
		    | _ => state)

	      val nm = Name.fresh_named_var "tuplen"

	      val state = TS.emit state (Tal.Coerce(Tal.Reg Tal.Eax,[Tal.Forgetname]))
	      val state = TS.emit state (Tal.ForgetUnique nm)

	      val () = chat 5 "Initializing elements\n"

	      fun loop state i sv32s = 
		(case sv32s
		   of [] => (i,state)
		    | sv::sv32s => 
		     let
		       val (state,rim) = allocate_regimm_for state env sv
		       val md = tuple_field Tal.Eax i
		       val state = TS.emit state (Tal.Mov (md,(rim,[])))
		       val state = sv32_trans_regimm state env rim sv
		     in loop state (i+0w1) sv32s
		     end)
	      val (sz,state) = loop state 0w0 sv32s

	      val () = chat 5 "Cleaning up\n"
	      val state = TS.release_reg state Tal.Eax
	      val state = spill_caller_save state
	      val state = TS.emit state (Tal.Malloc (nm,sz*0w4,NONE))
	      val () = chat 5 "Tuple done\n"
	    in state
	    end
	   | Select w => 
	    let
	      val sv = hd sv32s
	      val (state,r) = 
		(case dest
		   of CONT (d,Tal.Reg r) => (state,r)
		    | CONT (d,gop) => 
		     let
		       val (state,r) = TS.free_temp_reg state
		       val state = TS.init_temp state gop (Tal.Reg r,[])
		     in (state,r)
		     end
		    | RET => (state,Tal.Eax)
		    | _ => TS.free_temp_reg state)
	      val (state,tr) = allocate_reg_for state env sv
	      val md = tuple_field tr w
	      val state = TS.emit state (Tal.Mov(Tal.Reg r,(md,[])))
	      val state = sv32_trans_reg state env tr sv
	    in state
	    end
	   | Dyntag => error "No exceptions"
	   | Ptreq => 
	    let
	      val (sv1,sv2) = (case sv32s
				 of [sv1,sv2] => (sv1,sv2)
				  | _ => error "Bad number of args to Ptreq")
	      val (state,gop1) = allocate_gop_for state env sv1
	      val (state,gop2) = allocate_regimm_for state env sv2
	      val state = 
		(case dest
		   of CONT (_,d) => TS.emit state (Tal.Setcc (Tal.Eq,d))
		    | RET => TS.emit state (Tal.Setcc(Tal.Eq,Tal.Reg (Tal.Eax)))
		    | _ => state)

	      val state = TS.emit state (Tal.Cmp ((gop1,[]),(gop2,[])))
	    in state
	    end
      end
    fun externapp_trans state env dest args = error "No externapps"


    fun raise_trans state env dest args = error "No raises"
    fun handle_trans state env dest args = error "No handles"
    fun op64_trans state env dest oper = error "No float ops"

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

    fun switch_trans state env dest switch =
      let
	val state = jmp_dest state env dest
	val l = fresh_tal_lbl "merge"
	val c = TS.typeof_stack state
	val state = TS.emit_block state l (SOME c)

	val fallthru_dest = 
	  (case dest 
	     of CONT (_,d) => CONT (F (l,false),d)
	      | EFF _ => EFF (F (l,false))
	      | _ => dest)

	val jump_dest = 
	  (case dest 
	     of CONT (_,d) => CONT (L (l, false),d)
	      | EFF _ => EFF (L (l,false))
	      | CC (F _,d) => CC (L (l,false),d)
	      | CC (d,F _) => CC (d, L (l,false))
	      | _ => dest)
		    
      in
	(case switch 
	   of Sumcase {arg : sv32,arms :(w32  * var * exp) list,         default: exp option, rtype : con} =>
	     let
	       val t = TE.typeof_sv32 env arg
	       val (tagcount,carriers) = Dec.C.sum_ml t
	       val nontagcount = List.length carriers

	       val bound = Name.fresh_named_var "bound"

	       fun mapper (w,v,e) = 
		 let 
		   val env = TE.bind_var32 (env,(bound,LD.COps.sum2ksum' w t))
		   val e = LS.varSv32ExpSubst v (Var_32 bound) e
		   val l = fresh_tal_lbl "arm"
		 in ((env,l,e),(w,l))
		 end

	       val (bodies,tags) = LO.map_unzip mapper arms

	       val bodies = 
		 (case default
		    of SOME e => bodies @ [(env,fresh_tal_lbl "default",e)]
		     | NONE => bodies)

	       val args = 
		 (case bodies
		    of [] => error "no arms"
		     | (env,l,e)::rest => (env,fallthru_dest,l,e)::(map (fn (env,l,e) => (env,jump_dest,l,e)) rest))

	       val state = switch_arms state env args

	       val (tarms,varms) = split tagcount tags

	       val hasvalues = not (null varms)

	       val state = fallthru state env false

	       val (state,r) = allocate_reg_for state env arg

	       val (state,vgop) = 
		 (case TS.define_var state bound
		    of (state,SOME gop) => (state,gop)
		     | (state,NONE) => (state,Tal.Reg r))

	       fun loop (state,arms) = 
		 (case arms 
		    of [] => state
		     | (w,l)::arms => 
		      let
			val state = jcc_label state env Tal.Eq l false
			val state = TS.emit state (Tal.Cmp((Tal.Reg r,[]),(Tal.Immed w,[])))
		      in loop (state,arms)
		      end)

	       val varms_l = fresh_tal_lbl "carriers"	 
	       val tarms_l = fresh_tal_lbl "tags"	 
	       val name = Name.fresh_named_var "sumtg"

	       val state =
		 if nontagcount = 0 then state
		 else if nontagcount = 1 then TS.emit_block state varms_l NONE
		 else
		   let
		     val state = loop (state,varms)
		     val state = TS.emit state (Tal.Mov (Tal.Reg r,(Tal.Prjr((r,[]),0w0,NONE),[])))
		     val state = TS.emit_block state varms_l NONE
		   in state
		   end

	       val state =
		 if tagcount = 0w0 then fallthru state env false
		 else
		   let
		     val state = loop (state,tarms)
		     val state = TS.emit_block state tarms_l NONE
		     val state = jcc_label state env Tal.Above varms_l false
		     val state = TS.emit state (Tal.Cmp((Tal.Reg r,[]),(Tal.Immed 0w255,[])))
		   in state
		   end
	       val state = TS.init_temp state vgop (Tal.Reg r,[])
	       val state = TS.emit state (Tal.Nameobj(name,Tal.Reg r));
	       val state = sv32_trans_reg state env r arg
	     in state
	     end
	    | Dyncase {arg : sv32,arms :(sv32 * (var * con) * exp) list, default: exp,        rtype : con} =>
	     error "No dyncases yet"
	    | Intcase {arg : sv32,arms :(w32 * exp) list, size : size,   default: exp,        rtype : con} =>
	     error "No intcases yet"
	    | Ifthenelse {arg : conditionCode,  thenArm : exp, elseArm : exp, rtype : con} => 
	     let
	       val l_then = fresh_tal_lbl "then"
	       val l_else = fresh_tal_lbl "else"

	       val thenArg = (env,jump_dest,l_then,thenArm)
	       val elseArg = (env,fallthru_dest,l_else,elseArm)

	       val state = switch_arms state env [elseArg,thenArg]
	       val state = fallthru state env false
	       val state = cc_trans state env (F (l_then,false),L (l_else,false)) arg
	     in state
	     end)
      end
    and switch_arms bottomstate env args = 
      let
	fun loop (topstate,args) = 
	  (case args
	     of [] => topstate
	      | ((env,dest,l,exp)::args) => 
	       let
		 val state = TS.match_registers bottomstate topstate
		 val state = TS.emit_block (etrans state env dest exp) l NONE
		 val topstate = TS.match_registers topstate state
	       in loop (topstate,args)
	       end)
	val state =
	  (case args
	     of [] => bottomstate
	      | (env,dest,l,exp)::args => 
	       let
		 val topstate = TS.emit_block (etrans bottomstate env dest exp) l NONE
	       in loop (topstate,args)
	       end)
      in state
      end
    and cc_trans state env (nonzero,zero) cc = 
      let
	fun mk_jmp cont = 
	  (case cont 
	     of F (l,b) => L (l,b)
	      | L _ => cont
	      | _ => error "Shouldn't see steps here")

	val state = 
	  (case cc
	     of Exp_cc e => etrans state env (CC (nonzero,zero)) e
	      | And_cc (cc1,cc2) => 
	       let
		 val state = cc_trans state env (nonzero,zero) cc2
		 val l_and = fresh_tal_lbl "andalso"
		 val state = TS.emit_block state l_and NONE
		 val state = cc_trans state env (F (l_and,false),mk_jmp zero) cc1
	       in state
	       end
	      | Or_cc (cc1,cc2) =>
	       let
		 val state = cc_trans state env (nonzero,zero) cc2
		 val l_or = fresh_tal_lbl "orelse"
		 val state = TS.emit_block state l_or NONE
		 val state = cc_trans state env (mk_jmp nonzero,F (l_or,false)) cc1
	       in state
	       end
	      | Not_cc cc => cc_trans state env (zero,nonzero) cc)
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
		 val (state,dest) = 
		   (case TS.define_var state x
		      of (state,SOME gop) => (state,CONT(S,gop))
		       | (state,NONE) => (state,EFF S))

		 val () = debugdo (3,fn () => (print "Exp32_b: calling op32_trans\n"))
		 val state = op32_trans state env dest oper
		 val () = debugdo (3,fn () => (print "Exp32_b: finished\n"))
	       in state
	       end
	   | Exp64_b (xf,oper) =>
	       let
		 val dest = error "op64: No floats"
		 val state = op64_trans state env dest oper
	       in state
	       end
	   | Unpack_b (a,x,sv32) => 
	       let
		 val (state,r) = TS.reserve_temp_reg state
		 val state = 
		   (case TS.define_var state x
		      of (state,SOME gop) => TS.emit state (Tal.Mov(gop,(Tal.Reg r,[])))
		       | (state,NONE) => state)
		 val (state,goc) = allocate_goc_for state env sv32
		 val state = TS.emit state (Tal.Unpack (a,r,goc))
		 val state = sv32_trans_goc state env goc sv32
		 val state = TS.release_reg state r
	       in state
	       end
	   | Split_b (a1,a2,c) => TS.emit state (Tal.Letprod ([a1,a2],LTC.ctrans env c))
	   | Unfold_b (a,c) => TS.emit state (Tal.Letroll (a,LTC.ctrans env c))
	   | Inj_b (w,a,c,sv) => 
	       let
		 val (state,goc) = allocate_goc_for state env sv
		 val c = LTC.ctrans env c
		 val state = TS.emit state (Tal.Vcase (w,c,a,goc))
		 val state= sv32_trans_goc state env goc sv
	       in state
	       end
	   | Fixcode_b _ => error "No functions allowed!!")
      in state
      end

    and etrans (state : TS.state) (env : TE.env) (dest : dest) (exp : Lil.exp) : TS.state = 
      let 
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
      in state
      end

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

    fun timports_trans env timports : (TE.env * Tal.int_con vector) =
      let
	fun loop (env,timports,acc) = 
	  (case timports 
	     of [] => (env,Vector.fromList (rev acc))
	      | (l,a,lk)::timports => 
	       let
		 val k = LTC.ktrans lk
		 val env = TE.bind_cvar (env,(a,lk))
	       in loop(env,timports,(a,k,Tal.AbsCon)::acc)
	       end)
      in loop(env,timports,[])
      end

(*

    fun place_formals state env (eFormals,fFormals) = 
      let
	val state = TS.alloc_inargs state (List.length eFormals) (List.length fFormals)
	fun floop [] = state
	  | floop ((xf,c)::fFormals) = 
	  let
	    val state     = floop fFormals
	    val (state,d) = TS.reserve_next_inarg64 state
	    val state     = TS.init_var64 state d xf (LTC.ctrans env c)
	  in state
	  end
	val state = floop fFormals
	fun loop [] = state
	  | loop ((x,c)::eFormals)= 
	  let
	    val state     = loop eFormals
	    val (state,d) = TS.reserve_next_inarg32 state
	    val state     = TS.init_var32 state d x (LTC.ctrans env c)
	  in state
	  end
	val state = loop eFormals
	val (state,d) = TS.reserve_next_inarg32 state
	val state = place_ret_addr state env d
      in state
      end

*)


    fun place_formals state env (eFormals,fFormals) = 
      let
	val state = TS.alloc_inargs state (List.length eFormals) (List.length fFormals)
	fun floop [] = state
	  | floop ((xf,c)::fFormals) = 
	  let
	    val state     = floop fFormals
	    val state = TS.bind_next_formal64 state xf (LTC.ctrans env c)
	  in state
	  end
	val state = floop fFormals
	fun loop [] = state
	  | loop ((x,c)::eFormals)= 
	  let
	    val state     = loop eFormals
	    val state = TS.bind_next_formal32 state x (LTC.ctrans env c)
	  in state
	  end
	val state = loop eFormals
	val state = bind_ret_addr state env 
      in state
      end

    fun codetrans env (l,f as Function {tFormals    : (var * kind) list,
					eFormals    : (var * con) list,
					fFormals    : (var * con) list,
					rtype       : con,
					body        : exp}) : Tal.code_block list = 
      let

	val ltype = LTC.ctrans env (TE.typeof_code env f)

	val state = TS.new_state()
	val env = TE.bind_cvars (env,tFormals)
	val env = TE.bind_vars32 (env,eFormals)
	val env = TE.bind_vars64 (env,fFormals)
	(* currently irrelevant *)
	val env = TE.set_ms_return env (Tal.cvoid (TTD.K.T32))

	val () = chat 3 ("Translating code body: "^(Name.label2string l)^"\n")
	val state = etrans state env RET body
	val () = chat 4 "Placing code formals\n"
	val state = place_formals state env (eFormals,fFormals)

	val state = TS.new_state' (TS.get_inarg_size state,TS.get_temp_size state,TS.get_outarg_size state)

	val () = chat 4 "Re-translating code body\n"
	val state = etrans state env RET body
	val () = chat 4 "Placing code formals\n"
	val state = place_formals state env (eFormals,fFormals)
	val framesize = TS.get_frame_size state
	val state = TS.emit state (Tal.ArithBin(Tal.Sub,Tal.Reg Tal.Esp,Tal.Immed (framesize * 0w4)))


	val state = TS.emit_block state (TTD.E.mk_tal_lbl l) (SOME ltype)
	val blocks = TS.get_blocks state

	val () = chat 3 "Finished with code block\n"
      in blocks
      end

    fun tuple_trans env (l,t,qs,svs) : Tal.data_block = 
      let
	val t = LTC.ctrans env t
	val qs = List.concat (map (coercion env) qs)
	val ditems = map (sv32_data_item_trans env) svs
      in (TTD.E.mk_tal_lbl l,0w4,SOME t,(ditems,qs))
      end

    fun data_block env d : Tal.data_block option = 
      (case d
	 of Dboxed (l,sv64) => error "boxed:No floats yet"
	  | Dtuple args => SOME(tuple_trans env args)
	  | Darray (l,sz,t,svs) => error "No strings yet"
	  | Dcode (l,f) => NONE)

    fun code_block env d : Tal.code_block list option = 
      (case d
	 of Dcode (l,f) => SOME (codetrans env (l,f))
	  | _ => NONE)

    fun data_blocks env data : Tal.data_block vector = Vector.fromList (List.mapPartial (data_block env) data)
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
      let
	fun finder d = 
	  (case d
	     of Dcode (l',f) => Name.eq_label (l,l')
	      | _ => false)
      in case List.find finder data
	   of SOME (Dcode (l,f)) => TE.typeof_code env f
	    | _ => error "Entry label not in data segment"
      end

    fun modtrans (MODULE {unitname : string,
			  parms : Name.LabelSet.set,
			  entry_r : label,
			  entry_c : label,
			  timports : timport list,
			  data   : data list,
			  confun : con}) : (string * Tal.tal_int * Tal.tal_imp * Tal.tal_int) = 
      let
	val _ = chat 1 "Translating timports\n"
	val (env,int_cons) = timports_trans (TE.empty()) timports
	val int_abbrevs = Vector.fromList (TalAbbrev.C.abbrevs ())
	val int_kindabbrevs = Vector.fromList (TalAbbrev.K.abbrevs ())
	val () = TA.K.reset ()
	val () = TA.C.reset ()
	val tal_int_i : Tal.tal_int =
	  { 
	   int_abbrevs = int_abbrevs,
	   int_kindabbrevs = int_kindabbrevs,
	   int_cons = int_cons,
	   int_vals = Vector.fromList [(TTD.E.l_unit,TTD.T.unit())]
	   }

	val _ = chat 1 "Creating exports\n"

	val _ = chat 2 "Translating kind of confun\n"
	val lk = TE.kindof env confun
	val k = LTC.ktrans lk

	val _ = chat 2 "Translating type of expfun\n"
	val lc = get_entry_type env data entry_r
	val c = LTC.ctrans env lc

	val int_abbrevs = Vector.fromList (TalAbbrev.C.abbrevs ())
	val int_kindabbrevs = Vector.fromList (TalAbbrev.K.abbrevs ())

	val () = TA.K.reset ()
	val () = TA.C.reset ()
	val tal_int_e : Tal.tal_int =
	  { 
	   int_abbrevs = int_abbrevs,
	   int_kindabbrevs = int_kindabbrevs,
	   int_cons = Vector.fromList [(Name.label2var (TTD.E.mk_tal_lbl entry_c),k,Tal.AbsCon)],
	   int_vals = Vector.fromList [(TTD.E.mk_tal_lbl entry_r,c)]
	   }

	val () = TA.K.reset ()
	val () = TA.C.reset ()

	val _ = chat 1 "Translating confun\n"
	val c = LTC.ctrans env confun

	val con_blocks = Vector.fromList [((TTD.E.mk_tal_lbl entry_c),k,c)]

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
	val () = TA.K.reset ()
	val () = TA.C.reset ()

	val _ = chat 1 "Finished translating module to TAL\n"
      in (unitname,tal_int_i,tal_imp,tal_int_e)
      end

    fun inttrans (INTERFACE {unitname : string,
			     timports : timport list,
			     entry_c : label * var * kind,
			     entry_r : label * con}) : string * Tal.tal_int = 
      let
	val () = TA.K.reset ()
	val () = TA.C.reset ()

	val env = foldl (fn ((l,a,k),env) => TE.bind_cvar (env,(a,k))) (TE.empty()) timports

	val _ = chat 1 "Creating con exports\n"
	val (l,a,lk) = entry_c 
	val k = LTC.ktrans lk
	val int_cons = Vector.fromList [(a,k,Tal.AbsCon)]

	val _ = chat 1 "Creating term exports\n"
	val (l,lc) = entry_r
	val c = LTC.ctrans env lc
	val int_vals = Vector.fromList [(TTD.E.mk_tal_lbl l,c)]
	val int_abbrevs = Vector.fromList (TalAbbrev.C.abbrevs ())
	val int_kindabbrevs = Vector.fromList (TalAbbrev.K.abbrevs ())
	val tal_int : Tal.tal_int =
	  { 
	   int_abbrevs = int_abbrevs,
	   int_kindabbrevs = int_kindabbrevs,
	   int_cons = int_cons,
	   int_vals = int_vals
	   }


	val _ = chat 1 "<Finished translating interface to TAL\n"

	val () = TA.K.reset ()
	val () = TA.C.reset ()

      in (unitname,tal_int)
      end

end  (* LilToTalExp *)