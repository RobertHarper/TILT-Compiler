structure LilToTalState :> LILTOTALSTATE =
  struct
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet
    structure LU = LilUtil
    structure LD = LilDefs
    structure SD = StackDesc
    structure ID = InstrDesc
    structure TE = LilToTalEnv
    structure LTC = LilToTalCon 
    structure LC = LilContext
    structure TTD = TalTranslationDefs
    structure TI = TalInstructions

    structure LO = Listops

    open StateTypes

    fun error s = Util.error "liltotalstate.sml" s    

    val LilToTalStateWarn = Stats.ff "LilToTalStateWarn"
    fun warn s = if !LilToTalStateWarn then (print "WARNING: ";print s;print "\n") else ()

    val LilToTalStateUseDual = Stats.ff "LilToTalStateUseDual"
    val debug = Stats.ff "LilToTalStateDebug"

    val debuglev = Stats.int("LilToTalStateDebuglev",0)
    val chatlev = Stats.int("LilToTalStateChatlev",0)
    val commentlev = Stats.int("LilToTalStateCommentlev",1)

    fun obind opt f = 
      (case opt 
	 of SOME a => f a
	  | NONE => NONE)

    fun indent i s = 
      let
	fun loop 0 cs = String.implode cs
	  | loop i cs = loop (i - 1) (#" "::cs)
      in loop (i+1) (String.explode s)
      end

    fun chatp i = !(chatlev) >= i
    fun chat i s = if chatp i then (print (indent i s);print "\n") else ()
    fun debugdo (i,t) = if (!debug) andalso (i <= !debuglev) then (ignore (t()); ()) else ()

    fun cout (c : Lil.con) : Lil.con_ = #c c
    fun kout (k : Lil.kind) : Lil.kind_ = #k k

    fun vector_update (v,i,vl) = Vector.mapi (fn (i',vl') => if i = i' then vl else vl') (v,0,NONE)

    type slot = SD.slot

    datatype loc32 = 
      Reg of Tal.reg
    | Slot of StackDesc.slot
    | Dual of StackDesc.slot * Tal.reg
      
    datatype loc64 = 
      FReg of int (* ith fpstack slot *)
    | FSlot of StackDesc.slot 
    | FDual of StackDesc.slot * int
      
    val rcount = 6
    fun rstr r = 
      (case r
	 of Tal.Eax => "EAX"
	  | Tal.Ebx => "EBX"
	  | Tal.Ecx => "ECX"
	  | Tal.Edx => "EDX"
	  | Tal.Esi => "ESI"
	  | Tal.Edi => "EDI"
	  | Tal.Ebp => error "Ebp not a GP register"
	  | Tal.Esp => error "Esp not a GP register"
	  | Tal.Virt _ => error "Virtuals don't have index")

    fun ridx r = 
      (case r
	 of Tal.Eax => 0
	  | Tal.Ebx => 1
	  | Tal.Ecx => 2
	  | Tal.Edx => 3
	  | Tal.Esi => 4
	  | Tal.Edi => 5
	  | Tal.Ebp => error "Ebp not a GP register"
	  | Tal.Esp => error "Esp not a GP register"
	  | Tal.Virt _ => error "Virtuals don't have index")
    fun idxr i = 
      (case i
	 of 0 => Tal.Eax 
	  | 1 => Tal.Ebx
	  | 2 => Tal.Ecx
	  | 3 => Tal.Edx
	  | 4 => Tal.Esi
	  | 5 => Tal.Edi
	  | 6 => error "Epb not a GP register"
	  | 7 => error "Esp not a GP register"
	  | _ => error "Bad register index")



    fun eq_reg r1 r2 = r1 = r2

    fun eq_gop gop1 gop2 =
      (case (gop1,gop2)
	 of (Tal.Reg r,Tal.Reg r') => r = r'
	  | (Tal.Prjr ((Tal.Esp,[]),i,NONE), Tal.Prjr ((Tal.Esp,[]),i',NONE)) => i = i'
	  | (Tal.Prjr ((Tal.Virt _,[]),i,NONE), Tal.Prjr ((Tal.Virt _,[]),i',NONE)) => i = i'
	  | _ => false)

    fun con2talcon x env c = 
      (case c 
	 of LilCon c => (warn ("Using lilcon in state reg for var "^(Name.var2string x));LTC.ctrans env c)
	  | TalCon c => c)


    structure S = 
      struct

	structure H = 
	  struct
	    type heuristics = 
	      {indices : int VarMap.map,
	       index : int,
	       spill_slots : slot VarMap.map ref}
	       
	    fun new () : heuristics = 
	      {indices = VarMap.empty,
	       index = 0,
	       spill_slots = ref VarMap.empty}
	    fun varidx v indices = 
	      (case VarMap.find (indices,v)
		 of SOME i => i
		  | NONE => (warn ("Index for var not found: "^(Name.var2string v));valOf (Int.maxInt)))
	    fun choose_spill_var (l : Lil.var list) ( {indices,...} : heuristics ) =
	      let
		fun loop ([],v,min) = v
		  | loop (v1::l,v2,min) = 
		  let
		    val vidx = varidx v1 indices
		  in if vidx < min then loop (l,v1,vidx)
		     else loop (l,v2,min)
		  end
	      in 
		(case l 
		   of [] => error "No spill candidates"
		    | v::l => loop(l,v,varidx v indices))
	      end
	    fun see_var v ({indices,index,spill_slots} : heuristics) = 
	      {indices = VarMap.insert (indices,v,index),
	       index = index + 1,
	       spill_slots = spill_slots}
	    fun get_spill_slot ({spill_slots,...} : heuristics) var = VarMap.find (!spill_slots,var)
	    fun set_spill_slot ({spill_slots,...} : heuristics) var slot = spill_slots := VarMap.insert(!spill_slots,var,slot)
	  end

	(*generate_functional_updates (NONE) (SOME "state") ["registers","stack","vars32","vars64","instr","heuristics"];*)
	type state = 
	  {registers : rstat Vector.vector,
	   stack : SD.stackdesc,
	   vars32 : loc32 VarMap.map,
	   vars64 : loc64 VarMap.map,
	   instr : ID.instrdesc,
	   heuristics : H.heuristics
	   }

	fun set_registers ({ registers = _  , stack , vars32 , vars64 , instr , heuristics } :state) registers =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr,
	   heuristics = heuristics
	   }
	fun set_stack ({ registers , stack = _  , vars32 , vars64 , instr , heuristics } :state) stack =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr,
	   heuristics = heuristics
	   }
	fun set_vars32 ({ registers , stack , vars32 = _  , vars64 , instr , heuristics } :state) vars32 =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr,
	   heuristics = heuristics
	   }
	fun set_vars64 ({ registers , stack , vars32 , vars64 = _  , instr , heuristics } :state) vars64 =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr,
	   heuristics = heuristics
	   }
	fun set_instr ({ registers , stack , vars32 , vars64 , instr = _  , heuristics } :state) instr =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr,
	   heuristics = heuristics
	   }
	fun set_heuristics ({ registers , stack , vars32 , vars64 , instr , heuristics = _  } :state) heuristics =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr,
	   heuristics = heuristics
	   }
	fun new_state () = 
	  {registers = Vector.fromList (LO.map0count (fn _ => Avail Junk) rcount),
	   stack = SD.empty_stackdesc (),
	   vars32 = VarMap.empty,
	   vars64 = VarMap.empty,
	   instr = ID.empty_instrdesc (),
	   heuristics = H.new()}

	fun new_state' frameargs = 
	  {registers = Vector.fromList(LO.map0count (fn _ => Avail Junk) rcount),
	   stack = SD.empty_stackdesc' frameargs,
	   vars32 = VarMap.empty,
	   vars64 = VarMap.empty,
	   instr = ID.empty_instrdesc (),
	   heuristics = H.new()}

	fun copy_state (state : state) : state = state
(*	  {registers = Array.tabulate(Array.length (#registers state),fn i => Array.sub(#registers state,i)),
	   stack = #stack state,
	   vars32 = #vars32 state,
	   vars64 = #vars64 state,
	   instr = #instr state}*)

	fun emit (state : state) (instr : Tal.instruction) = set_instr state (ID.emit (#instr state) instr)
	fun emitn (state : state) (instrs : Tal.instruction list) =
	  let
	    val instr = #instr state
	    val instr = foldl (fn (i,instr) => ID.emit instr i) instr instrs
	  in set_instr state instr
	  end

	fun comment state lev s = 
	  if (!commentlev) >= lev then emit state (Tal.Comment s) else state

	fun get_var32_loc' (state : state) (v : Lil.var) : loc32 option = VarMap.find (#vars32 state,v)
	fun get_var32_loc (state : state) (v : Lil.var) : loc32 = 
	  (case get_var32_loc' state v
	     of SOME loc => loc
	      | NONE => error ("No location for variable " ^ (Name.var2string v)))
	     
	fun set_var32_loc state v loc = 
	  let
	    val vars32 = VarMap.insert (#vars32 state,v,loc)
	    val heuristics = H.see_var v (#heuristics state)
	    val state = set_vars32 state vars32
	    val state = set_heuristics state heuristics
	  in state
	  end
	  
	fun get_var64_loc' (state : state) (v : Lil.var) : loc64 option = VarMap.find (#vars64 state,v)
	fun get_var64_loc (state : state) (v : Lil.var) : loc64 = 
	  (case get_var64_loc' state v
	     of SOME loc => loc
	      | NONE => error ("No location for variable " ^ (Name.var2string v)))
	     
	fun set_var64_loc state v loc = set_vars64 state (VarMap.insert (#vars64 state,v,loc))
	  
	fun delete_var32 state v = set_vars32 state (#1 (VarMap.remove (#vars32 state,v)))
	fun delete_var64 state v = set_vars64 state (#1 (VarMap.remove (#vars64 state,v)))

	fun get_rstat (state : state) (r : Tal.reg) : rstat = 
	  let
	    val regs = #registers state
	    val rstat = Vector.sub(regs,ridx r)
	  in rstat
	  end

	fun set_rstat (state : state) (r : Tal.reg) (s : rstat) : state = 
	  let
	    val regs = #registers state
	    val state = set_registers state (vector_update(regs,ridx r,s))
	  in state
	  end

	fun set_sstat (state : state) (slot : slot) (s : rstat) : state = 
	  let
	    val stack = #stack state
	    val stack = SD.set_sstat stack slot s
	    val state = set_stack state stack
	  in state
	  end

	fun get_sstat (state : state) (slot : slot) : rstat = 
	  let
	    val stack = #stack state
	    val sstat = SD.get_sstat stack slot 
	    val state = set_stack state stack
	  in sstat
	  end

	local
	  fun alloc_slot (sd_alloc_slot : SD.stackdesc -> SD.stackdesc * slot) (state : state) : (state * slot) = 
	    let
	      val stack = #stack state
	      val (stack,slot) = sd_alloc_slot stack 
	      val state = set_stack state stack
	    in (state,slot)
	    end
	in 
	  val alloc_temp_slot = alloc_slot SD.alloc_temp_slot
	  val alloc_temp64_slot = alloc_slot SD.alloc_temp64_slot

	  val alloc_outarg_slot = alloc_slot SD.alloc_outarg_slot
	  val alloc_outarg64_slot = alloc_slot SD.alloc_outarg64_slot
	end

	fun inarg_slot_i (state : state) i = SD.inarg_slot_i (#stack state) i
	fun inarg64_slot_i (state : state) i = SD.inarg64_slot_i (#stack state) i
	fun outarg_slot_i (state : state) i = SD.outarg_slot_i (#stack state) i
	fun outarg64_slot_i (state : state) i = SD.outarg64_slot_i (#stack state) i

	fun slot2genop ({stack, ...} : state) slot = SD.slot2genop stack slot
	fun genop2slot32 ({stack, ...} : state) g = SD.genop2slot32 stack g
	fun genop2slot64 ({stack, ...} : state) g = SD.genop2slot64 stack g

	fun typeof_stack (state : state) (env : TE.env) : Tal.con = 
	  let
	    val frame = SD.typeof_stack (#stack state) env
	    val s = Tal.cappend frame (TTD.T.s_con1)
	    val s = Tal.cappend s TTD.T.s_con2
	  in s
	  end

	fun typeof_reg state env r = 
	  let
	    val () = debugdo(4,fn () => print ("Begin typeof_reg \n"))
	    val t = 
	      case r 
		of Tal.Esp => SOME (Tal.csptr (typeof_stack state env))
		  
		 | Tal.Ebp => SOME (TTD.T.handler_frame (if TE.in_handler env then (Tal.cappend TTD.T.s_con1 TTD.T.s_con2) else TTD.T.s_con2))
		 | _ => 
		  let
		    fun get_type env (c,x) = 
		      let
			val () = debugdo(4,fn () => print ("typeof_reg " ^ (Name.var2string x) ^ "\n"))
			  
			val c = ((TE.find_talvar32 LTC.ctrans (env,x))
				 handle LC.Unbound _ => con2talcon x env c)
		      in SOME c
		      end
		    
		    val res = 
		      (case get_rstat state r
			 of Reserved (Full a) => get_type env a
			  | Avail(Full a) => get_type env a
			  | _ => NONE)
		  in res
		  end
	    val () = debugdo(4,fn () => print ("Done typeof_reg \n"))
	  in t
	  end
	fun typeof_reg' state env r = 
	  (case typeof_reg state env r
	     of SOME t => t
	      | NONE => Tal.pcjunk4)
	fun typeof_reg'' state env r = SOME (typeof_reg' state env r)
	fun typeof_state' (state : state) (env : TE.env) : Tal.machine_state = 
	  let
	    val ms = Tal.ms_empty
	    val ms = Tal.set_ms_eax ms (typeof_reg'' state env Tal.Eax)
	    val ms = Tal.set_ms_ebx ms (typeof_reg'' state env Tal.Ebx)
	    val ms = Tal.set_ms_ecx ms (typeof_reg'' state env Tal.Ecx)
	    val ms = Tal.set_ms_edx ms (typeof_reg'' state env Tal.Edx)
	    val ms = Tal.set_ms_esi ms (typeof_reg'' state env Tal.Esi)
	    val ms = Tal.set_ms_edi ms (typeof_reg'' state env Tal.Edi)
	    val ms = Tal.set_ms_ebp ms (typeof_reg'' state env Tal.Ebp)
	    val ms = Tal.set_ms_esp ms (typeof_reg'' state env Tal.Esp)
	  in ms
	  end
	fun typeof_state (state : state) (env : TE.env) : Tal.con = Tal.ccode_ms (typeof_state' state env)

	fun get_frame_size (state : state) : Tal.int32 = SD.get_frame_size (#stack state)
	fun get_inarg_size (state : state) : Tal.int32 = SD.get_inarg_size (#stack state)
	fun get_inarg32_size (state : state) : Tal.int32 = SD.get_inarg32_size (#stack state)
	fun get_inarg64_size (state : state) : Tal.int32 = SD.get_inarg64_size (#stack state)
	fun get_temp_size (state : state) : Tal.int32 = SD.get_temp_size (#stack state)
	fun get_temp64_size (state : state) : Tal.int32 = SD.get_temp64_size (#stack state)
	fun get_outarg_size (state : state) : Tal.int32 = SD.get_outarg_size (#stack state)

	fun grow_state oldstate state = 
	  let
	    val state = set_stack state (SD.grow_inarg32s (#stack state) (get_inarg32_size oldstate))
	    val state = set_stack state (SD.grow_inarg64s (#stack state) (get_inarg64_size oldstate))
	    val state = set_stack state (SD.grow_temps (#stack state) (get_temp_size oldstate))
	    val state = set_stack state (SD.grow_ftemps (#stack state) (get_temp64_size oldstate))
	    val state = set_stack state (SD.grow_outargs (#stack state) (get_outarg_size oldstate))
	  in state
	  end

	fun get_frame_elt_count (state : state) = SD.get_frame_elt_count (#stack state)
	fun get_outarg_count (state : state) = SD.get_outarg_count (#stack state)
	fun get_inarg_count (state : state) = SD.get_inarg_count (#stack state)
	fun get_inarg32_count (state : state) = SD.get_inarg32_count (#stack state)
	fun get_inarg64_count (state : state) = SD.get_inarg64_count (#stack state)

	fun grow_inargs state num32 num64 = 
	  let
	    val sz = LU.i2w(num32 * 4 + num64 * 8)
	    val cursz = get_inarg_size state
	    val cur32 = get_inarg32_size state
	  in 
	    if sz > cursz then 
	      set_stack state (SD.grow_inarg32s (#stack state) (cur32 + (sz - cursz)))
	    else state
	  end

	fun setup_temp64s (state : state) = 
	  let
	    val numtemps = SD.get_temp64_count (#stack state)
	    fun loop (i,state : state ) = 
	      if i = numtemps then state
	      else 
		let
		  val slot = SD.temp64_slot_i (#stack state) i
		  val offset = SD.slot2offset (#stack state) slot
		  val state = emit state (TI.coerce_slot_to_junk offset 0w8)
		in  loop (i+1,state)
		end
	  in loop (0,state)
	  end

	fun reset_state (state : state) : state = 
	  let
	  in
	    {registers = Vector.fromList(LO.map0count (fn _ => Avail Junk) rcount),
	     stack = SD.reset_stackdesc (#stack state),
	     vars32 = VarMap.empty,
	     vars64 = VarMap.empty,
	     instr = ID.flush_instrs (#instr state),
	     heuristics = H.new()}
	  end

	fun get_blocks (state : state) : Tal.code_block list = ID.get_blocks (#instr state)
	fun flush_blocks (state : state) : state = set_instr state (ID.flush_blocks (#instr state))
	fun add_blocks (state : state) (blocks : Tal.code_block list) : state = 
	  set_instr state (ID.add_blocks (#instr state) blocks)

	fun add_blocks_before (state : state) (blocks : Tal.code_block list) : state = 
	  set_instr state (ID.add_blocks_before (#instr state) blocks)

	fun emit_block (state : state) (l : Tal.label) (copt : Tal.con option) = 
	  set_instr state (ID.emit_block (#instr state) l copt)

	fun pop_outargs state = set_stack state (SD.pop_outargs (#stack state))

	fun get_outargs (state : state) = SD.get_outargs (#stack state)

	fun alloc_outarg_space state l = set_stack state (SD.alloc_outarg_space (#stack state) l)
	fun alloc_inarg_space state num32 num64 = set_stack state (SD.alloc_inarg_space (#stack state) num32 num64)
	fun adjust_inargs state num32 num64 = set_stack state (SD.adjust_inargs (#stack state) num32 num64)
      
	fun reg2string regs i  = 
	  (rstr (idxr i)) ^ " : " ^ (rstat2string (Vector.sub(regs,i)))

	fun regs2strings regs = 
	  let
	    fun loop (i,acc) = 
	      if i < rcount then loop ( i + 1,reg2string regs i :: acc)
	      else acc
	  in loop (0,[])
	  end

	fun print_stack (state : state) = SD.print_stack (#stack state)

	fun state2strings (state : state) = 
	  let
	    val strings1 = regs2strings (#registers state)
	    val strings2 = SD.stack2strings (#stack state)
	  in strings1 @ strings2
	  end
	fun print_state (state : state) = app (fn s => (print s;print "\n")) (state2strings state)

	fun comment_stack (state : state) i = 
	  let
	    val state = List.foldr (fn (s,state) => comment state i s) state (state2strings state)
	  in state
	  end

	fun vars_in_regs (state : state) = 
	  let
	    fun folder (rstat,vars) = 
	      (case rstat
		 of Avail (Full(c,x)) => x::vars
		  | _ => vars)
	  in Vector.foldl folder [] (#registers state)
	  end

	fun choose_var_to_spill state = 
	  (case vars_in_regs state
	     of [] => NONE
	      | vars => SOME (H.choose_spill_var vars (#heuristics state)))

	fun choose_reg_to_spill state =
	  obind (choose_var_to_spill state)
	  (fn v => case get_var32_loc state v 
		     of Reg r => SOME r
		      | Dual (s,r) => SOME r
		      | Slot _  => error "Spill var not in a reg!")
	    
      end (* structure S *)


    fun print_loc loc = 
      (case loc
	 of Reg r => Pptal.pp_genop (Tal.Reg r)
	  | Slot s => SD.print_slot s
	  | Dual (s,r) => (Pptal.pp_genop (Tal.Reg r); print " and ";SD.print_slot s))

    fun get_rcont state r = rstat2rcont (S.get_rstat state r)

    fun get_scont state s = rstat2rcont (S.get_sstat state s)

    fun genop2loc state gop = 
      (case gop 
	 of Tal.Reg r => 
	   (case get_rcont state r
	      of Full(c,x) => S.get_var32_loc state x
	       | Junk => Reg r)
	  | _ => 
	   let
	     val slot = S.genop2slot32 state gop
	   in 
	     (case get_scont state slot
		of Full(c,x) => S.get_var32_loc state x
		 | Junk => Slot slot)
	   end)


    fun genop2loc64 state gop = 
      (case gop 
	 of Tal.Reg r => error "Not a float operand"
	  | _ => FSlot(S.genop2slot64 state gop))

    fun loc2genop state loc = 
      (case loc 
	 of Dual (s,r) => Tal.Reg r
	  | Reg r => Tal.Reg r
	  | Slot slot => S.slot2genop state slot)


    fun loc642genop state loc = 
      (case loc 
	 of FSlot slot => S.slot2genop state slot
	  | _ => error "Float registers not handled yet")

    fun set_loc_stat state loc s = 
      (case loc
	 of Reg r => S.set_rstat state r s
	  | Slot slot => S.set_sstat state slot s
	  | Dual(slot,r) => 
	   let
	     val state = S.set_rstat state r s
	     val state = S.set_sstat state slot s
	   in state
	   end)

    fun set_loc64_stat state loc s = 
      (case loc
	 of FSlot slot => S.set_sstat state slot s
	  | _ => error "Float registers not yet handled")

    fun get_loc_stat state loc = 
      (case loc
	 of Reg r => S.get_rstat state r
	  | Slot slot => S.get_sstat state slot
	  | Dual(slot,r) => S.get_rstat state r)

    fun get_loc64_stat state loc = 
      (case loc
	 of FSlot slot => S.get_sstat state slot
	  | _ => error "no fregs")

    fun release_reg state r = 
      (case S.get_rstat state r
	 of Reserved rs' => S.set_rstat state r (Avail rs')
	  | _ => state)


    fun release_slot state slot = 
      (case S.get_sstat state slot
	 of Reserved rs' => S.set_sstat state slot (Avail rs')
	  | _ => state)

    fun release_loc state loc = 
      (case loc 
	 of Reg r => release_reg state r
	  | Slot slot  => release_slot state slot
	  | Dual (slot,r) => 
	   let
	     val state = release_reg state r
	     val state = release_slot state slot
	   in state
	   end)
(* XXX
    fun release_locs state locs = List.foldl (fn (loc,state) => release_loc state loc) state locs

    fun release_loc64 state loc = state	(* XXX: Just a placeholder *)
    fun release_locs64 state locs = state (* XXX: Just a placeholder *)
*)
    fun release_temp state gop = release_loc state (genop2loc state gop)

    fun release_temp64 state gop = 
      let
	val slot = S.genop2slot64 state gop
      in 
	case S.get_sstat state slot
	  of Reserved rs => 
	    S.set_sstat state slot (Avail rs)
	   | _ => state
      end

    fun get_reg_rcont state r = 
      rstat2rcont (S.get_rstat state r)
    fun get_slot_rcont state s = 
      rstat2rcont (S.get_sstat state s)
    fun get_loc_rcont state l = 
      rstat2rcont (get_loc_stat state l)
    fun get_loc64_rcont state l = 
      rstat2rcont (get_loc64_stat state l)


    fun mark_reg what state r  = S.set_rstat state r (what(get_reg_rcont state r))
    fun mark_slot what state s = S.set_sstat state s (what(get_slot_rcont state s))
    fun mark_loc what state l  = set_loc_stat state l (what(get_loc_rcont state l))
    fun mark_loc64 what state l = set_loc64_stat state l (what(get_loc64_rcont state l))

    (* Pick a free register for allocation if one exists. *)
    fun choose_free_reg (state : S. state) = 
      let
	fun folder (idx,rstat,opt) =
	  (case rstat
	     of Avail Junk => SOME (idxr idx)
	      | _ => opt)
      in Vector.foldri folder NONE (#registers state,0,NONE)
      end

    (* Choose a register to spill, based on a minor heuristic (least recently used var).
     * This doesn't check to make sure that all variables are actually in use,
     * so try choose_free_reg first.  *)
    fun choose_spill_reg (state : S.state) = 
      (case S.choose_reg_to_spill state
	 of SOME r => r
	  | NONE => (* If no vars are available to be spilled, spill an abitrary temp *)
	   let
	     fun folder (idx,rstat,opt) =
	       (case rstat
		  of Avail(Full _) => SOME (idxr idx)
		   | _ => opt)
	   in case Vector.foldri folder NONE (#registers state,0,NONE)
		of SOME r => r
		 | NONE => error "All registers reserved: too many reg temps."
	   end)


    fun get_free_temp_slot' state rcont = 
      let
	val (state,slot) = S.alloc_temp_slot state
	val state = S.set_sstat state slot (Avail rcont)
      in (state,slot)
      end

    fun get_free_temp_slot64' state rcont = 
      let
	val (state,slot) = S.alloc_temp64_slot state
	val state = S.set_sstat state slot (Avail rcont)
      in (state,slot)
      end

    (*  For a variable v : con which is in a register:
     *  1) if v is in a register only, move it to a temp slot
     *  2) if v is in a Dual slot, narrow it down to a temp.
     *)
    fun spill_var_from_reg state (v,con) = 
      let

	val () = debugdo(2,fn () => print ("Spilling var " ^ (Name.var2string v) ^ "\n"))

	val oldloc = S.get_var32_loc state v
	val (state,newloc) =
	  (case oldloc
	     of Dual (s,r) => 
	       let
		 val state = S.emit state (Tal.Mov (Tal.Reg r,(S.slot2genop state s,[])))
		 val state = S.set_rstat state r (Avail Junk)
	       in (state,Slot s)
	       end
	      | Reg r => 
	       let
		 val (state,slot) = get_free_temp_slot' state (Full (con,v))
		 (* Spilling a var means loading it into its designated location *)
		 val state = S.emit state (Tal.Mov (Tal.Reg r,(S.slot2genop state slot,[])))
		 val state = S.set_rstat state r (Avail Junk)
	       in (state,Slot slot)
	       end
	      | Slot slot => error "Variable not in register")

	val state = S.comment state 6 ("Spilling var " ^ (Name.var2string v))

      in S.set_var32_loc state v newloc
      end


    (* Make a particular register available , spilling if necessary
     *)
    fun make_reg_available state r =
      let
	val state = 
	  case S.get_rstat state r
	    of Avail(Full (con,v)) => spill_var_from_reg state (v,con)
	     | Avail Junk => state
	     | Reserved _ => error "Freeing reserved register"
      in state
      end

    (* Set register contents to rcont, spilling first if full *)
    fun spill_and_set_reg state r rcont = 
      let
	val state = make_reg_available state r
	val state = S.set_rstat state r (Avail rcont)
      in state
      end

    (* Get an arbitrary free register, spilling if necessary *)
    fun get_free_reg' state rcont = 
      (case choose_free_reg state
	 of SOME r => (S.set_rstat state r (Avail rcont),Reg r)
	  | NONE => 
	   let
	     val r = choose_spill_reg state
	     val state = make_reg_available state r 
	     val state = S.set_rstat state r (Avail rcont)
	   in (state,Reg r)
	   end)
	  

    fun get_free_loc' (memok : bool) (state : S.state) (rcont : rcont) : (S.state * loc32) = 
      (case choose_free_reg state
	 of SOME r => (S.set_rstat state r (Avail rcont),Reg r)
	  | NONE => 
	   if memok then
	     let
	       val (state,slot) = get_free_temp_slot' state rcont
	     in (state,Slot slot)
	     end
	   else get_free_reg' state rcont)
	
    fun get_free_temp' memok state rcont = 
      let
	val (state,loc) = get_free_loc' memok state rcont
      in (state,loc2genop state loc)
      end

    fun get_free_temp_reg' (state : S.state) (rcont : rcont) : (S.state * Tal.reg) = 
      (case get_free_temp' false state rcont
	 of (state,Tal.Reg r) => (state,r)
	  | _ => error "Not a register")

    fun get_free_temp_reg state = get_free_temp_reg' state Junk
    fun get_free_temp_any state = get_free_temp' true state Junk

    fun get_free_temp memok state = get_free_temp' memok state Junk

    fun get_free_loc64 (memok : bool) (state : S.state) (rcont : rcont) : (S.state * loc64) = 
      if memok then
	let
	  val (state,slot) = get_free_temp_slot64' state rcont
	in (state,FSlot slot)
	end
      else error "No float reg allocation yet"
	
    fun get_free_temp64' memok state rcont = 
      let
	val (state,loc) = get_free_loc64 memok state rcont
      in (state,loc642genop state loc)
      end


    (* Set a register to Reserved, spilling if necessary *)
    fun reserve_reg' state r rcont = 
      let
	val state = make_reg_available state r
	val state = S.set_rstat state r (Reserved rcont)
      in state
      end


    fun reserve_slot' state rcont = 
      let
	val (state,slot) = S.alloc_temp_slot state
	val state = S.set_sstat state slot (Reserved rcont)
      in (state,slot)
      end

    fun reserve_loc' (memok : bool) (state : S.state) (rcont : rcont) : (S.state * loc32) = 
      let
	val (state,loc) = get_free_loc' memok state rcont
	val state = mark_loc Reserved state loc
      in (state,loc)
      end
	
    fun reserve_temp' memok state rcont = 
      let
	val (state,loc) = reserve_loc' memok state rcont
      in (state,loc2genop state loc)
      end

    fun reserve_reg state r = reserve_reg' state r Junk
    fun reserve_slot state = reserve_slot' state Junk

    fun reserve_one_of_regs state rlist = 
      let
	fun findloop [] = NONE
	  | findloop (r::rlist) = 
	  (case S.get_rstat state r 
	     of Avail Junk => SOME r
	      | _ => findloop rlist)

	fun spillloop [] = error "All registers from list are reserved"
	  | spillloop (r::rlist) =
	  (case S.get_rstat state r 
	     of Avail _ => (reserve_reg state r,r)
	      | _ => spillloop rlist)

      in case findloop rlist
	   of SOME r => (reserve_reg state r,r)
	    | NONE => spillloop rlist
      end

    fun reserve_temp memok state = reserve_temp' memok state Junk

    fun reserve_temp_reg' (state : S.state) (rcont : rcont) : (S.state * Tal.reg) = 
      (case reserve_temp' false state rcont
	 of (state,Tal.Reg r) => (state,r)
	  | _ => error "Not a register")

    fun reserve_temp_reg state = reserve_temp_reg' state Junk

    val reserve_temp_any = reserve_temp true

    fun reserve_temp_slot state = 
      let
	val (state,slot) = reserve_slot state
      in (state,S.slot2genop state slot)
      end

    fun reserve_slot64' state rcont = 
      let
	val (state,slot) = S.alloc_temp64_slot state
	val state = S.set_sstat state slot (Reserved rcont)
      in (state,slot)
      end

    fun reserve_loc64 (state : S.state) (rcont : rcont) : (S.state * loc64) = 
      let
	val (state,slot) = reserve_slot64' state rcont
      in (state,FSlot slot)
      end
	
    fun reserve_slot64 state = reserve_slot64' state Junk

    fun eq_gop_goc gop1 (gop2,qs) = 
      if eq_gop gop1 gop2 then
	SOME qs
      else NONE

    fun reg_mov state r1 r2 = 
      if eq_reg r1 r2 then state
      else S.emit state (Tal.Mov(Tal.Reg r1,(Tal.Reg r2,[])))

    fun reg_slot_mov state r s = S.emit state (Tal.Mov(Tal.Reg r,(S.slot2genop state s,[])))

    fun slot_reg_mov state s r = S.emit state (Tal.Mov(S.slot2genop state s,(Tal.Reg r,[])))

    fun eq_loc loc1 loc2 = 
      (case (loc1,loc2)
	 of (Reg r,Reg r') => eq_reg r r'
	  | (Slot s1,Slot s2) => SD.eq_slot s1 s2
	  | (Dual(s1,r1),Dual(s2,r2)) => SD.eq_slot s1 s2 andalso eq_reg r1 r2
	  | _ => false)

    fun eq_loc64 loc1 loc2 = 
      (case (loc1,loc2)
	 of (FSlot s1,FSlot s2) => SD.eq_slot s1 s2
	  | _ => error "Float slots only")

    fun sub_loc loc1 loc2 = 
      (case (loc1,loc2)
	 of (Reg r,Reg r') => eq_reg r r'
	  | (Slot s1,Slot s2) => SD.eq_slot s1 s2
	  | (Dual(s1,r1),Dual(s2,r2)) => SD.eq_slot s1 s2 andalso eq_reg r1 r2
	  | (Dual(s1,r1),Reg r2) => eq_reg r1 r2
	  | (Dual(s1,r1),Slot s2) => SD.eq_slot s1 s2
	  | _ => false)

    (*Locations of loc1 minus locations of loc2 *)
    fun loc_diff loc1 loc2 = 
      (case (loc1,loc2)
	 of (Reg r,Reg r') => if eq_reg r r' then [] else [Reg r]
	  | (Slot s1,Slot s2) => if SD.eq_slot s1 s2 then [] else [Slot s1]
	  | (Dual(s1,r1),Dual(s2,r2)) => 
	   (if SD.eq_slot s1 s2 then [] else [Slot s1]) @
	   (if eq_reg r1 r2 then [] else [Reg r1])
	  | (Dual(s1,r1),Reg r2) => if eq_reg r1 r2 then [Slot s1] else [Slot s1,Reg r1]
	  | (Dual(s1,r1),Slot s2) => if SD.eq_slot s1 s2 then [Reg r1] else [Slot s1,Reg r1]
	  | (Reg r1,Dual (s2,r2)) => if eq_reg r1 r2 then [] else [Reg r1]
	  | (Slot s1,Dual (s2,r2)) => if SD.eq_slot s1 s2 then [] else [Slot s1]
	  | _ => [loc1])

    (*Locations of loc1 minus locations of loc2 *)
    fun loc64_diff loc1 loc2 = 
      (case (loc1,loc2)
	 of (FSlot s1,FSlot s2) => if SD.eq_slot s1 s2 then [] else [FSlot s1]
	  | _ => error "No Fp regs yets")

    fun slot_slot_mov state s1 s2 = 
      if SD.eq_slot s1 s2 then state
      else
	let
	  (* Protect the current slots from being chosen as spill slots.
	   * (Don't assume that they are already marked as having contents,  
	   * even though this really ought to be the case.) *)
	  val s1stat = S.get_sstat state s1
	  val s2stat = S.get_sstat state s2
	  val state = S.set_sstat state s1 (Reserved Junk)
	  val state = S.set_sstat state s2 (Reserved Junk)
	  val (state,r) = reserve_temp_reg state
	  val state = slot_reg_mov state s1 r
	  val state = reg_slot_mov state r s2
	  val state = release_reg state r
	  val state = S.set_sstat state s1 s1stat
	  val state = S.set_sstat state s2 s2stat
	in state
	end

    (*XXX wrong once we do fpreg allocation *)
    fun slot_slot_mov64 state s1 s2 = 
      if SD.eq_slot s1 s2 then state
      else
	let
	  val state = S.emitn state (TI.fp_mov (S.slot2genop state s1) (S.slot2genop state s2))
	in state
	end
	
    fun loc_mov state loc1 loc2 = 
      let
	val state = 
	  (case (loc1,loc2) 
	     of (Reg r1,Reg r2) => reg_mov state r1 r2
	      | (Reg r,Slot s) => reg_slot_mov state r s
	      | (Slot s,Reg r) => slot_reg_mov state s r
	      | (Slot s1,Slot s2) => slot_slot_mov state s1 s2
	      | (Reg r1,Dual (s,r2)) => reg_mov state r1 r2
	      | (Slot s1,Dual(s2,r)) => slot_reg_mov state s1 r
	      | (Dual(s,r1),Reg r2) => 
	       let
		 val state = reg_mov state r1 r2
		 val state = slot_reg_mov state s r2
	       in state
	       end
	      | (Dual(s1,r1),Slot s2) => 
	       let
		 val state = reg_slot_mov state r1 s2
		 val state = slot_slot_mov state s1 s2
	       in state
	       end
	      | (Dual(s1,r1),Dual(s2,r2)) => 
	       let
		 val state = reg_mov state r1 r2
		 val state = 
		   if SD.eq_slot s1 s2 then state
		   else slot_reg_mov state s1 r2
	       in state
	       end)
      in state
      end

    fun loc64_mov state loc1 loc2 = 
      let
	val state = 
	  (case (loc1,loc2) 
	     of (FSlot s1,FSlot s2) => slot_slot_mov64 state s1 s2
	      | _ => error "No floating point regs yet")
      in state
      end

    (* Mov variable association to loc, emitting code to
     * initialize variables old location from the new.  Sets
     * location status to the same as the old location status 
     *)
    fun mov_var_to_loc state x loc = 
      let
	val oldloc = S.get_var32_loc state x

	val () = debugdo (6,fn () => (print "Moving var ";
				      PpLil.pp_var x;
				      print " from location ";
				      print_loc oldloc;
				      print " to location ";
				      print_loc loc;
				      print "\n"))

	val status = get_loc_stat state oldloc
	val state = loc_mov state oldloc loc
	val state = S.set_var32_loc state x loc
	val state = set_loc_stat state oldloc (Avail Junk)
	val state = set_loc_stat state loc status
      in state
      end

    fun mov_var_to_loc64 state x loc = 
      let
	val oldloc = S.get_var64_loc state x
	val status = get_loc64_stat state oldloc
	val state = loc64_mov state oldloc loc
	val state = S.set_var64_loc state x loc
	val state = set_loc64_stat state oldloc (Avail Junk)
	val state = set_loc64_stat state loc status
      in state
      end

    (* Emit code to initialize register,
     * and ensure its status is Junk.
     *)
    fun init_reg (state : S.state) (r : Tal.reg) (goc : Tal.genop Tal.coerce) : S.state = 
      (case eq_gop_goc (Tal.Reg r) goc
	 of SOME [] => state
	  | SOME qs => S.emit state (Tal.Coerce goc)
	  | NONE => 
	   let
	     val state = S.emit state (Tal.Mov (Tal.Reg r,goc))
	     val state = S.set_rstat state r (Avail Junk)
	   in state
	   end)


    (* Emit code to initialize temp,
     * and change its status from Reserved to Junk.
     * init_temp state dest src
     *)
    fun init_temp (state : S.state) (gop : Tal.genop) (goc as (gop2,qs) : Tal.genop Tal.coerce) : S.state = 
      let
	val () = debugdo (7,fn () => (print "State before is \n";S.print_state state;print "\n"))
	val state = 
	  (case gop
	     of Tal.Reg r => init_reg state r goc
	      | Tal.Prjr _ => 
	       (case eq_gop_goc gop goc
		  of SOME [] => state
		   | SOME qs => S.emit state (Tal.Coerce goc)
		   | NONE => 
		    (case goc 
		       of (Tal.Prjr _,_) =>  (* No mem/mem moves. *)
			 let
			   val (state,r) = get_free_temp_reg state
			   val state = S.emit state (Tal.Mov (gop,(Tal.Reg r,[])))
			   val state = init_reg state r goc
			 in state
			 end
			| _ =>  
			 let
			   val state = S.emit state (Tal.Mov (gop,goc))
			   val state = S.set_sstat state (S.genop2slot32 state gop) (Avail Junk)
			 in state
			 end))
	      | _ => state)
	val () = debugdo (7,fn () => (print "State after is \n";S.print_state state;print "\n"))
      in state
      end

    (* Emit code to initialize temp64,
     * and change its status from Reserved to Junk.
     * init_temp state dest src
     *)
    fun init_temp64 (state : S.state) (dest : Tal.genop) (src : Tal.genop) : S.state = 
      (case dest
	 of Tal.Reg r => error "Not a float operand"
	  | Tal.Prjr ((_,[]),i,NONE) => 
	   (if eq_gop dest src then state
	    else
	      (case src
		 of Tal.Prjr _ =>  
		   let
		     val state = S.emit state (Tal.FPsomeargs(Tal.Fstp,Tal.FPgenop(Tal.B8,dest)))
		     val state = S.emit state (Tal.FPsomeargs(Tal.Fld,Tal.FPgenop(Tal.B8,src)))
		     val state = S.set_sstat state (S.genop2slot64 state dest) (Avail Junk)
		   in state
		   end
		  | _ =>  error "Not a float operand"))
	  | _ => error "Not a temp")



    (* Lookup the assigned location for a variable, if one exists. 
     *)
    fun define_var32 memok state x = 
      let
	val () = debugdo(3,fn () => (print "Defining var: ";
				     PpLil.pp_var x;
				     print "\n"))
	val res = 
	  (case S.get_var32_loc' state x
	     of SOME loc => 
	       let
		 val (state,loc) = 
		   (case loc
		      of Reg r => 
			(S.set_rstat state r (Reserved Junk),Reg r)
		       | Slot slot =>
			if memok then  
			  (S.set_sstat state slot (Reserved Junk),Slot slot)
			else
			  let
			    val (state,loc') = reserve_loc' false state Junk
			    val state = loc_mov state loc loc'
			    val state = S.set_sstat state slot (Avail Junk)
			  in (state,loc')
			  end
		       | Dual(slot,r) => 
			let
			  val state = S.set_rstat state r (Reserved Junk)
			  val state = S.set_sstat state slot (Avail Junk)
			  val state = S.emit state (Tal.Mov(S.slot2genop state slot,(Tal.Reg r,[])))
			in (state,loc)
			end)
		 val state = S.delete_var32 state x
	       in (state,loc2genop state loc)
	       end
	      | NONE => reserve_temp memok state)
      in res
      end



    (* Lookup the assigned location for a variable, if one exists. 
     *)
    fun define_var64 state x = 
      let
	val () = debugdo(3,fn () => (print "Defining var64: ";
				     PpLil.pp_var x;
				     print "\n"))
      in
	(case S.get_var64_loc' state x
	   of SOME loc => 
	     let
	       val state = 
		 (case loc
		    of FSlot slot => S.set_sstat state slot (Reserved Junk)
		     | _ => error "Float registers not yet used")
	       val state = S.delete_var64 state x
	     in (state,loc642genop state loc)
	     end
	    | NONE => 	
	     let
	       val (state,loc) = reserve_loc64 state Junk
	     in (state,loc642genop state loc)
	     end)
      end



    (* Kill a variable.
     *)
    fun kill_var32 state x = 
      let
	val () = debugdo(3,fn () => (print "killing var: ";
				     PpLil.pp_var x;
				     print "\n"))
	val res = 
	  (case S.get_var32_loc' state x
	     of SOME loc => 
	       let
		 val state = set_loc_stat state loc (Avail Junk)
		 val state = S.delete_var32 state x
	       in state
	       end
	      | NONE => state)
      in res
      end



    (* Lookup the assigned location for a variable, if one exists. 
     * Kill the variable as well.
     *)
    fun kill_var64 state x = 
      let
	val () = debugdo(3,fn () => (print "Killing var64: ";
				     PpLil.pp_var x;
				     print "\n"))
      in
	(case S.get_var64_loc' state x
	   of SOME loc => 
	     let
	       val state = 
		 (case loc
		    of FSlot slot => S.set_sstat state slot (Avail Junk)
		     | _ => error "Float registers not yet used")
	       val state = S.delete_var64 state x
	     in state
	     end
	    | NONE => state)
      end


    fun reserve_loc_for_var memok state x c = 
      (case S.get_var32_loc' state x
	 of SOME loc =>
	   (case (loc,memok)
	      of (Slot slot,false) =>
		if !LilToTalStateUseDual then
		  let
		    val (state,r) = reserve_temp_reg' state (Full(c,x))
		    val state = S.set_var32_loc state x (Dual (slot,r))
		  in (state,Reg r)
		  end
		else
		  let
		    val (state,r) = reserve_temp_reg' state (Full(c,x))
		    val state = set_loc_stat state loc (Avail Junk)
		    val state = slot_reg_mov state slot r
		    val state = S.set_var32_loc state x (Reg r)
		  in (state,Reg r)
		  end 
	       | _ => 
		(case get_loc_stat state loc
		   of Avail rc => (set_loc_stat state loc (Reserved rc),loc)
		    | _ => error "Already reserved"))
	  | NONE => (* Never seen this var before *)
	   let
	     val (state,loc) = reserve_loc' memok state (Full (c,x))
	     val state = S.set_var32_loc state x loc
	   in (state, loc)
	   end)

    fun reserve_for_var memok state x c = 
      let
	val (state,loc) = reserve_loc_for_var memok state x c
      in (state,loc2genop state loc)
      end


    fun reserve_loc_for_var64 state x c = 
      (case S.get_var64_loc' state x
	 of SOME loc => (state,loc)
	  | NONE => (* Never seen this var before *)
	   let
	     val (state,loc) = reserve_loc64 state (Full (c,x))
	     val state = S.set_var64_loc state x loc
	   in (state, loc)
	   end)

    fun reserve_for_var64 state x c = 
      let
	val (state,loc) = reserve_loc_for_var64 state x c
      in (state,loc642genop state loc)
      end


    fun sanity_check' allow_reserved (state : S.state) = 
      let
	val regs = #registers state
	fun var_error x s =
	  (print "Problem with var ";PpLil.pp_var x;print "\n";
	   error s)
	fun loop i = 
	  if i < rcount then 
	    let
	      fun var_check x = 
		(case S.get_var32_loc' state x
		   of NONE => var_error x "UNSANE: NO location for var in register"
		    | SOME (Reg r) => if ridx r = i then ()
				      else var_error x "UNSANE: var info/reg mismatch"
		    | SOME (Dual(slot,r)) => if ridx r = i then ()
					     else var_error x "UNSANE: var info/dual mismatch"
		    | _ => var_error x "UNSANE: var loc is Slot, but reg has var")
		   
	      val () = 
		(case Vector.sub(regs,i)
		   of Avail(Full(c,x)) => var_check x
		    | Avail(Junk) => ()
		    | Reserved (Full(c,x)) => if allow_reserved then var_check x
					      else error "UNSANE: Register is reserved across op/block boundary"
		    | Reserved Junk => if allow_reserved then ()
				       else error "UNSANE: Register is reserved across op/block boundary")
	    in loop ( i + 1)
	    end
	  else ()
	fun apper (x,loc) = 
	  let
	    fun check_var x' = 
	      if Name.eq_var (x,x') then () 
	      else (print "Var ";PpLil.pp_var x; print " marked with location: ";print_loc loc;print "\n";
		    print "Location holds var ";PpLil.pp_var x';print "\n";
		    var_error x "UNSANE: var has bad location")
	  in
	    case get_loc_stat state loc
	      of Avail (Full(c,x')) => check_var x'
	       | Reserved (Full(c,x')) => check_var x'
	       | _ => (print "Location is ";print_loc loc;print "\n";
		       var_error x "UNSANE: var is unmarked")
	  end
	     
	val () = (loop 0;
		  VarMap.appi apper (#vars32 state)) handle any => (print "UNSANE STATE: state is\n";
								    S.print_state state;
								    print "\n";
								    raise any)
	val () = chat 4 "Sanity check passed"
      in ()
      end

    val sanity_check = sanity_check' false

    (*
     * If we've seen the variable before then:
     *
     * previously emitted code expects x to be in loc.
     * previously emitted code also uses gop.
     *
     * if gop and loc are the same, we do not need
     * to do anything.  This will always be the case
     * if gop was reserved via reserve_for_var.
     * 
     * Otherwise, we have a choice here: we could keep 
     * the variable association the same and emit code 
     * to initialize gop from loc, or we could change the
     * variable association to gop, and emit code to 
     * initialize loc from gop.  We do the former.
     *)
    fun init_var32 state gop x c = 
      (case S.get_var32_loc' state x
	 of SOME loc => 
	   let
	     val gop2 = loc2genop state loc
	     val state = 
	       if eq_gop gop gop2 then state 
	       else init_temp state gop (gop2,[])
	     val state = release_temp state gop
	     val () = sanity_check' true state
	     val () = chat 5 "Finished init_var"
	   in state
	   end
	  | NONE => 
	   let
	     val loc = genop2loc state gop
	     val state = set_loc_stat state loc (Avail (Full(c,x)))
	     val state = S.set_var32_loc state x loc
	   in state
	   end)

    fun init_var64 state gop x c = 
      (case S.get_var64_loc' state x
	 of SOME loc => 
	   let
	     val gop2 = loc642genop state loc
	     val state = 
	       if eq_gop gop gop2 then state 
	       else init_temp64 state gop gop2
	     val state = release_temp64 state gop
	   in state
	   end
	  | NONE => 
	   let
	     val loc = genop2loc64 state gop
	     val state = set_loc64_stat state loc (Avail (Full(c,x)))
	     val state = S.set_var64_loc state x loc
	   in state
	   end)


    fun load_var state r x c = 
      let
	val () = chat 7 ("Loading var: "^(Name.var2string x))
	val state = 
	  case S.get_var32_loc' state x
	    of SOME (Reg r') => 
	      if r = r' then state
	      else 
		let
		  val state = spill_and_set_reg state r (Full (c,x))
		  val state = S.set_rstat state r' (Avail Junk)
		  val state = S.emit state (Tal.Mov(Tal.Reg r',(Tal.Reg r,[])))
		  val state = S.set_var32_loc state x (Reg r)
		in state
		end
	     | SOME (Dual(slot,r')) =>
		if r = r' then state
		else 
		  let
		    val state = spill_and_set_reg state r (Full (c,x))
		    val state = S.set_rstat state r' (Avail Junk)
		    val state = S.emit state (Tal.Mov(Tal.Reg r',(Tal.Reg r,[])))
		    val state = S.set_var32_loc state x (Dual(slot,r))
		  in state
		  end
	     | SOME (Slot slot) =>
		  if !LilToTalStateUseDual then
		    let
		      val state = spill_and_set_reg state r (Full (c,x))
		      val state = S.set_var32_loc state x (Dual(slot,r))
		    in state
		    end
		  else
		    let
		      val state = spill_and_set_reg state r (Full (c,x))
		      val state = S.set_sstat state slot (Avail Junk)
		      val state = slot_reg_mov state slot r
		      val state = S.set_var32_loc state x (Reg r)
		      val () = sanity_check' true state
		    in state
		    end
	     | NONE =>
		  let
		    val state = spill_and_set_reg state r (Full (c,x))
		    val state = S.set_var32_loc state x (Reg r)
		  in state
		  end
	val () = chat 7 ("Done loading var")
      in state 
      end

    fun add_blocks_from_state dest source = S.add_blocks dest (S.get_blocks source)
    fun add_blocks_before_from_state dest source = S.add_blocks_before dest (S.get_blocks source)

(*
    fun realloc_inargs state num32 num64 = 
      let
	val inargs = S.get_inargs state

	fun floop(inargs,num64,state) = 
	  if num64 = 0 then loop (inargs,num32,state)
	  else
	    (case inargs
	       of [] => (Avail Junk,false) :: floop (inargs,num64 -1,state)
		| arg::inargs => 
		 (case arg
		    of 
*)
    fun pop_outargs state = 
      let
	val outargs = S.get_outargs state
	val size = S.get_outarg_size state
	fun loop (outargs,state) = 
	  (case outargs
	     of [] => state
	     | arg::outargs =>
	       let
		 val state = 
		   (case arg
		      of (Avail(Full(c,x)),true) => 
			(case S.get_var32_loc state x
			   of Dual(slot,r) => mov_var_to_loc state x (Reg r)
			    | Slot slot => (* Can only be slot *)
			     let
			       val (state,loc) = get_free_loc' false state (Full(c,x))
			       val state = mov_var_to_loc state x loc
			     in state
			     end
			    | _ => error "Bad outarg location")
		       | (Avail(Full(c,x)),false) => 
			(case S.get_var64_loc state x
			   of FSlot slot => 
			     let
			       val (state,loc) = get_free_loc64 true state (Full(c,x))
			       val state = mov_var_to_loc64 state x loc
			     in state
			     end
			    | _ => error "Bad outarg64 location")
		       | (Reserved _,_) => (warn "popping reserved outarg";S.print_stack state;state)
		       | _ => state)
	       in loop(outargs,state)
	       end)
	val state = loop(outargs,state)
	val state = S.pop_outargs state
	val state = if size > 0w0 then S.emit state (Tal.ArithBin(Tal.Sub,Tal.Reg Tal.Esp,Tal.Immed size)) else state
      in state
      end

    fun alloc_outargs state num32 num64 = S.alloc_outarg_space state ((LO.map0count (fn _ => true) num32)@(LO.map0count (fn _ => false) num64))
    fun alloc_outargs' state l = S.alloc_outarg_space state l

    fun alloc_inargs state num32 num64 = S.alloc_inarg_space state (num32+1) num64

    fun stat2type status = 
      case status
	of Avail(Full(c,x)) => c
	 | Reserved (Full(c,x)) => (warn "Getting type from reserved slot";c)
	 | _ => error "No type info in status"

    fun vars_in_loc state loc = 
      (case loc
	 of Dual(slot,r) => 
	   (case (get_slot_rcont state slot,get_reg_rcont state r)
	      of (Full(c1,x1),Full(c2,x2)) =>
		if Name.eq_var (x1,x2) then 
		  [x1]
		else 
		  [x1,x2]
	       | (Full (c1,x1),_) => [x1]
	       | (_,Full(c2,x2)) => [x2]
	       | _ => [])
	  | _ => 
	      (case get_loc_rcont state loc
		 of Full (c,x) => [x]
		  | Junk => []))

    (* Empty out a location, moving any vars to a new location.
     * Do not assume that the paired locations (Duals) have the same
     * contents.  Reserves.
     *)
    fun empty_loc state loc = 
      let
	(* For every var in loc, find the locations
	 * besides loc in which the variable resides.
	 * if none, move it to a fresh temp slot.  
	 * Otherwise, restrict it to the remaining locations.
	 *)
	fun spill_var (x,state) = 
	  (case loc_diff (S.get_var32_loc state x) loc 
	     of [] => 
	       let
		 val (state,newloc) = get_free_loc' true state Junk
		 val state = mov_var_to_loc state x newloc
(* XXX		 val state = release_loc state newloc *)
	       in state
	       end
	      | [loc2] => mov_var_to_loc state x loc2
	      | _ => error "Not possible: x is in loc, and |loc| <= 2")

(* XXX: should mark_loc precede loop? *)
	val state = List.foldl spill_var state (vars_in_loc state loc)
	val state = mark_loc Reserved state loc
      in state 
      end

    fun empty_loc64 state loc = 
      let
	val state = 
	  (case loc
	     of FDual(slot,r) => error "No 64 bit Duals yet"
	      | _ => 
	       (case get_loc64_stat state loc
		  of Avail(Full (c,x)) => 
		    let
		      val (state,newloc) = get_free_loc64 true state Junk
		      val state = mov_var_to_loc64 state x newloc
(* XXX		      val state = release_loc64 state newloc *)
		    in state
		    end
		   | Avail Junk => state
		   | _ => error "Trying to spill reserved location"))
	val state = mark_loc64 Reserved state loc
      in state 
      end
	  
    fun empty_locs state locs = List.foldl (fn (loc,state) => empty_loc state loc) state locs
    fun empty_loc64s state locs = List.foldl (fn (loc,state) => empty_loc64 state loc) state locs

    fun adjust_inargs state num32 num64 = 
      let
	val state = S.grow_inargs state num32 num64
	val cur32 = S.get_inarg32_count state
	val cur64 = S.get_inarg64_count state
	fun clear64 state i = 
	  if i < cur64 then clear64 (empty_loc64 state (FSlot(S.inarg64_slot_i state i))) (i + 1)
	  else state
	(* clear out everything from num64 to cur64 *)
	val state = clear64 state num64

	(* How many 32 bit slots are we moving to the 64 bit area (maybe negative) *)
	val shift32s = 2 * (num64 - cur64)
	fun clear32 state i = 
	  if i < shift32s then clear32 (empty_loc state (Slot (S.inarg_slot_i state i))) (i + 1)
	  else state
	val state = clear32 state 0

	val new32 = cur32 + 2*(cur64 - num64)  (* grow_inargs guarantees >= 0*)
	val state = S.adjust_inargs state new32 num64
      in state
      end

    (* For every variable only in the old state,
     * add it to the stack/reg description and mark its location.
     *)
    fun compensate_absent32 oldstate (x,loc,(state,changed)) = 
      (case S.get_var32_loc' state x
	 of NONE => 
	   let
	     val () = debugdo (7,fn () => (print "Compensating absent var ";PpLil.pp_var x;print "\n"))
	     val state = empty_loc state loc
	     val status = get_loc_stat oldstate loc
	     val state = S.set_var32_loc state x loc
	     val state = set_loc_stat state loc status
(* XXX	     val state = release_loc state loc *)
	     val state = S.comment state 5 ("Compensating absent var "^(Name.var2string x))
	   in (state,true)
	   end
	  | SOME _ => (state,changed))

    (* For every variable only in the old state,
     * add it to the stack/reg description and mark its location.
     *)
    fun compensate_absent64 oldstate (x,loc,(state,changed)) = 
      (case S.get_var64_loc' state x
	 of NONE => 
	   let
	     val () = debugdo (7,fn () => (print "Compensating absent fvar ";PpLil.pp_var x;print "\n"))
	     val state = empty_loc64 state loc
	     val status = get_loc64_stat oldstate loc
	     val state = S.set_var64_loc state x loc
	     val state = set_loc64_stat state loc status
(* XXX	     val state = release_loc64 state loc *)
	     val state = S.comment state 5 ("Compensating absent fvar "^(Name.var2string x))
	   in (state,true)
	   end
	  | SOME _ => (state,changed))

     (* For variables in both, reconcile the locations.
     *)
    fun compensate32 oldstate (x,loc,(state,changed)) = 
      (case S.get_var32_loc' state x
	 of NONE => (state,changed)
	  | SOME qloc => 
	   (case loc_diff loc qloc
	      of [] => (state,changed)
	       | locs => 
		let
		  val () = debugdo (7,fn () => (print "Compensating var ";PpLil.pp_var x;print "\n"))
		  val state = empty_locs state locs
		  val () = debugdo (8,fn () => (print "Emptied state is \n";S.print_state state;print "\n"))
		  val state = mov_var_to_loc state x loc
(* XXX		  val state = release_locs state locs *)
		  val state = S.comment state 5 ("Compensating var "^(Name.var2string x))
		  val () = debugdo (8,fn () => (print "Compensated state is \n";S.print_state state;print "\n"))
		in (state,true)
		end))


     (* For variables in both, reconcile the locations.
     *)
    fun compensate64 oldstate (x,loc,(state,changed)) = 
      (case S.get_var64_loc' state x
	 of NONE => (state,changed)
	  | SOME qloc => 
	   (case loc64_diff loc qloc
	      of [] => (state,changed)
	       | locs => 
		let
		  val () = debugdo (7,fn () => (print "Compensating fvar ";PpLil.pp_var x;print "\n"))
		  val state = empty_loc64s state locs
		  val state = mov_var_to_loc64 state x loc
(* XXX		  val state = release_locs64 state locs *)
		  val state = S.comment state 5 ("Compensating fvar "^(Name.var2string x))
		in (state,true)
		end))

    fun compensate compensater get_vars coercer coercee = VarMap.foldli (compensater coercer) (coercee,false) (get_vars coercer)

    fun compensate_vars compensate_absent compensater get_vars coercer coercee =
      let
	fun loop (coercee,changed) = 
	  if changed then loop (compensate compensater get_vars coercer coercee)
	  else coercee

	val coercee = loop (coercee,true)
	val (coercee,changed) = compensate compensate_absent get_vars coercer coercee
	val coercee = loop (coercee,changed)
      in coercee
      end

    fun compensate_vars32 coercer coercee = compensate_vars compensate_absent32 compensate32 #vars32 coercer coercee
    fun compensate_vars64 coercer coercee = compensate_vars compensate_absent64 compensate64 #vars64 coercer coercee

    fun match_state (coercer : S.state) (coercee : S.state) = 
      let
	val () = debugdo (7,fn () => (print "Matching states\n";
				      print "Coercer is :\n";S.print_state coercer;
				      print "Coercee is :\n";S.print_state coercee;
				      print "\n"))
	val coercee = S.comment coercee 1 "End compensation code"
	val coercee = S.comment_stack coercee 8
	val () = debugdo (5,fn () => sanity_check coercer)
	val () = debugdo (5,fn () => sanity_check coercee)  
	val coercee = S.copy_state coercee
	val coercee = adjust_inargs coercee (S.get_inarg32_count coercer) (S.get_inarg64_count coercer)
	val coercee = S.grow_state coercer coercee
	val coercee = compensate_vars32 coercer coercee
	val coercee = compensate_vars64 coercer coercee
	val () = debugdo (7,fn () => (print "Matched state is :\n";S.print_state coercee;
				      print "\n"))
	val () = debugdo (5,fn () => sanity_check coercee)
	val coercee = S.comment_stack coercee 8
	val coercee = S.comment coercee 1 "Begin compensation code"
      in coercee
      end


    fun revert_state (coercer : S.state) (coercee : S.state) = 
      let
	val () = debugdo (7,fn () => (print "Reverting states\n";
				      print "Coercer is :\n";S.print_state coercer;
				      print "Coercee is :\n";S.print_state coercee))

	val () = debugdo (5,fn () => sanity_check coercer)
	val () = debugdo (5,fn () => sanity_check coercee)
	val coercee = S.copy_state coercee

	val coercee = S.set_registers coercee (#registers coercer)
	val coercee = S.set_vars32 coercee (#vars32 coercer)
	val coercee = S.set_vars64 coercee (#vars64 coercer)
	val coercee = S.set_stack coercee (#stack coercer)

	val () = debugdo (7,fn () => (print "Coerced state is :\n";S.print_state coercee;
				      print "\n"))
	val () = debugdo (5,fn () => sanity_check coercee)
      in coercee
      end


    fun reserve_next_arg32 state = 
      let
	val (state,slot) = S.alloc_outarg_slot state
	val () = 
	  debugdo (7,fn () => (print "Reserving outarg: ";
			       SD.print_slot slot;
			       print "\n"))
	val state = S.set_sstat state slot (Reserved Junk)
      in (state,S.slot2genop state slot)
      end

    fun reserve_next_arg64 (state : S.state) : (S.state * Tal.genop) = 
      let
	val (state,slot) = S.alloc_outarg64_slot state
	val () = 
	  debugdo (7,fn () => (print "Reserving outarg64: ";
			       SD.print_slot slot;
			       print "\n"))
	val state = S.set_sstat state slot (Reserved Junk)
      in (state,S.slot2genop state slot)
      end




    fun reserve_inarg32 state i = 
      let
	val slot = S.inarg_slot_i state i
	val state = empty_loc state (Slot slot)  (* Reserves *)
      in (state,S.slot2genop state slot)
      end

    fun reserve_inarg64 (state : S.state) (i : int) : (S.state * Tal.genop) = 
      let
	val slot = S.inarg64_slot_i state i
	val () = 
	  debugdo (7,fn () => (print "Reserving inarg64 # ";print (Int.toString i);print " : ";
			       SD.print_slot slot;
			       print "\n"))

	val state = empty_loc64 state (FSlot slot)  (* Reserves *)
      in (state,S.slot2genop state slot)
      end


    fun reserve_last_inarg32 state = 
      let
	val count = S.get_inarg32_count state
	val last_idx = count - 1
      in
	reserve_inarg32 state last_idx
      end


    (* Emit code to place formal in it's chosen location. *)
    fun bind_formal32 state x (c : con) i = 
      let
	val slot = S.inarg_slot_i state i
	val state = 
	  (case S.get_var32_loc' state x
	     of SOME loc => 
	       let
		 (* If variable isn't already in the slot, spill the slot *)
		 val state = if sub_loc loc (Slot slot) then state else empty_loc state (Slot slot)
		 (* Emit a mov from the inarg slot to the location (no-op if already there) *)
		 val state = mov_var_to_loc state x (Slot slot)
	       in state 
	       end
	      | NONE => state)
      in state
      end

    fun bind_formal64 (state : S.state) (x : Lil.var) (c :con) (i : int) : S.state = 
      let
	val slot = S.inarg64_slot_i state i
	val state = 
	  (case S.get_var64_loc' state x
	     of SOME loc => 
	       let
		 (* If variable isn't already in the slot, spill the slot *)
		 val state = if eq_loc64 loc (FSlot slot) then state else empty_loc64 state (FSlot slot)
		 (* Emit a mov from the inarg slot to the location (no-op if already there) *)
		 val state = mov_var_to_loc64 state x (FSlot slot)
	       in state 
	       end
	      | NONE => state)
      in state
      end


    fun define_var_from_reg state x r = 
      let
	val state = load_var state r x (LilCon (LD.T.void()))
	val (state,d) = define_var32 false state x 
	val state = release_temp state d
	val state = reserve_reg state r
      in state
      end
    fun var32_used state v = isSome(S.get_var32_loc' state v)
    fun var64_used state v = isSome(S.get_var64_loc' state v)

    fun shift_temp (state : S.state) (gop1 : Tal.genop) (gop2 : Tal.genop) : S.state = 
      if eq_gop gop1 gop2 then state
      else
	let
	  val loc1 = genop2loc state gop1
	  val loc2 = genop2loc state gop2
	  val state = empty_loc state loc2  (* Reserves *)
	  val state = loc_mov state loc1 loc2
	  val state = release_temp state gop1
	in state
	end

    fun loc32_2type state loc = 
      (case get_loc_stat state loc
	 of Avail (Full(c,x)) => c
	  | Reserved(Full(c,x)) => c
	  | _ => error "no info for var")

    fun loc64_2type state loc = 
      (case get_loc64_stat state loc
	 of Avail (Full(c,x)) => c
	  | Reserved(Full(c,x)) => c
	  | _ => error "no info for var")

    fun get_live_vars32 (state : S.state) = map #1 (VarMap.listItemsi (#vars32 state))
    fun get_live_vars64 (state : S.state) = map #1 (VarMap.listItemsi (#vars64 state))

    fun wrapper (s : string) (get_state : 'res -> S.state) (doer : unit -> 'res) : 'res = 
      if !debug then 
	let
	  val () = chat 7 ("Starting "^s)
	  val res = doer()
	  val state = get_state res
	  val () = chat 7 ("Finishing "^s)
	  val () = sanity_check' true state
	in res
	end
      else doer()

    fun id (s : S.state) : S.state = s
    fun fst (s : S.state,_ : 'a) : S.state = s
    fun wrap1 s f state = wrapper s id (fn () => f state)
    fun wrap2 s f state a = wrapper s id (fn () => f state a)
    fun wrap3 s f state a b = wrapper s id (fn () => f state a b)
    fun wrap4 s f state a b c = wrapper s id (fn () => f state a b c)
    fun wrap_pair1 s f state = wrapper s fst (fn () => f state)
    fun wrap_pair2 s f state a = wrapper s fst (fn () => f state a)
    fun wrap_pair3 s f state a b = wrapper s fst (fn () => f state a b)
    fun wrap_pair4 s f state a b c = wrapper s fst (fn () => f state a b c)
    fun wrap_second_pair1 s f state = wrapper s fst (fn () => f state)
    fun wrap_second_pair2 s f a state = wrapper s fst (fn () => f a state)
    fun wrap_second_pair3 s f a state b = wrapper s fst (fn () => f a state b)
    fun wrap_second_pair4 s f a state b c = wrapper s fst (fn () => f a state b c)

    (* Exports *)
    type state = S.state

    val comment = S.comment
    val comment_stack = S.comment_stack
    val print_stack = S.print_stack
    val print_state = S.print_state

    val emit = S.emit
    val emitn = S.emitn
    val emit_block = S.emit_block

    val release_reg = release_reg
    val reserve_reg = wrap2 "reserve_reg" reserve_reg

    val reserve_inarg32 = wrap_second_pair2 "reserve_inarg32" reserve_inarg32
    val reserve_inarg64 = wrap_second_pair2 "reserve_inarg64" reserve_inarg64
    val reserve_next_arg32 = wrap_second_pair1 "reserve_next_arg32" reserve_next_arg32
    val reserve_next_arg64 = wrap_second_pair1 "reserve_next_arg64" reserve_next_arg64

    val reserve_temp = wrap_second_pair2 "reserve_temp" reserve_temp

    val reserve_for_var = wrap_second_pair4 "reserve_for_var" reserve_for_var

    val init_reg = wrap3 "init_reg" init_reg
    val init_temp = wrap3 "init_temp" init_temp
    val init_var32 = wrap4 "init_var32" init_var32
    val init_var64 = wrap4 "init_var64" init_var64

    val typeof_reg = S.typeof_reg
    val typeof_reg' = S.typeof_reg'
    val typeof_stack = S.typeof_stack
    val typeof_state = S.typeof_state
    val typeof_state' = S.typeof_state'

    val get_frame_size = S.get_frame_size
    val get_inarg_size = S.get_inarg_size
    val get_inarg32_size = S.get_inarg32_size
    val get_inarg64_size = S.get_inarg64_size
    val get_temp_size = S.get_temp_size
    val get_temp64_size = S.get_temp64_size
    val get_outarg_size = S.get_outarg_size

    val load_var = wrap4 "load_var" load_var

    val new_state = S.new_state
    val new_state' = S.new_state'
    val copy_state = S.copy_state

    val pop_outargs = wrap1 "pop_outargs" pop_outargs

    val alloc_outargs = wrap3 "alloc_outargs" alloc_outargs
    val alloc_inargs = wrap3 "alloc_inargs" alloc_inargs
    val adjust_inargs = wrap3 "adjust_inargs" adjust_inargs

    val alloc_outargs' = wrap2 "alloc_outargs'" alloc_outargs'

    val reserve_temp_slot = wrap_pair1 "reserve_temp_slot" reserve_temp_slot
    val reserve_temp_reg = wrap_pair1 "reserve_temp_reg" reserve_temp_reg
    val reserve_temp_any = wrap_pair1 "reserve_temp_any" reserve_temp_any

    val bind_formal64 = wrap4 "bind_formal64" bind_formal64
    val bind_formal32 = wrap4 "bind_formal32" bind_formal32

    val add_blocks_from_state = add_blocks_from_state
    val add_blocks_before_from_state = add_blocks_before_from_state
    val get_blocks = S.get_blocks
    val flush_blocks = S.flush_blocks
    val reset_state = S.reset_state
    val define_var_from_reg = wrap3 "define_var_from_reg" define_var_from_reg
    val kill_var32 = wrap2 "kill_var32" kill_var32
    val kill_var64 = wrap2 "kill_var64" kill_var64
    val define_var32 = wrap_second_pair3 "define_var32" define_var32
    val define_var64 = wrap_pair2 "define_var64" define_var64

    val shift_temp = wrap3 "shift_temp" shift_temp

    exception FrameTooSmall = SD.FrameTooSmall


    val free_temp_reg = get_free_temp_reg
    val free_temp_any = get_free_temp_any
    val free_temp = get_free_temp
    val free_reg = make_reg_available
    val reserve_one_of_regs = reserve_one_of_regs
    val get_frame_elt_count = S.get_frame_elt_count
    val get_outarg_count = S.get_outarg_count
    val setup_temp64s = S.setup_temp64s
    val grow_state = S.grow_state
end  (* LilToTal *)