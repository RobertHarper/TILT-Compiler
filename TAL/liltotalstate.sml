structure LilToTalState :> LILTOTALSTATE =
  struct
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet
    structure LU = LilUtil

    fun error s = Util.error "liltotalstate.sml" s    

    fun warn s = (print "WARNING: ";print s;print "\n")

    val debug = Stats.ff "LilToTalStateDebug"

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


    val rcount = 7
    fun ridx r = 
      (case r
	 of Tal.Eax => 0
	  | Tal.Ebx => 1
	  | Tal.Ecx => 2
	  | Tal.Edx => 3
	  | Tal.Esi => 4
	  | Tal.Edi => 5
	  | Tal.Ebp => 6
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
	  | 6 => Tal.Ebp
	  | 7 => error "Esp not a GP register"
	  | _ => error "Bad register index")

    datatype rcont = 
      Junk  (* Empty or reserved for temp (not live across calls).  
	     * If we want to allow temps live across calls, must allow 
	     * a type here so that we can get the correct machine state. *)
      | Full of (Tal.con * Lil.var)  (* Full (c,x) holds variable x : c *)
      

    (* Reserved registers are locked down (usually because they have
     * been allocated, but not yet initialized).  Available registers
     * are available for spilling or use (depending on the rcont) *)
    datatype rstat = Reserved of rcont | Avail of rcont

    datatype slot = 
      Temp of int     (* ith temp *)
      | Inarg of int  (* ith inarg/tailslot *)
      | Outarg of int (* ith outarg *)

    datatype loc32 = 
      Reg of Tal.reg
      | Slot of slot
      | Dual of slot * Tal.reg

    datatype loc64 = 
      Freg of int (* ith fpstack slot *)
      | FSlot of slot 
      | FDual of slot * int

    type sstat = rstat * bool  (* true => 32 bit *)


    fun print_rcont c = 
      (case c
	 of Junk => print "junk"
	  | Full(c,x) => (print "Contains ";
			  PpLil.pp_var x))

    fun print_rstat r = 
      (case r
	 of Avail c => (print "Avail (";print_rcont c;print ")")
	  | Reserved c => (print "Reserved (";print_rcont c;print ")"))

    fun eq_reg r1 r2 = r1 = r2

    fun eq_slot s1 s2 = 
      (case (s1,s2) 
	 of (Temp i,Temp i') => i = i'
	  | (Inarg i,Inarg i') => i = i'
	  | (Outarg i,Outarg i') => i = i'
	  | _ => false)

    fun eq_gop gop1 gop2 =
      (case (gop1,gop2)
	 of (Tal.Reg r,Tal.Reg r') => r = r'
	  | (Tal.Prjr ((Tal.Esp,[]),i,NONE), Tal.Prjr ((Tal.Esp,[]),i',NONE)) => i = i'
	  | _ => false)


    fun sstat_size ss = 
      (case ss
	 of (_,true) => 1
	  | (_,false) => 2)

    fun sstat_list_size slist = List.foldl (fn (ss,acc) => sstat_size ss + acc) 0 slist

    structure SD = 
      struct
	(* Invariant: all junk slots are 32 bits.
	 * lists represent stacks, bottom to top,
	 * with offsets indicating offsets from the bottom.
	 *)
	(* generate_functional_updates (NONE) (SOME "stackdesc") ["inargs","outargs","temps"]; *)
	type stackdesc = 
	  {inargs : sstat list,
	   outargs : sstat list,
	   temps : sstat list}

	fun empty_stackdesc () = 
	  {inargs = [],
	   outargs = [],
	   temps = []}

	fun set_inargs ({ inargs = _  , outargs , temps } :stackdesc) inargs =
	  {
	   inargs = inargs,
	   outargs = outargs,
	   temps = temps
	   }
	fun set_outargs ({ inargs , outargs = _  , temps } :stackdesc) outargs =
	  {
	   inargs = inargs,
	   outargs = outargs,
	   temps = temps
	   }
	fun set_temps ({ inargs , outargs , temps = _  } :stackdesc) temps =
	  {
	   inargs = inargs,
	   outargs = outargs,
	   temps = temps
	   }

	fun print_stack ({inargs, outargs, temps}) = 
	  let
	    fun printslot (r,is32) = 
	      (if is32 then () else print "   (cont)   \n";
	       print_rstat r;print "\n")
	    val () = print "****Stack****\n"
	    val () = print "----Inargs---\n"
	    val () = app printslot inargs
	    val () = print "----Temps----\n"
	    val () = app printslot temps
	    val () = print "----Outargs--\n"
	    val () = app printslot outargs
	    val () = print "*****End*****\n"
	  in ()
	  end

	fun junk_sstat i =
	  let
	    fun loop (i,acc) =
	      if i = 0 then acc
	      else loop (i-1,(Avail Junk,true)::acc)
	  in loop (i,[])
	  end

	fun empty_stackdesc' (inarg_size,temp_size,outarg_size) = 
	  {
	   inargs = junk_sstat (LU.w2i inarg_size),
	   outargs = junk_sstat (LU.w2i outarg_size),
	   temps =  junk_sstat (LU.w2i temp_size)
	   }
	  
	local
	  (* Set the ith slot, adding slots as necessary.
	   * I think that alloc_slot should guarantee that this
	   * never needs to add slots.
	   *)
	  fun set_sstat_slot slist i s = 
	    (case (slist,i) 
	       of ([],_) => set_sstat_slot (junk_sstat (i + 1)) i s
		| (ss::slist,0) => 
		 (case #2 ss
		    of true => s::slist
		     | false => s::(Avail Junk,true)::slist)
		| (ss::slist,i) => 
		    (case #2 ss
		       of true => set_sstat_slot slist (i-1) s
			| false => set_sstat_slot ((Avail Junk,true)::slist) (i-1) s))
	       
	       
	  fun set_xxx_slot (get_slist,set_slist) stackdesc i s = 
	    let
	      val slist = get_slist stackdesc
	      val slist = set_sstat_slot slist i s
	      val stackdesc = set_slist stackdesc slist
	    in stackdesc
	    end

	  val set_temp_slot = set_xxx_slot (#temps,set_temps)
	  val set_inarg_slot = set_xxx_slot (#inargs,set_inargs)
	  val set_outarg_slot = set_xxx_slot (#outargs,set_outargs)

	  fun get_sstat_slist slist i = 
	    (case (slist,i)
	       of (ss::_,0) => ss
		| ([],_) => (Avail(Junk),true)
		| (ss::slist,i) => get_sstat_slist slist (i-1))
	in

	  (* Overwrite a slot with new info *)    
	  fun set_sstat (stackdesc : stackdesc) (slot : slot) (s : sstat) : stackdesc = 
	    (case slot
	       of Temp i => set_temp_slot stackdesc i s
		| Inarg i => set_inarg_slot stackdesc i s
		| Outarg i => set_outarg_slot stackdesc i s)

	  (* Get slot info *)    
	  fun get_sstat (stackdesc : stackdesc) (slot : slot) : sstat = 
	    (case slot
	       of Temp i => get_sstat_slist (#temps stackdesc) i 
		| Inarg i => get_sstat_slist (#inargs stackdesc) i 
		| Outarg i => get_sstat_slist (#outargs stackdesc) i)
	end

	local

	  fun alloc_slist_slot slist = 
	    let
	      fun loop (sl,i) =
		(case sl
		   of [] => (slist@[(Avail Junk,true)],i)
		    | (Avail Junk,true)::sl => (slist,i)
		    | (_,true)::sl => loop (sl,i+1)
		    | (_,false)::sl => loop (sl,i+2))
	    in loop (slist,0)
	    end

	  fun alloc_slot (get_slist,set_slist,constr) (sd : stackdesc) : stackdesc * slot = 
	    let
	      val slist = get_slist sd
	      val (slist,idx) = alloc_slist_slot slist
	      val sd = set_slist sd slist
	    in (sd,constr idx)
	    end
	in
	  val alloc_temp_slot = alloc_slot (#temps,set_temps,Temp)
	  val alloc_inarg_slot = alloc_slot (#inargs,set_inargs,Inarg)
	  val alloc_outarg_slot = alloc_slot (#outargs,set_outargs,Outarg)
	end

	(* Translate offset from bottom of frame region
	 * into offset from top of stack (stack ptr) 
	 *)
	fun slot2offset ({inargs,outargs,temps} : stackdesc) (slot : slot) : Tal.int32 = 
	  let
	  in
	    (case slot
	       of Inarg i =>
		 let
		   val prefix = (List.length outargs) + (List.length temps) + (List.length inargs)
		 in LU.i2w (prefix - (i + 1))
		 end
		| Temp i => 
		 let
		   val prefix = (List.length outargs) + (List.length temps) 
		 in LU.i2w (prefix - (i + 1))
		 end
		| Outarg i =>
		 let
		   val prefix = (List.length outargs) 
		 in  LU.i2w (prefix - (i + 1))
		 end)
	  end
	(* Translate offset from top of stack (stack ptr)
	 * into offset from bottom of a specific area.
	 *)
	fun offset2slot (sd as {inargs,outargs,temps} : stackdesc) (w : Tal.int32) : slot = 
	  let
	    val i = LU.w2i w
	    val ol = List.length outargs
	    val tl = List.length temps
	    val il = List.length inargs
	  in 
	    if i < il then Inarg (il - (i + 1))
	    else if i < (il + tl) then Temp ((il + tl) - (i + 1))
	    else if i < (il + tl + ol) then Outarg ((il + tl + ol) - (i + 1))
            else (print "Offset ";print (Int.toString i);
		  print " too large\n";
		  print_stack sd;
		  error "Bad offset")
	  end

	fun typeof_sd sd =
	  (case sd
	     of (Reserved (Full(c,x)),_) => c
	      | (Avail (Full(c,x)),_) => c
	      | (_,true) => Tal.pcjunk4
	      | (_,false) => Tal.pcjunk 0w8)

	fun typeof_slist slist =
	  let
	    fun loop (slist,acc) = 
	      (case slist
		 of [] => acc
		  | sd::slist => loop(slist,Tal.ccons (typeof_sd sd) acc))
	  in loop (slist,Tal.cempty)
	  end

	fun typeof_stack ({inargs,outargs,temps} : stackdesc) : Tal.con = 
	  let
	    val bot = typeof_slist inargs
	    val mid = typeof_slist temps
	    val top = typeof_slist outargs
	  in Tal.cappend top (Tal.cappend mid bot)
	  end

	fun get_frame_size ({inargs,outargs,temps} : stackdesc) : Tal.int32 = LU.i2w((sstat_list_size outargs) + (sstat_list_size temps))
	fun get_inarg_size ({inargs,outargs,temps} : stackdesc) : Tal.int32 = LU.i2w(sstat_list_size inargs)
	fun get_temp_size ({inargs,outargs,temps} : stackdesc) : Tal.int32 = LU.i2w(sstat_list_size temps)
	fun get_outarg_size ({inargs,outargs,temps} : stackdesc) : Tal.int32 = LU.i2w(sstat_list_size outargs)

(*	fun pop_frame ({inargs,outargs,temps} : stackdesc) : stackdesc = {inargs = inargs,outargs = [],temps = []}*)

	fun pop_outargs (sd : stackdesc) = (set_outargs sd [],#outargs sd)
	fun alloc_outarg_space (sd : stackdesc) i = (set_outargs sd (junk_sstat i))
	fun alloc_inarg_space (sd : stackdesc) i = (set_inargs sd (junk_sstat i))
	    
      end (* structure SD *)

    structure ID = 
      struct
	(* generate_functional_updates (NONE) (SOME "instrdesc") ["blocks","instrs"]; *)
	type instrdesc = 
	  {blocks : Tal.code_block list,
	   instrs : Tal.instruction list}  

	fun empty_instrdesc () = 
	  {blocks = [],
	   instrs = []}

	fun set_blocks ({ blocks = _  , instrs } :instrdesc) blocks =
	  {
	   blocks = blocks,
	   instrs = instrs
	   }
	fun set_instrs ({ blocks , instrs = _  } :instrdesc) instrs =
	  {
	   blocks = blocks,
	   instrs = instrs
	   }

	fun emit (id : instrdesc) (instr : Tal.instruction) = 
	  set_instrs id (instr :: (#instrs id))

	fun get_blocks (id : instrdesc) = 
	  let
	    val () = (case #instrs id
			of [] => ()
			 | _ => warn "Unclosed block1")
	  in #blocks id
	  end

	fun flush_blocks (id : instrdesc) = 
	  let
	    val () = (case #instrs id
			of [] => ()
			 | _ => warn "Unclosed block1 flushed")
	    val id = set_blocks id []
	    val id = set_instrs id []
	  in id
	  end

	fun add_blocks (id : instrdesc) (blocks : Tal.code_block list): instrdesc = 
	  set_blocks id ((#blocks id) @ blocks)

	fun emit_block (id : instrdesc) l copt = 
	  let
	    val instrs = #instrs id
	    val block = (l,copt,Vector.fromList instrs)
	    val blocks = block::(#blocks id)
	    val id = set_blocks id blocks
	    val id = set_instrs id []
	  in id
	  end

      end (* structure ID *)

    structure S = 
      struct
	(*generate_functional_updates (NONE) (SOME "state") ["registers","stack","vars32","vars64","instr"];*)
	type state = 
	  {registers : rstat array,
	   stack : SD.stackdesc,
	   vars32 : loc32 VarMap.map,
	   vars64 : loc64 VarMap.map,
	   instr : ID.instrdesc
	   }

	fun set_registers ({ registers = _  , stack , vars32 , vars64 , instr } :state) registers =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr
	   }
	fun set_stack ({ registers , stack = _  , vars32 , vars64 , instr } :state) stack =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr
	   }
	fun set_vars32 ({ registers , stack , vars32 = _  , vars64 , instr } :state) vars32 =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr
	   }
	fun set_vars64 ({ registers , stack , vars32 , vars64 = _  , instr } :state) vars64 =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr
	   }
	fun set_instr ({ registers , stack , vars32 , vars64 , instr = _  } :state) instr =
	  {
	   registers = registers,
	   stack = stack,
	   vars32 = vars32,
	   vars64 = vars64,
	   instr = instr
	   }

	fun new_state () = 
	  {registers = Array.array(rcount,Avail Junk),
	   stack = SD.empty_stackdesc (),
	   vars32 = VarMap.empty,
	   vars64 = VarMap.empty,
	   instr = ID.empty_instrdesc ()}

	fun new_state' frameargs = 
	  {registers = Array.array(rcount,Avail Junk),
	   stack = SD.empty_stackdesc' frameargs,
	   vars32 = VarMap.empty,
	   vars64 = VarMap.empty,
	   instr = ID.empty_instrdesc ()}

	fun emit (state : state) (instr : Tal.instruction) = set_instr state (ID.emit (#instr state) instr)

	fun get_var32_loc' (state : state) (v : Lil.var) : loc32 option = VarMap.find (#vars32 state,v)
	fun get_var32_loc (state : state) (v : Lil.var) : loc32 = 
	  (case get_var32_loc' state v
	     of SOME loc => loc
	      | NONE => error ("No location for variable " ^ (Name.var2string v)))
	     
	fun set_var32_loc state v loc = set_vars32 state (VarMap.insert (#vars32 state,v,loc))
	  
	fun get_rstat (state : state) (r : Tal.reg) : rstat = 
	  let
	    val regs = #registers state
	    val rstat = Array.sub(regs,ridx r)
	  in rstat
	  end

	fun set_rstat (state : state) (r : Tal.reg) (s : rstat) : state = 
	  let
	    val regs = #registers state
	    val rstat = Array.update(regs,ridx r,s)
	  in state
	  end

	fun set_sstat (state : state) (slot : slot) (s : sstat) : state = 
	  let
	    val stack = #stack state
	    val stack = SD.set_sstat stack slot s
	    val state = set_stack state stack
	  in state
	  end

	fun get_sstat (state : state) (slot : slot) : sstat = 
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
	  val alloc_inarg_slot = alloc_slot SD.alloc_inarg_slot
	  val alloc_outarg_slot = alloc_slot SD.alloc_outarg_slot
	end

	fun slot2offset ({stack, ...} : state) slot = SD.slot2offset stack slot
	fun offset2slot ({stack, ...} : state) w = SD.offset2slot stack w

	fun typeof_stack (state : state) : Tal.con = SD.typeof_stack (#stack state) 

	fun typeof_reg state r = 
	  (case r 
	     of Tal.Esp => SOME (Tal.csptr (typeof_stack state))
	      | _ => 
	       (case get_rstat state r
		  of Reserved (Full(c,x)) => SOME c
		   | Avail(Full(c,x)) => SOME c
		   | _ => NONE))



	fun get_frame_size (state : state) : Tal.int32 = SD.get_frame_size (#stack state)
	fun get_inarg_size (state : state) : Tal.int32 = SD.get_inarg_size (#stack state)
	fun get_temp_size (state : state) : Tal.int32 = SD.get_temp_size (#stack state)
	fun get_outarg_size (state : state) : Tal.int32 = SD.get_outarg_size (#stack state)

	fun get_blocks (state : state) : Tal.code_block list = ID.get_blocks (#instr state)
	fun flush_blocks (state : state) : state = set_instr state (ID.flush_blocks (#instr state))
	fun add_blocks (state : state) (blocks : Tal.code_block list) : state = 
	  set_instr state (ID.add_blocks (#instr state) blocks)

	fun emit_block (state : state) (l : Tal.label) (copt : Tal.con option) = 
	  set_instr state (ID.emit_block (#instr state) l copt)

	fun pop_outargs state = 
	  let
	    val (stack,outargs) = SD.pop_outargs (#stack state)
	  in (set_stack state stack,outargs)
	  end
	fun alloc_outarg_space state i = set_stack state (SD.alloc_outarg_space (#stack state) i)
	fun alloc_inarg_space state i = set_stack state (SD.alloc_inarg_space (#stack state) i)
      
      end (* structure S *)


    fun slot2genop state slot =
      Tal.Prjr ((Tal.Esp,[]),S.slot2offset state slot,NONE)

    fun genop2slot state gop = 
      (case gop
	 of Tal.Prjr ((Tal.Esp,[]),w,NONE) => S.offset2slot state w
	  | _ => error "Not a slot")

    fun genop2loc state gop = 
      (case gop 
	 of Tal.Prjr ((Tal.Esp,[]),w,NONE) => Slot(S.offset2slot state w)
	  | Tal.Reg r => Reg r
	  | _ => error "Not a location genop")

    fun loc2genop state loc = 
      (case loc 
	 of Dual (s,r) => Tal.Reg r
	  | Reg r => Tal.Reg r
	  | Slot slot => slot2genop state slot)


    fun release_reg state r = 
      (case S.get_rstat state r
	 of Reserved rs' => S.set_rstat state r (Avail rs')
	  | _ => state)


    fun release_slot state slot = 
      (case S.get_sstat state slot
	 of (Reserved rs',is32) => S.set_sstat state slot (Avail rs',is32)
	  | _ => state)

    fun release_temp state gop = 
      let
	val loc = genop2loc state gop
      in 
	case loc 
	  of Reg r => release_reg state r
	   | Slot slot  => release_slot state slot
	   | Dual (slot,r) => 
	    let
	      val state = release_reg state r
	      val state = release_slot state slot
	    in state
	    end
      end

   (* Pick a free register for allocation if one exists. *)
    fun get_free_reg (state : S. state) = 
      let
	fun folder (idx,rstat,opt) =
	  (case rstat
	     of Avail Junk => SOME (idxr idx)
	      | _ => opt)
      in Array.foldli folder NONE (#registers state,0,NONE)
      end

    (* This could benefit from a heuristic. *)
    fun choose_spill_reg (state : S.state) = 
      let
	fun folder (idx,rstat,opt) =
	  (case rstat
	     of Avail(Full _) => SOME (idxr idx)
	      | _ => opt)
      in case Array.foldli folder NONE (#registers state,0,NONE)
	   of SOME r => r
	    | NONE => error "All registers reserved: too many reg temps."
      end

    (* Finds a temp slot for the variable v: con, and fill it.
     * Updates slot info for spill slot *)
    fun spill_var state (v,con) = 
      let

	val () = chat 2 ("Spilling var " ^ (Name.var2string v) ^ "\n")

	val (state,slot) =
	  (case S.get_var32_loc state v
	     of Dual (s,r) => 
	       let
		 val state = S.emit state (Tal.Mov (Tal.Reg r,(slot2genop state s,[])))
	       in (state,s)
	       end
	      | Reg r => 
	       let
		 val (state,slot) = free_slot' state (Full (con,v))
		 (* Spilling a var means loading it into its designated location *)
		 val state = S.emit state (Tal.Mov (Tal.Reg r,(slot2genop state slot,[])))
	       in (state,slot)
	       end
	      | Slot (s as Temp _) => (state,s)
	      | Slot slot => 
	       let
		 val (state,slot) = free_slot' state (Full (con,v))
		 val (state,t) = free_temp' false state (Junk)
		 (* Spilling a var means loading it into its designated location *)
		 val state = S.emit state (Tal.Mov (t,(slot2genop state slot,[])))
	       in (state,slot)
	       end)

      in S.set_var32_loc state v (Slot slot)
      end


    (* Set a register to Junk, spilling if necessary.
     *)
    and spill_reg state r =
      let
	val state = 
	  case S.get_rstat state r
	    of Avail(Full (con,v)) => spill_var state (v,con)
	     | Avail Junk => state
	     | Reserved _ => error "Freeing reserved register"
	val state = S.set_rstat state r (Avail Junk)
      in state
      end


    (* Get a free register, spilling if necessary *)
    and free_reg' state r rcont = 
      let
	val state = spill_reg state r
	val state = S.set_rstat state r (Avail rcont)
      in state
      end

    and free_slot' state rcont = 
      let
	val (state,slot) = S.alloc_temp_slot state
	val state = S.set_sstat state slot (Avail rcont,true)
      in (state,slot)
      end

    and free_loc (memok : bool) (state : S.state) (rcont : rcont) : (S.state * loc32) = 
      (case get_free_reg state
	 of SOME r => (free_reg' state r rcont,Reg r)
	  | NONE => 
	   if memok then
	     let
	       val (state,slot) = free_slot' state rcont
	     in (state,Slot slot)
	     end
	   else
	     let
	       val r = choose_spill_reg state
	       val state = free_reg' state r rcont
	     in (state,Reg r)
	     end)
	
    and free_temp' memok state rcont = 
      let
	val (state,loc) = free_loc memok state rcont
      in (state,loc2genop state loc)
      end

    fun free_reg state r = free_reg' state r Junk
    fun free_slot state = free_slot' state Junk


    fun free_temp memok state = free_temp' memok state Junk

    fun free_temp_reg' (state : S.state) (rcont : rcont) : (S.state * Tal.reg) = 
      (case free_temp' false state rcont
	 of (state,Tal.Reg r) => (state,r)
	  | _ => error "Not a register")

    fun free_temp_reg state = free_temp_reg' state Junk

    val free_temp_any = free_temp true



    (* Set a register to Reserved, spilling if necessary *)
    fun reserve_reg' state r rcont = 
      let
	val state = spill_reg state r
	val state = S.set_rstat state r (Reserved rcont)
      in state
      end

    fun reserve_slot' state rcont = 
      let
	val (state,slot) = S.alloc_temp_slot state
	val state = S.set_sstat state slot (Reserved rcont,true)
      in (state,slot)
      end

    fun reserve_loc (memok : bool) (state : S.state) (rcont : rcont) : (S.state * loc32) = 
      (case get_free_reg state
	 of SOME r => (reserve_reg' state r rcont,Reg r)
	  | NONE => 
	   if memok then
	     let
	       val (state,slot) = reserve_slot' state rcont
	     in (state,Slot slot)
	     end
	   else
	     let
	       val r = choose_spill_reg state
	       val state = reserve_reg' state r rcont
	     in (state,Reg r)
	     end)
	
    fun reserve_temp' memok state rcont = 
      let
	val (state,loc) = reserve_loc memok state rcont
      in (state,loc2genop state loc)
      end

    fun reserve_reg state r = reserve_reg' state r Junk
    fun reserve_slot state = reserve_slot' state Junk


    fun reserve_temp memok state = reserve_temp' memok state Junk

    fun reserve_temp_reg' (state : S.state) (rcont : rcont) : (S.state * Tal.reg) = 
      (case reserve_temp' false state rcont
	 of (state,Tal.Reg r) => (state,r)
	  | _ => error "Not a register")

    fun reserve_temp_reg state = reserve_temp_reg' state Junk

    val reserve_temp_any = reserve_temp true




    fun eq_gop_goc gop1 (gop2,qs) = 
      if eq_gop gop1 gop2 then
	SOME qs
      else NONE

    (* Emit code to initialize register,
     * and ensure its status is Junk.
     *)
    fun init_reg (state : S.state) (r : Tal.reg) (goc : Tal.genop Tal.coerce) : S.state = 
      let
	val state = 
	  (case eq_gop_goc (Tal.Reg r) goc
	     of SOME [] => state
	      | SOME qs => S.emit state (Tal.Coerce goc)
	      | NONE => S.emit state (Tal.Mov (Tal.Reg r,goc)))
	val state = S.set_rstat state r (Avail Junk)
      in state
      end


    (* Emit code to initialize temp,
     * and change its status from Reserved to Junk.
     * init_temp state dest src
     *)
    fun init_temp (state : S.state) (gop : Tal.genop) (goc : Tal.genop Tal.coerce) : S.state = 
      (case gop
	 of Tal.Reg r => init_reg state r goc
	  | Tal.Prjr ((Tal.Esp,[]),i,NONE) => 
	   let
	     val state = 
	       (case eq_gop_goc gop goc
		  of SOME [] => state
		   | SOME qs => S.emit state (Tal.Coerce goc)
		   | NONE => 
		    (case goc 
		       of (Tal.Prjr _,_) =>  (* No mem/mem moves. *)
			 let
			   val (state,r) = free_temp_reg state
			   val state = S.emit state (Tal.Mov (gop,(Tal.Reg r,[])))
			   val state = init_reg state r goc
			 in state
			 end
			| _ =>  S.emit state (Tal.Mov (gop,goc))))
	     val state = S.set_sstat state (genop2slot state gop) (Avail Junk,true)
	   in state
	   end
	  | _ => error "Not a temp")


    (* Lookup the assigned location for a variable, if one exists. 
     * Kill the variable as well.
     *)
    fun define_var state x = 
      let
	val () = debugdo(3,fn () => (print "Defining var: ";
				     PpLil.pp_var x;
				     print "\n"))
      in
	(case S.get_var32_loc' state x
	   of SOME loc => 
	     let
	       val state = 
		 (case loc
		    of Reg r => S.set_rstat state r (Avail Junk)
		     | Slot slot => S.set_sstat state slot (Avail Junk,true)
		     | Dual(slot,r) => 
		      let
			val state = S.set_rstat state r (Avail Junk)
			val state = S.set_sstat state slot (Avail Junk,true)
		      in state
		      end)
	     in (state,SOME (loc2genop state loc))
	     end
	    | NONE => (state,NONE))
      end



    fun reserve_loc_for_var memok state x c = 
      (case S.get_var32_loc' state x
	 of SOME loc =>
	   (case (loc,memok)
	      of (Slot slot,false) => 
		let
		  val (state,r) = reserve_temp_reg' state (Full(c,x))
		  val state = S.set_var32_loc state x (Dual (slot,r))
		in (state,Reg r)
		end
	       | _ => (state,loc))
	  | NONE => (* Never seen this var before *)
	   let
	     val (state,loc) = reserve_loc memok state (Full (c,x))
	     val state = S.set_var32_loc state x loc
	   in (state, loc)
	   end)

    fun reserve_for_var memok state x c = 
      let
	val (state,loc) = reserve_loc_for_var memok state x c
      in (state,loc2genop state loc)
      end

    fun set_loc_stat state loc s = 
      (case loc
	 of Reg r => S.set_rstat state r s
	  | Slot slot => S.set_sstat state slot (s,true)
	  | Dual(slot,r) => 
	   let
	     val state = S.set_rstat state r s
	     val state = S.set_sstat state slot (s,true)
	   in state
	   end)

    fun get_loc_stat state loc = 
      (case loc
	 of Reg r => S.get_rstat state r
	  | Slot slot => #1 (S.get_sstat state slot)
	  | Dual(slot,r) => S.get_rstat state r)

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
	   in 
	     if eq_gop gop gop2 then 
	       set_loc_stat state (genop2loc state gop) (Avail (Full (c,x)))
	     else 
	       init_temp state gop (gop2,[])
	   end
	  | NONE => 
	   let
	     val loc = genop2loc state gop
	     val state = set_loc_stat state loc (Avail (Full(c,x)))
	     val state = S.set_var32_loc state x loc
	   in state
	   end)

    fun init_var64 state gop x c = error "No floats yet"

    fun load_var state r x c = 
      let
	val state = reserve_reg' state r (Full (c,x))
      in
	case S.get_var32_loc' state x
	 of SOME (Reg r') => 
	   if r = r' then state
	   else 
	     let
	       val state = S.set_rstat state r' (Avail Junk)
	       val state = S.set_var32_loc state x (Reg r)
	     in state
	     end
	  | SOME (Dual(slot,r')) =>
	    if r = r' then state
	    else 
	      let
		val state = S.set_rstat state r' (Avail Junk)
		val state = S.set_var32_loc state x (Dual(slot,r))
	      in state
	      end
	  | SOME (Slot slot) => 
	      let
		val state = S.set_var32_loc state x (Dual(slot,r))
	      in state
	      end
	  | NONE =>
	      let
		val state = S.set_var32_loc state x (Reg r)
	      in state
	      end
      end

    fun add_blocks_from_state dest source = S.add_blocks dest (S.get_blocks source)

    fun pop_outargs state = 
      let
	val (state,outargs) = S.pop_outargs state
	val size = S.get_outarg_size state
	fun loop (outargs,state) = 
	  (case outargs
	     of [] => state
	     | arg::outargs =>
	       let
		 val state = 
		   (case arg
		      of (Avail(Full(c,x)),true) => spill_var state (x,c)
		       | (Avail(Full(c,x)),false) => error "Floats not handled yet"
		       | (Reserved _,_) => (warn "popping reserved inarg";state)
		       | _ => state)
	       in loop(outargs,state)
	       end)
	val state = loop(outargs,state)
	val state = S.emit state (Tal.ArithBin(Tal.Sub,Tal.Reg Tal.Esp,Tal.Immed (size * 0w4)))
      in state
      end

    fun alloc_outargs state num32 num64 = S.alloc_outarg_space state (num32 + 2 * num64)
    fun alloc_inargs state num32 num64 = S.alloc_inarg_space state (num32 + 2 * num64 + 1)

    fun reserve_next_arg32 state = 
      let
	val (state,slot) = S.alloc_outarg_slot state
	val state = S.set_sstat state slot (Reserved Junk,true)
      in (state,slot2genop state slot)
      end

    fun reserve_next_arg64 state = error "No floats yet"

    fun reserve_next_inarg32 state = 
      let
	val (state,slot) = S.alloc_inarg_slot state
	val state = S.set_sstat state slot (Reserved Junk,true)
      in (state,slot2genop state slot)
      end

    fun reserve_next_inarg64 state = error "No floats yet"

    fun reg_mov state r1 r2 = 
      if eq_reg r1 r2 then state
      else S.emit state (Tal.Mov(Tal.Reg r1,(Tal.Reg r2,[])))

    fun reg_slot_mov state r s = S.emit state (Tal.Mov(Tal.Reg r,(slot2genop state s,[])))

    fun slot_reg_mov state s r = S.emit state (Tal.Mov(slot2genop state s,(Tal.Reg r,[])))

    fun slot_slot_mov state s1 s2 = 
      if eq_slot s1 s2 then state
      else
	let
	  val (state,r) = reserve_temp_reg state
	  val state = slot_reg_mov state s1 r
	  val state = reg_slot_mov state r s2
	  val state = release_reg state r
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
		   if eq_slot s1 s2 then state
		   else slot_reg_mov state s1 r2
	       in state
	       end)
      in state
      end

    fun sanity_check (state : S.state) = 
      let
	val regs = #registers state
	fun loop i = 
	  if i < rcount then 
	    let
	      val () = 
		(case Array.sub(regs,i)
		   of Avail(Full(c,x)) => ()
		    | Avail(Junk) => ()
		    | Reserved _ => error "Register is reserved across op boundary")
	    in loop ( i + 1)
	    end
	  else ()
      in loop 0
      end

    fun match_registers (orig_state : S.state) (state : S.state) = 
      let
	val old_regs = #registers orig_state
	fun loop i = 
	  if i < rcount then 
	    let
	      val state = 
		(case Array.sub(old_regs,i)
		   of Avail(Full(c,x)) => 
		     let
		       val r = idxr i
		       val state = load_var state r x c
		       val state = release_reg state r
		     in state 
		     end
		    | Avail(Junk) => spill_reg state (idxr i)
		    | Reserved _ => error "Trying to match reserved register")
	    in loop ( i + 1)
	    end
	  else state
      in loop 0
      end

    fun compensate (x,loc,state) = 
      (case S.get_var32_loc' state x
	 of NONE => state
	  | SOME qloc => 
	   let
	     val state = loc_mov state qloc loc
	     val state = S.set_var32_loc state x loc

	     val state = set_loc_stat state qloc (Avail Junk)
(*	     val state = set_loc_stat state loc (Avail(Full(c,x)))*)
	   in state
	   end)
	 
    fun coerce_state (coercee : S.state) (coercer : S.state) = 
      let
	val vars32 = #vars32 coercer
	val coercee = VarMap.foldli compensate coercee vars32
      in coercee
      end


    fun reconcile_state (state : S.state) (newstate : S.state) = 
      let
	fun reconcile (x,loc,state) = 
	  (case S.get_var32_loc' state x
	     of NONE => 
	       let
		 val state = S.set_var32_loc state x loc
		 val state = set_loc_stat state loc (get_loc_stat newstate loc)
	       in state
	       end
	      | SOME _ => state)
	     
	val vars32 = #vars32 newstate
	val state = VarMap.foldli reconcile state vars32
      in state
      end

    fun join_states (principal : S.state) (adjunct : S.state) = 
      let
	val adjunct = coerce_state adjunct principal
	val principal = reconcile_state principal adjunct
	val principal = add_blocks_from_state principal adjunct
      in principal
      end

    fun reserve_next_inarg32 state = 
      let
	val (state,slot) = S.alloc_inarg_slot state
	val state = S.set_sstat state slot (Reserved Junk,true)
      in (state,slot2genop state slot)
      end


    (* Emit code to place formal in it's chosen location. *)
    fun bind_next_formal32 state x c = 
      let
	val (state,slot) = S.alloc_inarg_slot state
	val state = S.set_sstat state slot (Avail(Full(c,x)),true)
      in
	(case S.get_var32_loc' state x
	   of SOME loc => 
	     let
	       val state =loc_mov state (Slot slot) loc
	       val state = set_loc_stat state loc (Avail Junk)
	     in state
	     end
	    | NONE => state)
      end

    (* Emit code to place formal in it's chosen location. *)
    fun bind_next_formal64 state x c = error "No floats yet"

    (* Exports *)
    type state = S.state

    val emit = S.emit
    val emit_block = S.emit_block

    val release_reg = release_reg

    val reserve_reg = reserve_reg
    val reserve_temp = reserve_temp
    val reserve_for_var = reserve_for_var

    val init_reg = init_reg
    val init_temp = init_temp
    val init_var32 = init_var32
    val init_var64 = init_var64

    val typeof_reg = S.typeof_reg
    val typeof_stack = S.typeof_stack

    val get_frame_size = S.get_frame_size
    val get_inarg_size = S.get_inarg_size
    val get_temp_size = S.get_temp_size
    val get_outarg_size = S.get_outarg_size

    val load_var = load_var
    val spill_reg = spill_reg

    val new_state = S.new_state
    val new_state' = S.new_state'

    val pop_outargs = pop_outargs
    val alloc_outargs = alloc_outargs


    val reserve_temp_reg = reserve_temp_reg
    val reserve_temp_any = reserve_temp_any

    val reserve_next_arg32 = reserve_next_arg32
    val reserve_next_arg64 = reserve_next_arg64

    val reserve_next_inarg32 = reserve_next_inarg32
    val reserve_next_inarg64 = reserve_next_inarg64

    val bind_next_formal32 = bind_next_formal32
    val bind_next_formal64 = bind_next_formal64

    val add_blocks_from_state = add_blocks_from_state
    val get_blocks = S.get_blocks
    val flush_blocks = S.flush_blocks

end  (* LilToTal *)