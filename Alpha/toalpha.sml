(*$import PPRTL DECALPHA MACHINEUTILS TRACETABLE BBLOCK DIVMULT TOASM Util *)

(* Translation from Rtl to DecAlpha pseudoregister-assembly. 
   Assumptions:
     (1) The thread pointer points to a structure where the first 32 longs
           store the saved general purpose registers.  In particular, when
	   making C calls, the allocation pointer and allocation limit are 
	   saved at offset given by the 8 times the register's number.

*)
         
functor Toalpha(structure Decalpha: DECALPHA 
                structure Pprtl: PPRTL
                structure Machineutils : MACHINEUTILS 
	        structure ArgTracetable : TRACETABLE 
                structure Bblock : BBLOCK
		    where type Machine.specific_instruction = Decalpha.specific_instruction
		    where type Machine.instruction = Decalpha.Machine.instruction
                structure DM : DIVMULT
		    where type DA.Machine.instruction = Decalpha.Machine.instruction
		sharing ArgTracetable = Bblock.Tracetable)
  :> TOASM where Machine = Decalpha.Machine
	   where Bblock = Bblock
	   where Tracetable = ArgTracetable
= 
struct

   structure Bblock = Bblock
   structure Decalpha = Decalpha
   structure Machineutils = Machineutils
   structure Tracetable = ArgTracetable
   structure W = TilWord32
   val i2w = W.fromInt
   val w2i = W.toInt
       
   open Machineutils Bblock
   open Decalpha 
   open Machine
   open Core

   val error = fn s => Util.error "alpha/trackstorage.sml" s

   (* Translate the two RTL register types into the single
      DECALPHA register type.  *)

   val tracemap = ref (Regmap.empty) : (register option * Tracetable.trace) Regmap.map ref

   local
     (* stack slot position of next variable to be made stack-resident *)

     val count = ref 0
     val stack_res = ref (Regmap.empty) : stacklocation Regmap.map ref

   in
     fun init_stack_res () = (count := 0; stack_res := Regmap.empty)
     fun get_stack_res () = !stack_res
     fun add_stack x =
	 (case Regmap.find(!stack_res,x)
	  of SOME i => i
	   | NONE => 
		 let val a = !count
		 in stack_res := Regmap.insert(!stack_res,x,SPILLED_INT(!count));
		    count := !count + 1;
		    SPILLED_INT a
		 end)
   end




   fun translateRep rep =
       case rep of
	  Rtl.TRACE => (NONE, Tracetable.TRACE_YES)
        | Rtl.LOCATIVE => (NONE, Tracetable.TRACE_IMPOSSIBLE)
        | Rtl.COMPUTE path =>
	      (case path of
	         Rtl.Projvar_p (Rtl.REGI(v,_),[]) => 
		     ((case (Regmap.find(!tracemap,R (Name.var2int v))) of
			   NONE => NONE
			 | SOME (ropt,_) => ropt),
			   Tracetable.TRACE_STACK(add_stack (R (Name.var2int v))))
	       | Rtl.Projvar_p (Rtl.REGI(v,_),i) => 
		     ((case (Regmap.find(!tracemap,R (Name.var2int v))) of
			   NONE => NONE
			 | SOME (ropt,_) => ropt),
		       Tracetable.TRACE_STACK_REC(add_stack(R (Name.var2int v)),i))
	       | Rtl.Projvar_p (Rtl.SREGI _, i) => error "SREG should not contain type"
	       | Rtl.Projlabel_p (l,[]) => (NONE,Tracetable.TRACE_GLOBAL l)
	       | Rtl.Projlabel_p (l,i) => (NONE,Tracetable.TRACE_GLOBAL_REC (l,i))
	       | Rtl.Notneeded_p => (NONE,Tracetable.TRACE_IMPOSSIBLE))
	| Rtl.UNSET => (NONE,Tracetable.TRACE_UNSET)
	| Rtl.NOTRACE_INT => (NONE,Tracetable.TRACE_NO)
	| Rtl.NOTRACE_REAL => (NONE,Tracetable.TRACE_NO)
	| Rtl.NOTRACE_CODE => (NONE,Tracetable.TRACE_NO)
	| Rtl.NOTRACE_LABEL => (NONE,Tracetable.TRACE_NO)

   fun internal_translateRep v Rtl.UNSET = (add_stack (R (Name.var2int v)); 
					    translateRep Rtl.UNSET)
     | internal_translateRep _ rep = translateRep rep	

   fun translateSReg Rtl.HEAPPTR = Rheap
     | translateSReg Rtl.HEAPLIMIT = Rhlimit
     | translateSReg Rtl.STACKPTR = Rsp
     | translateSReg Rtl.THREADPTR = Rth
     | translateSReg Rtl.EXNPTR = Rexnptr
     | translateSReg Rtl.EXNARG = Rexnarg
     
   fun translateIReg (Rtl.REGI (v, rep)) = 	
       (tracemap := Regmap.insert(!tracemap,R (Name.var2int v),internal_translateRep v rep);
	R (Name.var2int v))
     | translateIReg (Rtl.SREGI s) = translateSReg s
     
   fun translateFReg (Rtl.REGF (v, _)) = F (Name.var2int v)

   fun translateReg (Rtl.I ir) = translateIReg ir
     | translateReg (Rtl.F fr) = translateFReg fr

   (* Translate an RTL register option into a DECALPHA register option *)
   fun translateIRegOpt NONE = NONE
     | translateIRegOpt (SOME Reg) = SOME (translateIReg Reg)

   (* Translate the register-or-immediate value found as the second 
      source operand in many alpha instructions *)
   fun translateOp (Rtl.REG rtl_reg) = 
         REGop (translateIReg rtl_reg)
     | translateOp (Rtl.IMM src2) =
	 if (in_imm_range src2) then 
	   IMMop src2
	 else
	   error ("immediate out of range: " ^ (Int.toString src2))





   (*****************************************
    * MAIN TRANSLATION STARTS HERE ROUTINES *
    *****************************************)

   (* Translation data structures. *)

   (* All the basic blocks genenerated are stored in this
      label-to-basic-block mapping *)
   val block_map = ref (Labelmap.empty) : bblock Labelmap.map ref

   (* As the translator steps through a basic block, the current_...
      values are updated; at the end of the basic block they're
      packaged up into a BBLOCK and stored in the block_map. *)

   (* Name of the procedure being allocated *)
   val current_proc   = ref (freshCodeLabel ()) : label ref

   (* The current procedure's formal return variables; accessible
      here so that we can add them to DecAlpha's RETURN assembly op *)
   val current_res    = ref []              : register list ref

   (* Name/Label of the block currently being allocated *)
   val current_label  = ref (freshCodeLabel ()) : label ref

   (* List of instructions in this basic block.
      !!!instrs are kept in REVERSE order in this list!!! *)
   val current_instrs = ref [] : instruction annotated list ref

   (* Blocks to which control may flow following this block *)
   val current_succs  = ref []              : label list ref

   (* True iff this block's label is ever referenced.  If not,
      we don't have to print this label in the final assembly code *)
   val current_truelabel = ref false        : bool ref

   (* list of all the labels of blocks in the procedure,
      in reverse order from the order in which the blocks appeared
      in the RTL code.  That is, as blocks are translated their
      label is prepended to this list.  Note that the hd of this list
      is the label of the last block saved in the block_map. *)
   val current_blocklabels = ref [] : label list ref

   (* Flag:  True if the last block stored ended with a unconditional
             branch, or otherwise control does not flow through to the
	     current block. *)
   val no_fallthrough = ref true

   (* Translation functions *)

   (* Find a block by it's name in the block_map *)
   fun getBlock block_label = 
       (case (Labelmap.find(! block_map, block_label)) of
	    SOME bl => bl | NONE => error "getBlock")

   (* Remove all occurrences of a given label from a list *)
   fun removeAllLabel [] _ = []
     | removeAllLabel (lab :: rest) lab' = 
       if (eqLabs lab lab') then
	 removeAllLabel rest lab'
       else
	 lab :: (removeAllLabel rest lab')

   (* Package up the current_??? values into a basic block,
      and store it in the block_map if the label must occur
      in the output program or the list of instructions is nonempty. *)
   fun saveBlock () =
     if (! current_truelabel orelse 
	 length (! current_instrs) > 0) then
       (block_map := Labelmap.insert (! block_map, ! current_label,
				      BLOCK{instrs  = ref (! current_instrs),
					    def     = Regset.empty,
					    use     = Regset.empty,
					    in_live  = ref (Regset.empty),
					    out_live = ref (Regset.empty),
					    truelabel= ! current_truelabel,
					    succs  = ref (! current_succs)});
       current_blocklabels := (! current_label) :: (! current_blocklabels))
     else
       (* We have a useless, empty basic block here.  Delete all
          references to this block as the successor of somebody. *)
       Labelmap.app 
          (fn (BLOCK{succs,...}) => 
	   succs := (removeAllLabel (! succs) (! current_label)))
	  (! block_map)
	

   (* Reset the current_??? values.  If a add_to_predecessor is
      true, and we weren't told the previous block does not
      fall through, then the last block stored in the block_map
      will have the newly reset block added as a successor. *)
   fun resetBlock new_label truelabel add_to_predecessor =
     (current_label  := new_label;
      current_instrs := [];
      current_succs  := [];
      current_truelabel := truelabel;

      if add_to_predecessor andalso (not (! no_fallthrough)) then
	let val (BLOCK{succs,...}) = getBlock (hd (! current_blocklabels))
	in 
	  succs := new_label :: (! succs) 
	end
      else
	();
	
      no_fallthrough := false)

   (* Adds an instruction to the current basic block, updating the other
      relevant current_??? values.  If this is a control-flow instruction,
      saves the current basic block and sets up a new, empty block. *)
   fun emit (instr : instruction) =
       (current_instrs := (NO_ANN instr) :: (! current_instrs);

	case (Decalpha.Machine.cFlow instr) of 
	  NONE => ()
	| SOME (fallthrough, succ_labels) =>
	    let 
	      val nextlabel = freshCodeLabel ()
	    in
	      current_succs := succ_labels @ (! current_succs);
	      if fallthrough then 
		 current_succs := nextlabel :: (! current_succs)
	      else
		 ();
	      saveBlock ();
	      resetBlock nextlabel false false;
	      no_fallthrough := not fallthrough
	    end)

   val Rpv = (case Rpv of
		NONE => error "no Rpv for Alpha"
	      | SOME x => x)

   (* Translate an RTL instruction into one or more 
      Alpha instructions *)
   fun load_imm (immed, Rdest) =
          let  
            val w65535 = i2w 65535
	    val low    = w2i (W.andb(immed, w65535))
            val high   = w2i (W.rshiftl(immed, 16))
            val low'   = if (low > 32767) then low - 65536 else low
            val high'  = if (high > 32767) then high - 65536 else high
	    val Rtemp  = if ((high' <> 0) andalso (low' = 0)) then Rzero
			 else (emit (SPECIFIC (LOADI (LDA, Rdest, low', Rzero))); Rdest)
	  in
	      if (low' >= 0) then
		  if (high' <> 0) then emit (SPECIFIC(LOADI(LDAH,Rdest,high',Rtemp))) else ()
	      else 
		  if (high' = ~1) then 
		      ()
		  else 
		      if (high' <> 32767) then
			  emit(SPECIFIC(LOADI (LDAH, Rdest, high' + 1, Rtemp)))
		      else
			  emit(SPECIFIC(LOADI (LDAH, Rdest, ~32768, Rtemp)))
	  end

   fun translate (Rtl.LI (immed, rtl_Rdest)) = load_imm(immed,translateIReg rtl_Rdest)
     | translate (Rtl.LADDR (label, offset, rtl_Rdest)) =
          let
	    val Rdest = translateIReg rtl_Rdest
	  in
	    emit (BASE(LADDR (Rdest, label)));
	    if (offset <> 0) then 
	      emit (SPECIFIC(LOADI (LDA, Rdest, offset, Rdest)))
	    else 
	      ()
	  end

     | translate (Rtl.LEA (Rtl.EA (rtl_Raddr, offset), rtl_Rdest)) =
          emit (SPECIFIC(LOADI (LDA, translateIReg rtl_Rdest, offset, 
		       translateIReg rtl_Raddr)))

     | translate (Rtl.MV (rtl_Rsrc, rtl_Rdest)) =
          emit (BASE(MOVI (translateIReg rtl_Rsrc, translateIReg rtl_Rdest)))

     | translate (Rtl.CMV (cmp, rtl_Rsrc1, op2, rtl_Rdest)) =
       let 
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rdest = translateIReg rtl_Rdest
       in
	 (emit o SPECIFIC o INTOP)
         (case cmp of
	   Rtl.EQ =>  (CMOVEQ,  Rsrc1, translateOp op2, Rdest)
	 | Rtl.NE =>  (CMOVNE,  Rsrc1, translateOp op2, Rdest)
	 | Rtl.GT =>  (CMOVGT,  Rsrc1, translateOp op2, Rdest)
	 | Rtl.GE =>  (CMOVGE,  Rsrc1, translateOp op2, Rdest)
	 | Rtl.LT =>  (CMOVLT,  Rsrc1, translateOp op2, Rdest)
	 | Rtl.LE =>  (CMOVLE,  Rsrc1, translateOp op2, Rdest)
	 | Rtl.LBS => (CMOVLBS, Rsrc1, translateOp op2, Rdest)
	 | Rtl.LBC => (CMOVLBC, Rsrc1, translateOp op2, Rdest))
       end

     | translate (Rtl.FMV (rtl_Fsrc, rtl_Fdest)) =
       let
	 val Fsrc = translateFReg rtl_Fsrc
	 val Fdest = translateFReg rtl_Fdest
       in
	 emit (BASE (MOVF (Fsrc,Fdest)))
       end

     | translate (Rtl.ADD (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (ADDL, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.SUB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SUBL, Rsrc1, src2, Rdest)))
       end
     | translate (Rtl.MUL (rtl_Rsrc1, op2 as (Rtl.IMM denom), rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val Rdest = translateIReg rtl_Rdest
       in if !DM.opt_on 
	  then let val instrs = DM.quad_mult_convert (Rsrc1,i2w denom, 
						      Rdest)
	       in app emit instrs
	       end
	  else
             let val src2  = translateOp op2
	     in emit (SPECIFIC(INTOP (MULL, Rsrc1, src2, Rdest)))
	     end
       end

     | translate (Rtl.MUL (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (MULL, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.DIV (rtl_Rsrc1, Rtl.REG rtl_Rsrc2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rsrc2 = translateIReg rtl_Rsrc2
	 val Rdest = translateIReg rtl_Rdest
       in
	 (* This is a special call to the libc __divl routine, which
	    wants arguments in $24 & 25, its address in $27, a return
            address in $23, and returns its result in $27. *)
	 emit (BASE(RTL (CALL{calltype = Rtl.C_NORMAL,
			      func = DIRECT (Rtl.C_EXTERN_LABEL "__divl", SOME (ireg 23)),
			      args = [Rsrc1, Rsrc2],
			      results = [Rdest],
			      argregs = SOME [ireg 24, ireg 25],
			      resregs = SOME [ireg 27],
			      destroys = SOME [ireg 23, ireg 24, ireg 25, 
					       ireg 26, ireg 27, ireg 29]})))
       end

     | translate (Rtl.DIV (rtl_Rsrc1, Rtl.IMM denom, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rdest = translateIReg rtl_Rdest
	 val instrs = DM.signed_div_convert(Rsrc1,i2w denom,Rdest)
       in
	 if (length instrs > 0)
	   then (app emit instrs)
	 else
	   let
	     val Rsrc2 = freshIreg ()
	   in
	     emit (SPECIFIC(LOADI(LDA, Rsrc2, denom, Rzero)));
	     emit (BASE(RTL (CALL{calltype = Rtl.C_NORMAL,
				  func = DIRECT (Rtl.C_EXTERN_LABEL "__divl",SOME (ireg 23)),
				  args = [Rsrc1, Rsrc2],
				  results = [Rdest],
				  argregs = SOME [ireg 24, ireg 25],
				  resregs = SOME [ireg 27],
				  destroys = SOME [ireg 23, ireg 24, ireg 25, 
						   ireg 26, ireg 27, ireg 29]})))
	   end
       end
     | translate (Rtl.MOD (rtl_Rsrc1, Rtl.REG rtl_Rsrc2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rsrc2 = translateIReg rtl_Rsrc2
	 val Rdest = translateIReg rtl_Rdest
       in
	 (* This is a special call to the libc __reml routine, which
	    wants arguments in $24 & 25, its address in $27, a return
            address in $23, and returns its result in $27 and the pv in $23. *)
		   emit (BASE(RTL (CALL{calltype = Rtl.C_NORMAL,
					func = DIRECT (Rtl.C_EXTERN_LABEL "__reml",SOME(ireg 23)),
					args = [Rsrc1, Rsrc2],
					results = [Rdest],
					argregs = SOME [ireg 24, ireg 25],
					resregs = SOME [ireg 27],
					destroys = SOME [ireg 23, ireg 24, ireg 25, 
							 ireg 26, ireg 27, ireg 29]})))
       end

     | translate (Rtl.MOD (rtl_Rsrc1, Rtl.IMM denom, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rdest = translateIReg rtl_Rdest
	 val Rsrc2 = freshIreg ()
       in
	 emit (SPECIFIC(LOADI(LDA, Rsrc2, denom, Rzero)));
	 emit (BASE(RTL (CALL{calltype = Rtl.C_NORMAL,
			      func = DIRECT (Rtl.C_EXTERN_LABEL "__reml",SOME(ireg 23)),
			      args = [Rsrc1, Rsrc2],
			      results = [Rdest],
			      argregs = SOME [ireg 24, ireg 25],
			      resregs = SOME [ireg 27],
			      destroys = SOME [ireg 23, ireg 24, ireg 25, 
					       ireg 26, ireg 27, ireg 29]})))
       end

     | translate (Rtl.S4ADD (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 (* Rdest <- 4 * Rsrc1 + src2 *)
	 emit (SPECIFIC(INTOP (S4ADDL, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.S8ADD (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 (* Rdest <- 8 * Rsrc1 + src2 *)
	 emit (SPECIFIC(INTOP (S8ADDL, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.S4SUB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 (* Rdest <- 4 * Rsrc1 - src2 *)
	 emit (SPECIFIC(INTOP (S4SUBL, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.S8SUB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 (* Rdest <- 8 * Rsrc1 - src2 *)
	 emit (SPECIFIC(INTOP (S8SUBL, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.ADDT (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (ADDLV, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.SUBT (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SUBLV, Rsrc1, src2, Rdest)))
       end


     | translate (Rtl.MULT (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (MULLV, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.DIVT args) = translate (Rtl.DIV args)

     | translate (Rtl.MODT args) = translate (Rtl.MOD args)

     | translate (Rtl.CMPSI (comparison, rtl_Rsrc1, Rtl.REG rtl_Rsrc2,
			     rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rsrc2 = translateIReg rtl_Rsrc2
	 val Rdest = translateIReg rtl_Rdest
       in
	 case comparison of
	   Rtl.EQ =>  emit (SPECIFIC (INTOP (CMPEQ, Rsrc1, REGop (Rsrc2), Rdest)))
	 | Rtl.LE =>  emit (SPECIFIC (INTOP (CMPLE, Rsrc1, REGop (Rsrc2), Rdest)))
	 | Rtl.LT =>  emit (SPECIFIC (INTOP (CMPLT, Rsrc1, REGop (Rsrc2), Rdest)))
	 | Rtl.GE =>  emit (SPECIFIC (INTOP (CMPLE, Rsrc2, REGop (Rsrc1), Rdest)))
	 | Rtl.GT =>  emit (SPECIFIC (INTOP (CMPLT, Rsrc2, REGop (Rsrc1), Rdest)))
	 | Rtl.NE => (emit (SPECIFIC (INTOP (CMPEQ, Rsrc1, REGop (Rsrc2), Rdest)));
		      emit (SPECIFIC (INTOP (CMPEQ, Rdest, REGop Rzero, Rdest))))
	 | _ => error ("translate/CMPSI/reg: bad comparison")
       end

     | translate (Rtl.CMPSI (comparison, rtl_Rsrc1, Rtl.IMM src2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rdest = translateIReg rtl_Rdest
       in
	 if (in_imm_range src2) then
	   (case comparison of
	      Rtl.EQ =>  emit (SPECIFIC (INTOP (CMPEQ, Rsrc1, IMMop src2,  Rdest)))
	    | Rtl.LE =>  emit (SPECIFIC (INTOP (CMPLE, Rsrc1, IMMop src2,  Rdest)))
	    | Rtl.LT =>  emit (SPECIFIC (INTOP (CMPLT, Rsrc1, IMMop src2,  Rdest)))
	    | Rtl.GE => if (src2 = 0) 
			    then  emit (SPECIFIC (INTOP (CMPLE, Rzero,   REGop Rsrc1, Rdest)))
			else (emit (SPECIFIC (INTOP (OR,    Rzero, IMMop src2,  Rat)));
			      emit (SPECIFIC(INTOP (CMPLE, Rat,   REGop Rsrc1, Rdest))))
	    | Rtl.GT =>  if (src2 = 0) 
			    then  emit (SPECIFIC(INTOP (CMPLT, Rzero,   REGop Rsrc1, Rdest)))
			 else (emit (SPECIFIC(INTOP (OR,    Rzero, IMMop src2,  Rat)));
			       emit (SPECIFIC(INTOP (CMPLT, Rat,   REGop Rsrc1, Rdest))))
	    | Rtl.NE => (emit (SPECIFIC(INTOP (CMPEQ, Rsrc1, IMMop src2,  Rdest)));
			 emit (SPECIFIC(INTOP (CMPEQ, Rdest, REGop Rzero, Rdest))))
	    | _ => error ("translate/CMPSI/imm: bad comparison"))
	 else
	   error ("CMPSI: Immediate out of range: " ^ (Int.toString src2))
       end

     | translate (Rtl.CMPUI (comparison, rtl_Rsrc1, Rtl.REG rtl_Rsrc2,
			     rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rsrc2 = translateIReg rtl_Rsrc2
	 val Rdest = translateIReg rtl_Rdest
       in
	 case comparison of
	   Rtl.EQ =>  emit (SPECIFIC (INTOP (CMPEQ,  Rsrc1, REGop Rsrc2, Rdest)))
	 | Rtl.LE =>  emit (SPECIFIC (INTOP (CMPULE, Rsrc1, REGop Rsrc2, Rdest)))
	 | Rtl.LT =>  emit (SPECIFIC (INTOP (CMPULT, Rsrc1, REGop Rsrc2, Rdest)))
	 | Rtl.GE =>  emit (SPECIFIC (INTOP (CMPULE, Rsrc2, REGop Rsrc1, Rdest)))
	 | Rtl.GT =>  emit (SPECIFIC (INTOP (CMPULT, Rsrc2, REGop Rsrc1, Rdest)))
	 | Rtl.NE => (emit (SPECIFIC (INTOP (CMPEQ,  Rsrc1, REGop Rsrc2, Rdest)));
		      emit (SPECIFIC (INTOP (CMPEQ,  Rdest, REGop Rzero, Rdest))))
	 | _ => error ("translate/CMPUI/reg: bad comparison")
       end

     | translate (Rtl.CMPUI (comparison, rtl_Rsrc1, Rtl.IMM src2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rdest = translateIReg rtl_Rdest
       in
	 if (in_imm_range src2) then
	   (case comparison of
	      Rtl.EQ => emit (SPECIFIC (INTOP (CMPEQ,  Rsrc1, IMMop src2, Rdest)))
	    | Rtl.LE => emit (SPECIFIC (INTOP (CMPULE, Rsrc1, IMMop src2, Rdest)))
	    | Rtl.LT => emit (SPECIFIC (INTOP (CMPULT, Rsrc1, IMMop src2, Rdest)))
	    | Rtl.GE => (emit (SPECIFIC (INTOP (OR,     Rzero, IMMop src2, Rat)));
			 emit (SPECIFIC (INTOP (CMPULE, Rat, REGop Rsrc1, Rdest))))
	    | Rtl.GT => (emit (SPECIFIC (INTOP (OR,     Rzero, IMMop src2, Rat)));
			 emit (SPECIFIC (INTOP (CMPULT, Rat, REGop Rsrc1, Rdest))))
	    | Rtl.NE => (emit (SPECIFIC (INTOP (CMPEQ, Rsrc1, IMMop src2, Rdest)));
			 emit (SPECIFIC (INTOP (CMPEQ, Rdest, REGop Rzero, Rdest))))
	    | _ => error ("translate/CMPUI/imm: bad comparison"))
	  else
            error ("translate/CMPUI/imm: Immediate out of range: " ^ 
                   (Int.toString src2))
       end

     | translate (Rtl.NOTB (rtl_Rsrc, rtl_Rdest)) =
       let
	 val Rsrc = translateIReg rtl_Rsrc
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (INTOP (ORNOT, Rzero, REGop Rsrc, Rdest)))
       end

     | translate (Rtl.ANDB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (INTOP (AND, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.ORB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (INTOP (OR, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.XORB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (INTOP (XOR, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.SLL (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 (* Need to sign-extend shifted result to regain
            canonical 32-bit form in register (See Assembly
            manual, page B-3 *)
	 emit (SPECIFIC (INTOP (SLL, Rsrc1, src2, Rdest)));
	 emit (SPECIFIC (INTOP (ADDL, Rdest, REGop Rzero, Rdest)))
       end

     | translate (Rtl.SRL (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 (* Need to sign-extend shifted result to regain
            canonical 32-bit form in register (See Assembly
            manual, page B-3 *)
	 emit (SPECIFIC (INTOP (ZAP, Rsrc1, IMMop 0xF0, Rdest)));
	 emit (SPECIFIC (INTOP (SRL, Rdest, src2, Rdest)));
	 emit (SPECIFIC (INTOP (ADDL, Rdest, REGop Rzero, Rdest)))
       end

     | translate (Rtl.SRA (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (INTOP (SRA, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.FADDD (rtl_Fsrc1, rtl_Fsrc2, rtl_Fdest)) =
       let
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val Fdest = translateFReg rtl_Fdest
       in
          emit (SPECIFIC (FPOP (ADDT, Fsrc1, Fsrc2, Fdest)))
       end

     | translate (Rtl.FSUBD (rtl_Fsrc1, rtl_Fsrc2, rtl_Fdest)) =
       let
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val Fdest = translateFReg rtl_Fdest
       in
          emit (SPECIFIC (FPOP (SUBT, Fsrc1, Fsrc2, Fdest)))
       end

     | translate (Rtl.FMULD (rtl_Fsrc1, rtl_Fsrc2, rtl_Fdest)) =
       let
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val Fdest = translateFReg rtl_Fdest
       in
          emit (SPECIFIC (FPOP (MULT, Fsrc1, Fsrc2, Fdest)))
       end

     | translate (Rtl.FDIVD (rtl_Fsrc1, rtl_Fsrc2, rtl_Fdest)) =
       let
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val Fdest = translateFReg rtl_Fdest
       in
          emit (SPECIFIC (FPOP (DIVT, Fsrc1, Fsrc2, Fdest)))
       end

     | translate (Rtl.FABSD (rtl_Fsrc, rtl_Fdest)) =
       let
	 val Fsrc  = translateFReg rtl_Fsrc
         val Fdest = translateFReg rtl_Fdest
       in
	 emit (SPECIFIC (FPOP (CPYS, Fzero, Fsrc, Fdest)))
       end

     | translate (Rtl.FNEGD (rtl_Fsrc, rtl_Fdest)) =
       let
	 val Fsrc  = translateFReg rtl_Fsrc
         val Fdest = translateFReg rtl_Fdest
       in
	 emit (SPECIFIC (FPOP (CPYSN, Fsrc, Fsrc, Fdest)))
       end


     | translate (Rtl.CVT_REAL2INT (rtl_Fsrc, rtl_Rdest)) =
       let
	 val Fsrc  = translateFReg rtl_Fsrc
         val Rdest = translateIReg rtl_Rdest
       in
	 (* Converts a double-precision floating-point value in Fsrc
	    to a canonical integer bit-pattern in Rdest, rounding
	    towards minus-infinity. *)
	   emit (SPECIFIC (FPCONV (CVTTQM, Fsrc, Fat)));
	   emit (BASE(LADDR (Rat, Rtl.ML_EXTERN_LABEL ("FPTOFROMINT"))));
	   emit (SPECIFIC(STOREF (STT, Fat, 0, Rat)));
	   emit (SPECIFIC(LOADI (LDQ, Rdest, 0, Rat)))
       end

     | translate (Rtl.CVT_INT2REAL (rtl_Rsrc, rtl_Fdest)) =
       let
	 val Rsrc  = translateIReg rtl_Rsrc
         val Fdest = translateFReg rtl_Fdest
       in
	 (* Converts an integer in Rsrc to a double-precision 
	    floating-point value in Fdest; this is always precise *)
	   emit (BASE(LADDR (Rat, Rtl.ML_EXTERN_LABEL ("FPTOFROMINT"))));
	   emit (SPECIFIC(STOREI (STQ, Rsrc, 0, Rat)));
	   emit (SPECIFIC(LOADF  (LDT, Fdest, 0, Rat)));
	   emit (SPECIFIC (FPCONV (CVTQT, Fdest, Fdest)))
       end

 
     | translate (Rtl.CMPF (comparison, rtl_Fsrc1, rtl_Fsrc2, rtl_Rdest)) =
       let
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val Rdest = translateIReg rtl_Rdest
	 val label = Rtl.fresh_code_label "cmpf"
	 val (fop,reverse_operand, reverse_result) = 
		(case comparison of
		   Rtl.EQ =>  (CMPTEQ, false, false)
		 | Rtl.LE =>  (CMPTLE, false, false)
		 | Rtl.LT =>  (CMPTLT, false, false)
		 | Rtl.GE =>  (CMPTLE, true, false)
		 | Rtl.GT =>  (CMPTLT, true, false)
		 | Rtl.NE =>  (CMPTEQ, false, true)
		 | _ => error ("translate/CMPF: bad comparison"))
	val (fail_test, pass_test) = if (not reverse_result) then (0,1) else (1,0)
       in
         emit (SPECIFIC (LOADI (LDA, Rdest, fail_test, Rzero)));
	 if (not reverse_operand)
	   then emit (SPECIFIC (FPOP(fop, Fsrc1, Fsrc2, Fat)))
	 else emit (SPECIFIC (FPOP(fop, Fsrc2, Fsrc1, Fat)));
         emit (SPECIFIC (CBRANCHF (FBEQ, Fat, label)));
    	 emit (SPECIFIC (LOADI (LDA, Rdest, pass_test, Rzero)));          
	 translate (Rtl.ILABEL label)
       end

     | translate (Rtl.BR ll) =  emit (BASE (BR ll))

     | translate (Rtl.BCNDI2 (comparison, rtl_Rsrc1, rtl_Rsrc2, loc_label, pre)) =
       let val rtl_tmp =  Rtl.REGI(Name.fresh_var(),Rtl.NOTRACE_INT)
       in 
	 translate (Rtl.CMPSI(comparison,rtl_Rsrc1,rtl_Rsrc2,rtl_tmp));
         translate(Rtl.BCNDI (Rtl.NE, rtl_tmp, loc_label, pre))
       end
     | translate (Rtl.BCNDF2 (comparison, rtl_Fsrc1, rtl_Fsrc2, loc_label, pre)) =
       let 
	 val rtl_Ftemp = Rtl.REGF(Name.fresh_var(),Rtl.NOTRACE_REAL)
	 val Ftemp = translateFReg rtl_Ftemp
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
	 val (fop,reverse_operand, reverse_result) = 
		(case comparison of
		   Rtl.EQ =>  (CMPTEQ, false, false)
		 | Rtl.LE =>  (CMPTLE, false, false)
		 | Rtl.LT =>  (CMPTLT, false, false)
		 | Rtl.GE =>  (CMPTLE, true, false)
		 | Rtl.GT =>  (CMPTLT, true, false)
		 | Rtl.NE =>  (CMPTEQ, false, true)
		 | _ => error ("translate/CMPF: bad comparison"))
       in 
	 if (not reverse_operand)
	   then emit (SPECIFIC (FPOP(fop, Fsrc1, Fsrc2, Ftemp)))
	 else emit (SPECIFIC (FPOP(fop, Fsrc2, Fsrc1, Ftemp)));
	 translate(Rtl.BCNDF (if (not reverse_result) then Rtl.NE else Rtl.EQ, 
				  rtl_Ftemp, loc_label, pre))
       end

     | translate (Rtl.BCNDI (comparison, rtl_Rsrc, ll, _)) =
       let
	 val Rsrc = translateIReg rtl_Rsrc
       in
          case comparison of
	    Rtl.EQ  => emit (SPECIFIC (CBRANCHI (BEQ,  Rsrc, ll)))
	  | Rtl.LE  => emit (SPECIFIC (CBRANCHI (BLE,  Rsrc, ll)))
	  | Rtl.LT  => emit (SPECIFIC (CBRANCHI (BLT,  Rsrc, ll)))
	  | Rtl.GE  => emit (SPECIFIC (CBRANCHI (BGE,  Rsrc, ll)))
	  | Rtl.GT  => emit (SPECIFIC (CBRANCHI (BGT,  Rsrc, ll)))
	  | Rtl.NE  => emit (SPECIFIC (CBRANCHI (BNE,  Rsrc, ll)))
	  | Rtl.LBC => emit (SPECIFIC (CBRANCHI (BLBC, Rsrc, ll)))
	  | Rtl.LBS => emit (SPECIFIC (CBRANCHI (BLBS, Rsrc, ll)))
       end

     | translate (Rtl.BCNDF (comparison, rtl_Fsrc, ll, _)) =
       let
	 val Fsrc = translateFReg rtl_Fsrc
       in
          (case comparison of
	     Rtl.EQ => emit (SPECIFIC (CBRANCHF(FBEQ, Fsrc, ll)))
           | Rtl.LE => emit (SPECIFIC (CBRANCHF(FBLE, Fsrc, ll)))
           | Rtl.LT => emit (SPECIFIC (CBRANCHF(FBLT, Fsrc, ll)))
           | Rtl.GE => emit (SPECIFIC (CBRANCHF(FBGE, Fsrc, ll)))
           | Rtl.GT => emit (SPECIFIC (CBRANCHF(FBGT, Fsrc, ll)))
           | Rtl.NE => emit (SPECIFIC (CBRANCHF(FBNE, Fsrc, ll)))
           | _ => error ("translate/BCNDF: bad comparison"))
       end

     | translate (Rtl.JMP (rtl_Raddr, rtllabels)) =
       let
	 val Raddr = translateIReg rtl_Raddr
       in (* JMP must first restore callee-save registers first *)
(*	 emit (BASE (MOVI (Raddr,Rpv)));
	 emit (BASE (JSR (false, Raddr, 1, loclabels)))
*)
	   emit (BASE (RTL (JMP (Raddr, rtllabels))))
       end

     | translate (Rtl.CALL {call_type, func, args, results, ...}) =
       let
	   val func = (case func of
			   Rtl.REG' rtl_Raddr => INDIRECT(translateIReg rtl_Raddr)
			 | Rtl.LABEL' l => DIRECT(l, NONE))
       in
	   (case call_type of
		Rtl.C_NORMAL =>
		    emit(BASE(RTL (CALL{calltype = Rtl.C_NORMAL,
					func = DIRECT(Rtl.C_EXTERN_LABEL "save_iregs", NONE),
					args = [],
					results = [],
					argregs = NONE,
					resregs = NONE,
					destroys = NONE})))
	      | _ => ());

	  emit (BASE(RTL (CALL{calltype = call_type,
			       func     = func,
			       args     = map translateReg args,
			       results  = map translateReg results,
			       argregs  = NONE,
			       resregs  = NONE,
			       destroys = NONE})));

	   (case call_type of
		Rtl.C_NORMAL =>
		    emit(BASE(RTL (CALL{calltype = Rtl.C_NORMAL,
					func = DIRECT(Rtl.C_EXTERN_LABEL "load_iregs", NONE),
					args = [],
					results = [],
					argregs = NONE,
					resregs = NONE,
					destroys = NONE})))
	      | _ => ());

	   (* Have a return for tailcalls in case we cannot do tailcalls from overflowing arguments *)
	   (case call_type of
		Rtl.ML_TAIL _ => emit (BASE (RTL (RETURN {results = !current_res})))
	      | _ => ())
       end


     | translate (Rtl.RETURN rtl_Raddr) =
          emit (BASE (RTL (RETURN {results = ! current_res})))

     | translate (Rtl.SAVE_CS l) = emit (BASE (RTL (SAVE_CS l)))

     | translate (Rtl.END_SAVE) = ()
     | translate (Rtl.RESTORE_CS) = ()
     | translate (Rtl.LOAD32I (Rtl.EA (rtl_Raddr, disp), rtl_Rdest)) =
       let
	 val Raddr = translateIReg rtl_Raddr
	 val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (LOADI (LDL, Rdest, disp, Raddr)))
       end

     | translate (Rtl.STORE32I (Rtl.EA (rtl_Raddr, disp), rtl_Rdest)) =
       let
	 val Raddr = translateIReg rtl_Raddr
	 val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (STOREI (STL, Rdest, disp, Raddr)))
       end


     | translate (Rtl.LOADQF (Rtl.EA (rtl_Raddr, disp), rtl_Fdest)) =
       let
	 val Raddr = translateIReg rtl_Raddr
	 val Fdest = translateFReg rtl_Fdest
       in
	 emit (SPECIFIC (LOADF (LDT, Fdest, disp, Raddr)))
       end

     | translate (Rtl.STOREQF (Rtl.EA (rtl_Raddr, disp), rtl_Fsrc)) =
       let
	 val Raddr = translateIReg rtl_Raddr
	 val Fsrc  = translateFReg rtl_Fsrc
       in
	 emit (SPECIFIC (STOREF (STT, Fsrc, disp, Raddr)))
       end

     | translate (Rtl.ICOMMENT str) = emit (BASE(ICOMMENT str))

     | translate (Rtl.MUTATE (Rtl.EA(base,disp),newval,isptr_opt)) =
	 let 
	     fun logwrite() = 
		 let val writelist_cursor = Rtl.ML_EXTERN_LABEL "writelist_cursor"
		     val cursor_addr = Rtl.REGI(Name.fresh_var(),Rtl.NOTRACE_LABEL)
		     val cursor_val = Rtl.REGI(Name.fresh_var(),Rtl.LOCATIVE)
		     val store_loc = Rtl.REGI(Name.fresh_var(),Rtl.LOCATIVE)
		 in  emit (SPECIFIC (INTOP (SUBL, Rhlimit, IMMop 8, Rhlimit)));
		     app translate [Rtl.LADDR(writelist_cursor,0,cursor_addr),
				    Rtl.LOAD32I(Rtl.EA(cursor_addr,0),cursor_val),
				    Rtl.ADD(base, Rtl.IMM disp,store_loc),
				    Rtl.STORE32I(Rtl.EA(cursor_val,0),store_loc),
				    Rtl.ADD(cursor_val, Rtl.IMM 4, cursor_val),
				    Rtl.STORE32I(Rtl.EA(cursor_addr,0),cursor_val)]
		 end
	 in
	   translate (Rtl.STORE32I(Rtl.EA(base,disp),newval));
	   (case isptr_opt of
		NONE => logwrite()
	      | SOME isptr =>
		    let val after = Rtl.fresh_code_label "dynmutate_after"
		    in  translate (Rtl.BCNDI(Rtl.EQ,isptr,after,false));
			logwrite();
			translate (Rtl.ILABEL after)
		    end)
	 end

     | translate (Rtl.INIT (Rtl.EA (rtl_Raddr, disp), rtl_Rdest, unused)) =
       let
	 val Raddr = translateIReg rtl_Raddr
	 val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (STOREI (STL, Rdest, disp, Raddr)))
       end

     | translate (Rtl.NEEDGC (Rtl.REG rtl_Rsize)) =
       let
	 val Rsize = translateIReg rtl_Rsize
	 val rtl_loclabel = Rtl.fresh_code_label "needgc"
       in
	 emit (SPECIFIC (INTOP   (S4ADDL, Rsize, REGop Rheap, Rat)));
	 emit (SPECIFIC (INTOP   (CMPULE, Rhlimit, REGop Rat, Rat)));
	 emit (SPECIFIC (CBRANCHI(BEQ, Rat, rtl_loclabel)));
	 emit (SPECIFIC (INTOP   (S4ADDL, Rsize, REGop Rzero, Rhlimit)));
	 emit (BASE (GC_CALLSITE rtl_loclabel));
	 emit (BASE (BSR (Rtl.ML_EXTERN_LABEL ("gc_raw"), NONE,
			     {regs_modified=[Rat], regs_destroyed=[Rat],
			      args=[Rat]})));
	 translate (Rtl.ILABEL rtl_loclabel)
       end

     | translate (Rtl.NEEDGC (Rtl.IMM 0)) = ()
     | translate (Rtl.NEEDGC (Rtl.IMM words)) =
       let
	 val rtl_Itemp = Rtl.REGI(Name.fresh_var(),Rtl.NOTRACE_INT)
	 val Itemp = translateIReg rtl_Itemp
	 val rtl_loclabel = Rtl.fresh_code_label "needgc"
	 val size = 4 * words
       in
	 if (in_ea_disp_range size) then
	   emit (SPECIFIC (LOADI (LDA, Rat, size, Rheap)))
	 else
	   (load_imm(i2w size, Rat);
	    emit (SPECIFIC (INTOP (ADDL, Rheap, REGop Rat, Rheap))));
	 emit (SPECIFIC (INTOP   (CMPULE, Rhlimit, REGop Rat, Rat)));
	 emit (SPECIFIC (CBRANCHI(BEQ, Rat, rtl_loclabel)));
	 load_imm(i2w size, Rhlimit);
	 emit (BASE (GC_CALLSITE rtl_loclabel));
	 emit (BASE (BSR (Rtl.ML_EXTERN_LABEL ("gc_raw"), NONE,
			  {regs_modified=[Rat], regs_destroyed=[Rat],
			   args=[Rat]})));
	 translate (Rtl.ILABEL rtl_loclabel)
       end

     | translate (Rtl.SOFT_VBARRIER _) = emit (SPECIFIC TRAPB)
     | translate (Rtl.SOFT_ZBARRIER _) = emit (SPECIFIC TRAPB)
     | translate (Rtl.HARD_VBARRIER _) = ()
     | translate (Rtl.HARD_ZBARRIER _) = ()

     | translate (Rtl.IALIGN x) = ()
(*
          emit(IALIGN (case x
		  of Rtl.OCTA => OCTA
		   | Rtl.QUAD => QUAD
		   | Rtl.LONG => LONG
		   | _ => error "unexpected alignment in instruction stream"))
*)
     | translate (Rtl.ILABEL ll) = 
          (* Begin new basic block *)
          (saveBlock ();
	   resetBlock ll true true)

	  
     | translate Rtl.HALT = 
       (* HALT is a no-op from the translator's point of view *)
          ()


       (* HANDLER_ENTRY: the start of handler. *)

     | translate Rtl.HANDLER_ENTRY =  
	 (* Fix $gp *)
	 (emit (BASE (RTL HANDLER_ENTRY));
	  emit (SPECIFIC (LOADI(LDGP, Rgp, 0, Rpv))))
	  

   (* Translates an entire Rtl procedure *)
   fun translateProc (Rtl.PROC {name, args, code, known, results, return, 
				save = _, vars = _}) =
     let
       val args   = map translateReg args
       val res    = map translateReg results
       val return = translateIReg return
     in
       (* initialization *)
       tracemap := Regmap.empty;
       block_map := Labelmap.empty;
       current_blocklabels := [];
       current_proc := name;
       current_res := res;
       init_stack_res();
       
       (* Create (empty) preamble block with same name as the procedure *)
       resetBlock name true false;
       saveBlock ();

       (* Start a new block *)
       resetBlock (freshCodeLabel()) true true;

       (* Translate instructions *)
       (Array.app 
	(fn arg => ((translate arg)
		    handle e => (print "exn raised during translation of Rtl instruction:\n  ";
				 Pprtl.pp_Instr arg;
				 raise e)))
	code);
       
       (* Flush last block *)
       saveBlock ();

       (* Return blocklabels with blocks in the SAME order as in
          the Rtl code, and the associated block_map *)
       (rev (! current_blocklabels), ! block_map, ! tracemap, get_stack_res())
     end

   (* For reasons of simplicity, the datatype for Rtl data is
      the same as the datatype for Decalpha data.  Hence the
      translation of the data is really easy---remove it from
      the array. *)
   fun array2list a = 
       let val len = Array.length a
	   fun loop n = if (n >= len) then []
			else (Array.sub(a,n))::(loop (n+1))
       in  loop 0
       end
(*   fun translateData data_array = array2list data_array *)


end (* struct *)
