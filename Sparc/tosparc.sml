(*$import Sparc MACHINEUTILS TRACETABLE BBLOCK TOASM Util Pprtl *)

(* WARNING: Use Rat or Rat2 only if sure that no spills (or at most one if one of Rat/Rat2 is used)
              will cause usage of Rat/Rat2 during the live range of Rat/Rat2.
   THREAD_UNSAFE ERROR XXX: don't use FPTOFROMINT; use thread-specific slot
*)

(* Translation from Rtl to Sparc pseudoregister-assembly. 
   Assumptions:
     (1) The thread pointer points to a structure where the first 32 longs
           store the saved general purpose registers.  In particular, when
	   making C calls, the allocation pointer and allocation limit are 
	   saved at offset given by the 8 times the register's number.

*)
         
functor Tosparc(structure Machineutils : MACHINEUTILS 
	        structure ArgTracetable : TRACETABLE 
                structure Bblock : BBLOCK
		    where type Machine.specific_instruction = Sparc.specific_instruction
		    where type Machine.instruction = Sparc.Machine.instruction
		sharing ArgTracetable = Bblock.Tracetable)
  :> TOASM where Machine = Sparc.Machine
	   where Bblock = Bblock
	   where Tracetable = ArgTracetable
= 
struct

   structure Bblock = Bblock
   structure Sparc = Sparc
   structure Machineutils = Machineutils
   structure Tracetable = ArgTracetable
   structure W = TilWord32
   val i2w = W.fromInt
   val w2i = W.toInt
       
   open Machineutils Bblock
   open Sparc
   open Machine
   open Core

   fun error s = Util.error "tosparc.sml" s

   (* Translate the two RTL register types into the single
      SPARC register type.  *)

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

   fun translateSReg Rtl.HEAPALLOC = Rheap
     | translateSReg Rtl.HEAPLIMIT = Rhlimit
     | translateSReg Rtl.STACK = Rsp
     | translateSReg Rtl.THREADPTR = Rth
     | translateSReg Rtl.EXNSTACK = Rexnptr
     | translateSReg Rtl.EXNARG = Rexnarg
     | translateSReg Rtl.HANDLER = Rhandler
     
   fun translateIReg (Rtl.REGI (v, rep)) = 	
       (tracemap := Regmap.insert(!tracemap,R (Name.var2int v),internal_translateRep v rep);
	R (Name.var2int v))
     | translateIReg (Rtl.SREGI s) = translateSReg s
     
   fun translateFReg (Rtl.REGF (v, _)) = F (2 * (Name.var2int v))


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
	   IMMop (INT src2)
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
       let fun branch_case (fallthrough, succ_labels) =
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
	   end
       in  (current_instrs := (NO_ANN instr) :: (! current_instrs);
	    case (Sparc.Machine.cFlow instr) of 
		NOBRANCH => ()
	      | BRANCH info => branch_case info
	      | DELAY_BRANCH info => (current_instrs := (NO_ANN (SPECIFIC NOP)) :: (! current_instrs);
				      branch_case info))
       end


   (* Translate an RTL instruction into one or more Alpha instructions *)
   fun load_imm (immed, Rdest) =
          let  (* SETHI sets the upper 22 bits and zeroes the low 10 bits; 
		  OR can take a 13-bit signed immediate *)
	      val high20 = w2i(W.rshifta(immed, 12))
	      val high22  = w2i(W.rshifta(immed, 10))
	      val low10   = w2i(W.andb(immed, 0w1023))
	      val high22op = HIGHINT immed
	      val low10op = LOWINT immed
	  in
	      if (high20 = 0 orelse high20 = ~1)
		 then let val low13op = INT (w2i immed)  (* assumes upper 22 bits all set or all clear *)
		      in  emit(SPECIFIC(INTOP(OR,Rzero,IMMop low13op, Rdest)))
		      end
	      else (emit(SPECIFIC(SETHI(high22op,Rdest)));
		    if (low10 = 0)
			 then ()
	 	     else (emit(SPECIFIC(INTOP(OR,Rdest,IMMop low10op, Rdest)))))
	  end

   fun translate_icmp Rtl.EQ = BE
     | translate_icmp Rtl.NE = BNE
     | translate_icmp Rtl.GT = BG
     | translate_icmp Rtl.GE = BGE
     | translate_icmp Rtl.LT = BL
     | translate_icmp Rtl.LE = BLE

   fun negate_icmp BE  = BNE
     | negate_icmp BNE = BE
     | negate_icmp BG  = BLE
     | negate_icmp BGE = BL
     | negate_icmp BL  = BGE
     | negate_icmp BLE = BG
     | negate_icmp BLEU = BGU
     | negate_icmp BGU = BLEU
     | negate_icmp BCC = BCS
     | negate_icmp BCS = BCC

   fun translate_fcmp Rtl.EQ = FBE
     | translate_fcmp Rtl.NE = FBNE
     | translate_fcmp Rtl.GT = FBG
     | translate_fcmp Rtl.GE = FBGE
     | translate_fcmp Rtl.LT = FBL
     | translate_fcmp Rtl.LE = FBLE

   fun loadEA' destOpt ea = 
       (case ea of
	    Rtl.REA(rtlBase, disp) => (translateIReg rtlBase, INT disp)
	  | Rtl.LEA(label, disp) => let val base = freshIreg()
				    in  emit(SPECIFIC(SETHI(HIGHLABEL (label,i2w disp), base)));
					(base, LOWLABEL (label,i2w disp))
				    end
	  | Rtl.RREA(r1, r2) => let val Rsrc1 = translateIReg r1
				    val Rsrc2 = translateIReg r2
				    val dest = (case destOpt of
						    NONE => translateIReg
							(Rtl.REGI(Name.fresh_var(),Rtl.LOCATIVE))
						  | SOME d => d)
				    val _ = emit (SPECIFIC(INTOP (ADD, Rsrc1, REGop Rsrc2, dest)))
				in  (dest, INT 0)
				end)

   val loadEA = loadEA' NONE

   fun translate (Rtl.LI (immed, rtl_Rdest)) = load_imm(immed,translateIReg rtl_Rdest)
     | translate (Rtl.LADDR (ea, rtl_Rdest)) =
          let val Rdest = translateIReg rtl_Rdest
	      val (Rbase,disp) = loadEA' (SOME Rdest) ea
	      val dispZero = (case disp of
				  LOWINT 0w0 => true
				| INT 0 => true
				| _ => false)
	  in  if dispZero
		  then (if eqRegs'(Rbase,Rdest) 
			    then ()
			else emit (BASE(MOVE (Rbase, Rdest))))
	      else emit (SPECIFIC(INTOP (OR, Rbase, IMMop disp, Rdest)))
	  end

     | translate (Rtl.MV (rtl_Rsrc, rtl_Rdest)) =
          emit (BASE(MOVE (translateIReg rtl_Rsrc, translateIReg rtl_Rdest)))

     | translate (Rtl.CMV (rtl_cmp, rtl_Rsrc1, op2, rtl_Rdest)) =
       let 
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val Rop2 = translateOp op2
	 val Rdest = translateIReg rtl_Rdest
         val label = Rtl.fresh_code_label "cmv"
         val cmp = translate_icmp rtl_cmp
       in emit(SPECIFIC(CMP (Rsrc1, IMMop (INT 0))));
	  emit(SPECIFIC(CBRANCHI(negate_icmp cmp,label)));
	  emit(SPECIFIC NOP);
	  emit(SPECIFIC(INTOP(OR, Rzero, Rop2, Rdest)));
          translate(Rtl.ILABEL label)
       end

     | translate (Rtl.FMV (rtl_Fsrc, rtl_Fdest)) =
       let
	 val Fsrc = translateFReg rtl_Fsrc
	 val Fdest = translateFReg rtl_Fdest
       in
	 emit (BASE (MOVE (Fsrc,Fdest)))
       end

     | translate (Rtl.ADD (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (ADD, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.SUB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SUB, Rsrc1, src2, Rdest)))
       end
     | translate (Rtl.MUL (rtl_Rsrc1, op2 as (Rtl.IMM denom), rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val Rdest = translateIReg rtl_Rdest
	 val src2  = translateOp op2
       in emit (SPECIFIC(INTOP (SMUL, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.MUL (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SMUL, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.UDIV (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val src2 = translateOp op2
	 val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(WRY  Rzero));
	 emit (SPECIFIC(INTOP (UDIV, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.UMOD (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val rtl_Rsrc2 = 
		(case op2 of
			Rtl.REG r => r
		      | Rtl.IMM n => 
			let val temp = Rtl.REGI(Name.fresh_var(), Rtl.NOTRACE_INT)
			    val _ = translate(Rtl.LI(i2w n,temp))
			in  temp
			end)
       in
	 translate(Rtl.CALL{call_type = Rtl.C_NORMAL,
			    func = Rtl.LABEL' (Rtl.ML_EXTERN_LABEL ".urem"),
			    args = [Rtl.I rtl_Rsrc1, Rtl.I rtl_Rsrc2],
			    results = [Rtl.I rtl_Rdest],
			    save = []})
       end

     | translate (Rtl.S4ADD (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SLL, Rsrc1, IMMop (INT 2), Rat)));
	 emit (SPECIFIC(INTOP (ADD, Rat, src2, Rdest)))
       end

     | translate (Rtl.S8ADD (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SLL, Rsrc1, IMMop (INT 3), Rat)));
	 emit (SPECIFIC(INTOP (ADD, Rat, src2, Rdest)))
       end

     | translate (Rtl.S4SUB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SLL, Rsrc1, IMMop (INT 2), Rat)));
	 emit (SPECIFIC(INTOP (SUB, Rat, src2, Rdest)))
       end

     | translate (Rtl.S8SUB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SLL, Rsrc1, IMMop (INT 3), Rat)));
	 emit (SPECIFIC(INTOP (SUB, Rat, src2, Rdest)))
       end

     | translate (Rtl.ADDT (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (ADDCC, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.SUBT (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SUBCC, Rsrc1, src2, Rdest)))
       end


     | translate (Rtl.MULT (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SMULCC, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.DIVT (rtl_Rsrc1, op2, rtl_Rdest)) = 
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val src2 = translateOp op2
	 val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC(INTOP (SRA, Rsrc1, IMMop (INT 31), Rat)));
	 emit (SPECIFIC(WRY   Rat));
	 emit (SPECIFIC(INTOP (SDIV, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.MODT (rtl_Rsrc1, op2, rtl_Rdest)) = 
       let
	 val rtl_Rsrc2 = 
		(case op2 of
			Rtl.REG r => r
		      | Rtl.IMM n => 
			let val temp = Rtl.REGI(Name.fresh_var(), Rtl.NOTRACE_INT)
			    val _ = translate(Rtl.LI(i2w n,temp))
			in  temp
			end)
       in
	 translate(Rtl.CALL{call_type = Rtl.C_NORMAL,
			    func = Rtl.LABEL' (Rtl.ML_EXTERN_LABEL ".rem"),
			    args = [Rtl.I rtl_Rsrc1, Rtl.I rtl_Rsrc2],
			    results = [Rtl.I rtl_Rdest],
			    save = []})
       end

     | translate (Rtl.CMPSI (cmp, rtl_Rsrc1, rtl_op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val op2 = translateOp rtl_op2
	 val Rdest = translateIReg rtl_Rdest
         val br = (case cmp of
		   Rtl.EQ =>  BE
		 | Rtl.NE =>  BNE
		 | Rtl.GT =>  BG
		 | Rtl.GE =>  BGE
		 | Rtl.LT =>  BL
		 | Rtl.LE =>  BLE)
         val label = Rtl.fresh_code_label "cmpsi"
       in emit(SPECIFIC(CMP(Rsrc1, op2)));
	  load_imm(0w1, Rdest);
	  emit(SPECIFIC(CBRANCHI(br,label)));
	  load_imm(0w0, Rdest);
          translate(Rtl.ILABEL label)
       end

     | translate (Rtl.CMPUI (cmp, rtl_Rsrc1, rtl_op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
	 val op2 = translateOp rtl_op2
	 val Rdest = translateIReg rtl_Rdest
         val br = (case cmp of
		   Rtl.EQ =>  BE
		 | Rtl.NE =>  BNE
		 | Rtl.GT =>  BGU
		 | Rtl.GE =>  BCC
		 | Rtl.LT =>  BCS
		 | Rtl.LE =>  BLEU)
         val label = Rtl.fresh_code_label "cmpui"
       in emit(SPECIFIC(CMP(Rsrc1, op2)));
	  load_imm(0w1, Rdest);
	  emit(SPECIFIC(CBRANCHI(br,label)));
	  load_imm(0w0, Rdest);
          translate(Rtl.ILABEL label)
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

     | translate (Rtl.ANDNOTB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (INTOP (ANDNOT, Rsrc1, src2, Rdest)))
       end

     | translate (Rtl.ORNOTB (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (INTOP (ORNOT, Rsrc1, src2, Rdest)))
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
	 emit (SPECIFIC (INTOP (ADD, Rdest, REGop Rzero, Rdest)))
       end

     | translate (Rtl.SRL (rtl_Rsrc1, op2, rtl_Rdest)) =
       let
	 val Rsrc1 = translateIReg rtl_Rsrc1
         val src2  = translateOp op2
         val Rdest = translateIReg rtl_Rdest
       in
	 emit (SPECIFIC (INTOP (SRL, Rsrc1, src2, Rdest)))
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
          emit (SPECIFIC (FPOP (FADDD, Fsrc1, Fsrc2, Fdest)))
       end

     | translate (Rtl.FSUBD (rtl_Fsrc1, rtl_Fsrc2, rtl_Fdest)) =
       let
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val Fdest = translateFReg rtl_Fdest
       in
          emit (SPECIFIC (FPOP (FSUBD, Fsrc1, Fsrc2, Fdest)))
       end

     | translate (Rtl.FMULD (rtl_Fsrc1, rtl_Fsrc2, rtl_Fdest)) =
       let
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val Fdest = translateFReg rtl_Fdest
       in
          emit (SPECIFIC (FPOP (FMULD, Fsrc1, Fsrc2, Fdest)))
       end

     | translate (Rtl.FDIVD (rtl_Fsrc1, rtl_Fsrc2, rtl_Fdest)) =
       let
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val Fdest = translateFReg rtl_Fdest
       in
          emit (SPECIFIC (FPOP (FDIVD, Fsrc1, Fsrc2, Fdest)))
       end

     | translate (Rtl.FABSD (rtl_Fsrc, rtl_Fdest)) =
       let
	 val Fsrc  = translateFReg rtl_Fsrc
         val Fdest = translateFReg rtl_Fdest
       in
	 emit (SPECIFIC (FPMOVE (FABSD, Fsrc, Fdest)))
       end

     | translate (Rtl.FNEGD (rtl_Fsrc, rtl_Fdest)) =
       let
	 val Fsrc  = translateFReg rtl_Fsrc
         val Fdest = translateFReg rtl_Fdest
       in
	 emit (SPECIFIC (FPMOVE (FNEGD, Fsrc, Fdest)))
       end


     | translate (Rtl.CVT_REAL2INT (rtl_Fsrc, rtl_Rdest)) =
       let
	 val Fsrc  = translateFReg rtl_Fsrc
         val Rdest = translateIReg rtl_Rdest
	 val scratch = INT threadScratch_disp
       in
	   emit (SPECIFIC (FPMOVE(FDTOI, Fsrc, Fat)));
	   emit (SPECIFIC (STOREF (STF, Fat, scratch, Rth)));
	   emit (SPECIFIC (LOADI (LD, Rdest, scratch, Rth)))
       end

     | translate (Rtl.CVT_INT2REAL (rtl_Rsrc, rtl_Fdest)) =
       let
	 val Rsrc  = translateIReg rtl_Rsrc
         val Fdest = translateFReg rtl_Fdest
	 val scratch = INT threadScratch_disp
       in
	   emit (SPECIFIC (STOREI (ST, Rsrc, scratch, Rth)));
	   emit (SPECIFIC (LOADF  (LDF, Fat, scratch, Rth)));
	   emit (SPECIFIC (FPMOVE (FITOD, Fat, Fdest)))
       end

 
     | translate (Rtl.CMPF (cmp, rtl_Fsrc1, rtl_Fsrc2, rtl_Rdest)) =
       let
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val Rdest = translateIReg rtl_Rdest
	 val label = Rtl.fresh_code_label "cmpf"
         val br = (case cmp of
		   Rtl.EQ =>  FBE
		 | Rtl.NE =>  FBNE
		 | Rtl.GT =>  FBG
		 | Rtl.GE =>  FBGE
		 | Rtl.LT =>  FBL
		 | Rtl.LE =>  FBLE)
       in emit(SPECIFIC(FCMPD(Fsrc1, Fsrc2)));
	  load_imm(0w1, Rdest);
	  emit(SPECIFIC(CBRANCHF(br,label)));
	  load_imm(0w0, Rdest);
          translate(Rtl.ILABEL label)
       end

     | translate (Rtl.BR ll) =  emit (BASE (BR ll))

     | translate (Rtl.BCNDI (comparison, rtl_Rsrc1, rtl_op2, loc_label, pre)) =
       let val Rsrc1 = translateIReg rtl_Rsrc1
	   val op2 = translateOp rtl_op2
	   val cmp = translate_icmp comparison
       in 
	   emit(SPECIFIC(CMP(Rsrc1,op2)));
	   emit(SPECIFIC(CBRANCHI(cmp, loc_label)))
       end

     | translate (Rtl.BCNDF (cmp, rtl_Fsrc1, rtl_Fsrc2, loc_label, pre)) =
       let 
	 val Fsrc1 = translateFReg rtl_Fsrc1
         val Fsrc2 = translateFReg rtl_Fsrc2
         val cmp = translate_fcmp cmp
       in 
	 emit (SPECIFIC (FCMPD(Fsrc1, Fsrc2)));
	 emit (SPECIFIC (CBRANCHF(cmp, loc_label)))
       end


     | translate (Rtl.JMP (rtl_Raddr, rtllabels)) =
       let
	 val Raddr = translateIReg rtl_Raddr
       in
	   emit (BASE (RTL (JMP (Raddr, rtllabels))))
       end

     | translate (Rtl.CALL {call_type, func, args, results, ...}) =
       let
	   val func = (case func of
			   Rtl.REG' rtl_Raddr => INDIRECT(translateIReg rtl_Raddr)
			 | Rtl.LABEL' l => DIRECT(l, NONE))
       in

	   (case (call_type, length args <= length indirect_int_args) of
		(Rtl.ML_TAIL _, false) =>
		    (emit (BASE(RTL (CALL{calltype = Rtl.ML_NORMAL,
					  func     = func,
					  args     = map translateReg args,
					  results  = map translateReg results,
					  argregs  = NONE,
					  resregs  = NONE,
					  destroys = NONE})));
		     emit (BASE (RTL (RETURN {results = !current_res}))))
	      | _ => 
		    emit (BASE(RTL (CALL{calltype = call_type,
					 func     = func,
					 args     = map translateReg args,
					 results  = map translateReg results,
					 argregs  = NONE,
					 resregs  = NONE,
					 destroys = NONE}))))
       end


     | translate (Rtl.RETURN rtl_Raddr) =
          emit (BASE (RTL (RETURN {results = ! current_res})))

     | translate (Rtl.PUSH_EXN) = ()
     | translate (Rtl.POP_EXN) = ()
     | translate (Rtl.THROW_EXN) = ()
     | translate (Rtl.CATCH_EXN) = 
	  let val tmp1 =  Rtl.REGI(Name.fresh_var(), Rtl.NOTRACE_INT)
	      val tmp2 =  Rtl.REGI(Name.fresh_var(), Rtl.NOTRACE_INT)
	  in  translate(Rtl.LOAD32I(Rtl.REA(Rtl.SREGI Rtl.THREADPTR,maxsp_disp),tmp1));
	      translate(Rtl.CMPUI(Rtl.GT,Rtl.SREGI Rtl.STACK,Rtl.REG tmp1,tmp2));
	      translate(Rtl.CMV(Rtl.NE,tmp2,Rtl.REG(Rtl.SREGI Rtl.STACK), tmp1));
	      translate(Rtl.STORE32I(Rtl.REA(Rtl.SREGI Rtl.THREADPTR,maxsp_disp),tmp1))
	  end

     | translate (Rtl.LOAD8I (ea, rtl_Rdest)) =
       let val (Raddr, disp) = loadEA ea
	   val Rdest = translateIReg rtl_Rdest
       in  emit (SPECIFIC (LOADI (LDUB, Rdest, disp, Raddr)))
       end

     | translate (Rtl.STORE8I (ea, rtl_Rdest)) =
       let val (Raddr, disp) = loadEA ea
	   val Rdest = translateIReg rtl_Rdest
       in  emit (SPECIFIC (STOREI (STUB, Rdest, disp, Raddr)))
       end

     | translate (Rtl.LOAD32I (ea, rtl_Rdest)) =
       let val (Raddr, disp) = loadEA ea
	   val Rdest = translateIReg rtl_Rdest
       in  emit (SPECIFIC (LOADI (LD, Rdest, disp, Raddr)))
       end

     | translate (Rtl.STORE32I (ea, rtl_Rdest)) =
       let val (Raddr, disp) = loadEA ea
	   val Rdest = translateIReg rtl_Rdest
       in  emit (SPECIFIC (STOREI (ST, Rdest, disp, Raddr)))
       end

     | translate (Rtl.LOAD64F (ea, rtl_Fdest)) =
       let val (Raddr, disp) = loadEA ea
	   val Fdest = translateFReg rtl_Fdest
       in emit (SPECIFIC (LOADF (LDDF, Fdest, disp, Raddr)))
       end

     | translate (Rtl.STORE64F (ea, rtl_Fsrc)) =
       let val (Raddr, disp) = loadEA ea
	   val Fsrc = translateFReg rtl_Fsrc
       in  emit (SPECIFIC (STOREF (STDF, Fsrc, disp, Raddr)))
       end

     | translate (Rtl.ICOMMENT str) = emit (BASE(ICOMMENT str))

     | translate (Rtl.STOREMUTATE ea) =
	   let val writeAlloc = Rtl.REGI(Name.fresh_var(),Rtl.LOCATIVE)
	       val writeAllocTemp = Rtl.REGI(Name.fresh_var(),Rtl.LOCATIVE)
	       val store_obj = Rtl.REGI(Name.fresh_var(),Rtl.TRACE)
	       val store_disp = Rtl.REGI(Name.fresh_var(),Rtl.NOTRACE_INT)
	   in  emit (SPECIFIC(LOADI (LD, translateIReg writeAlloc, INT writelistAlloc_disp, Rth)));
	       app translate
	       ((case ea of
		     Rtl.REA (r, i) => [Rtl.MV (r, store_obj), Rtl.LI(TilWord32.fromInt i, store_disp)]
		   | Rtl.LEA (l, i) => [Rtl.LADDR (Rtl.LEA(l, 0), store_obj), Rtl.LI(TilWord32.fromInt i, store_disp)]
		   | Rtl.RREA (r1, r2) => [Rtl.MV (r1, store_obj), Rtl.MV (r2, store_disp)]) @
		     [Rtl.STORE32I(Rtl.REA(writeAlloc,0),store_obj),
		      Rtl.STORE32I(Rtl.REA(writeAlloc,4),store_disp),
		      Rtl.ADD(writeAlloc, Rtl.IMM 8, writeAllocTemp)]);
	       emit (SPECIFIC(STOREI (ST, translateIReg writeAllocTemp,INT writelistAlloc_disp, Rth)))
	   end

     | translate (Rtl.NEEDMUTATE n) =
       let val writeAlloc = Rtl.REGI(Name.fresh_var(),Rtl.LOCATIVE)
	   val writeLimit = Rtl.REGI(Name.fresh_var(),Rtl.LOCATIVE)
	   val writeAllocTemp = Rtl.REGI(Name.fresh_var(),Rtl.LOCATIVE)
	   val afterLabel = Rtl.fresh_code_label "afterMutateCheck"
       in  emit (SPECIFIC(LOADI (LD, translateIReg writeAlloc, INT writelistAlloc_disp, Rth)));
	   emit (SPECIFIC(LOADI (LD, translateIReg writeLimit, INT writelistLimit_disp, Rth)));
	   translate (Rtl.ADD(writeAlloc, Rtl.IMM (8 * n), writeAllocTemp));
	   translate (Rtl.BCNDI(Rtl.LE, writeAllocTemp, Rtl.REG writeLimit, afterLabel, true));
	   emit (SPECIFIC (INTOP(SUB, Rheap, IMMop(INT(8 * n)), Rat)));
	   emit (BASE (GC_CALLSITE afterLabel));
	   emit (BASE (BSR (Rtl.ML_EXTERN_LABEL ("GCFromML"), NONE,
			    {regs_modified=[Rat], regs_destroyed=[Rat],
			     args=[Rat]})));
	   translate (Rtl.ILABEL afterLabel)
       end

     | translate (Rtl.NEEDALLOC (Rtl.IMM 0)) = ()
     | translate (Rtl.NEEDALLOC rtl_operand) =
       let
	   val rtl_loclabel = Rtl.fresh_code_label "needgc"
       in
	   (case rtl_operand of
		Rtl.REG rtl_Rsize => 
		    let val Rsize = translateIReg rtl_Rsize
		    in  emit (SPECIFIC (INTOP   (SLL, Rsize, IMMop (INT 2), Rat)));
			emit (SPECIFIC (INTOP   (ADD, Rat, REGop Rheap, Rat)))
		    end
	      | Rtl.IMM words => 
		    let val size = 4 * words
		    in  if (in_ea_disp_range size) 
			    then emit (SPECIFIC (INTOP (ADD, Rheap, IMMop (INT size), Rat)))
			else
			    (load_imm(i2w size, Rat);
			     emit (SPECIFIC (INTOP (ADD, Rheap, REGop Rat, Rat))))
		    end);
	  emit (SPECIFIC (LOADI (LD, Rhlimit, INT heapLimit_disp, Rth)));
	  emit (SPECIFIC (CMP     (Rat, REGop Rhlimit)));
	  emit (SPECIFIC (CBRANCHI(BLE, rtl_loclabel)));
	  emit (BASE (GC_CALLSITE rtl_loclabel));
	  emit (BASE (BSR (Rtl.ML_EXTERN_LABEL ("GCFromML"), NONE,
			   {regs_modified=[Rat], regs_destroyed=[Rat],
			    args=[Rat]})));
	  translate (Rtl.ILABEL rtl_loclabel)
       end


     | translate (Rtl.SOFT_VBARRIER _) = ()
     | translate (Rtl.SOFT_ZBARRIER _) = ()
     | translate (Rtl.HARD_VBARRIER _) = ()
     | translate (Rtl.HARD_ZBARRIER _) = ()

     | translate (Rtl.ILABEL ll) = 
          (* Begin new basic block *)
          (saveBlock ();
	   resetBlock ll true true)

	  
     | translate Rtl.HALT = 
       (* HALT is a no-op from the translator's point of view *)
          ()

   fun translateCode code = 
       let fun translate1 arg = ((translate arg)
				 handle e => (print "exn raised during translation of Rtl instruction:\n  ";
				 Pprtl.pp_Instr arg;
				 raise e))
       in  Array.app translate1 code
       end

   (* Translates an entire Rtl procedure *)
   fun translateProc (Rtl.PROC {name, args, code, results, return, 
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
       translateCode code;

       
       (* Flush last block *)
       saveBlock ();

       (* Return blocklabels with blocks in the SAME order as in
          the Rtl code, and the associated block_map *)
       (rev (! current_blocklabels), ! block_map, ! tracemap, get_stack_res())
     end

end (* struct *)
