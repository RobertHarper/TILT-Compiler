(*$import MACHINEUTILS DECALPHA Int32 Util *)

structure Decalphautils :> MACHINEUTILS =
struct

   open Decalpha
   open Machine
   open Core

   val error = fn s => Util.error "decalphautils.sml" s

   fun ireg n = R n
   fun freg n = F n
   fun listToSet lst = Regset.addList(Regset.empty, lst)
   fun setToList set = Regset.listItems set
   fun listdiff (a,b) = setToList(Regset.difference(listToSet a,listToSet b))
   fun listintersect (a,b) = setToList(Regset.intersection(listToSet a,listToSet b))
   fun listunion (a,b) = a @ b

   val min_ireg = ireg 0
   val min_freg = freg 0
   val max_ireg = ireg 21
   val max_freg = freg 28
   fun nextReg (reg as (R _)) = 
          if (eqRegs reg max_ireg) then min_ireg else (ireg(regNum reg +1))
     | nextReg (reg as (F _)) =
          if (eqRegs reg max_freg) then min_freg else (freg(regNum reg +1))

   fun prevReg (reg as (R _)) = 
          if (eqRegs reg min_ireg) then max_ireg else (ireg(regNum reg -1))
     | prevReg (reg as (F _)) =
          if (eqRegs reg min_freg) then max_freg else (freg(regNum reg -1))

   fun listToSet lst = Regset.addList(Regset.empty, lst)
   fun setToList set = Regset.listItems set

   fun regsBelow' (reg as (R _)) = 
       if (eqRegs reg min_ireg) then
	 [min_ireg]
       else if (regLE min_ireg reg) then 
	 (reg::(regsBelow' (prevReg reg))) 
       else []
     | regsBelow' (reg as (F _)) = 
       if (eqRegs reg min_freg) then
	 [min_freg]
       else if (regLE min_freg reg) then 
	 (reg::(regsBelow' (prevReg reg))) 
       else []

   val regsBelow = listToSet o regsBelow'

   fun regsAbove' (reg as (R _)) = 
       if (eqRegs reg max_ireg) then
	 [max_ireg]
       else if (regLE max_ireg reg) then 
	 (reg::(regsBelow' (prevReg reg))) 
       else []
     | regsAbove' (reg as (F _)) = 
       if (eqRegs reg max_freg) then
	 [max_freg]
       else if (regLE max_freg reg) then 
	 (reg::(regsBelow' (prevReg reg))) 
       else []

   val regsAbove = listToSet o regsAbove'


   val unsaved_regs  = [Rat, Rzero, Fat, Fzero, Rsp, Rat2, Fat2]

   val C_caller_saved_regs = 
     [ireg 0,  ireg 1,  ireg 2,  ireg 3,  ireg 4,  ireg 5, ireg 6, 
      ireg 7,  ireg 8,  ireg 16, ireg 17, ireg 18, ireg 19,
      ireg 20, ireg 21, ireg 22, ireg 23, ireg 24, ireg 25,
      ireg 26, ireg 27, ireg 29, 
      freg 0,  freg 1,  freg 2, 
      freg 10, freg 11, freg 12, freg 13, freg 14, freg 15,
      freg 16, freg 17, freg 18, freg 19, freg 20, freg 21,
      freg 22, freg 23, freg 24, freg 25, freg 26, freg 27,
      freg 28, freg 29, freg 30]

   val save_across_C = listintersect([Rheap,Rhlimit,Rexnptr],
				     C_caller_saved_regs)

   val C_int_args = map ireg [16, 17, 18, 19, 20, 21]
   val C_fp_args  = map freg [16, 17, 18, 19, 20, 21]
   val C_ra_reg   = Rra
   val C_int_res  = [ireg 0]
   val C_fp_res   = [freg 0]

   local
(*       val num_indirect_int_args = 14 *)
(*       val num_indirect_fp_args = 21 *)
       val num_indirect_int_args = 32
       val num_indirect_fp_args = 32
   in
       val indirect_int_args = 
	   listdiff(Regset.listItems (regsBelow (ireg (num_indirect_int_args - 1))), special_iregs)
       val indirect_fp_args  = 
	   Regset.listItems (regsBelow (freg (num_indirect_fp_args - 1)))
   end

   val Rpv = (case Rpv of
		NONE => error "no Rpv for Alpha"
	      | SOME x => x)
   val indirect_ra_reg   = Rra
   val indirect_int_res  = indirect_int_args
   val indirect_fp_res   = indirect_fp_args
   val indirect_caller_saved_regs = 
     listunion ([indirect_ra_reg,Rpv],
		listunion(indirect_int_args, indirect_fp_args))
   val indirect_callee_saved_regs = 
     listdiff(general_regs, indirect_caller_saved_regs)

  fun makeAsmHeader(PROCSIG{framesize,ra_offset,...}) = 
      let val (realized,framesize, ra_offset) = (case (framesize, ra_offset) of
						     (SOME f, SOME r) => (true,f,r)
						   | _ => (false,0,0))
	  val result =  ("\t.mask (1 << 26), -" ^ 
					       (Int.toString (framesize - ra_offset)) ^
					       "\n\t.frame $sp, " ^ 
					       (Int.toString framesize) ^ "\n" ^
					       "\t.prologue 1\n")
      in  if realized 
	      then result 
	  else ("error: framesize/ra_offset unrealized\n" ^ result)
      end


  fun msRegList l = 
    let fun doer(r,acc) = acc ^ " " ^ (msReg r)
    in  foldr doer "" l 
    end
  fun msRegSet s = msRegList (setToList s)

(*
  fun translateLocalLabel loclabel = loclabel
  fun translateCodeLabel loclabel = loclabel
  fun translateLabel (Rtl.LOCAL_LABEL ll) = I (translateLocalLabel ll)
    | translateLabel (Rtl.ML_EXTERN_LABEL label) = MLE label
    | translateLabel (Rtl.C_EXTERN_LABEL label) = CE (label,NONE)
*)

  val programHeader = ["\t.set noat\n"]
  fun procedureHeader label = [" \t.align 4\n", "\t.ent " ^ (msLabel (label)) ^ "\n"]
  fun procedureTrailer s = ["\t.end " ^ s ^ "\n"]
  val textStart = ["\t.text\n"]
  val dataStart = ["\t.data\n"]
  fun CodeLabelDecl _ = "" (* no need to do this with the OSF as *)

end

