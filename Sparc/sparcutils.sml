(*$import MACHINEUTILS Sparc Int32 Util *)

structure Sparcutils
    :> MACHINEUTILS =
struct

   open Sparc
   open Machine
   open Core

   val error = fn s => Util.error "sparcutils.sml" s

   fun ireg n = R n
   fun freg n = F n
   fun listToSet lst = Regset.addList(Regset.empty, lst)
   fun setToList set = Regset.listItems set
   fun listdiff (a,b) = setToList(Regset.difference(listToSet a,listToSet b))
   fun listintersect (a,b) = setToList(Regset.intersection(listToSet a,listToSet b))
   fun listunion (a,b) = a @ b

   val min_ireg = ireg 0
   val min_freg = freg 0
   val max_ireg = ireg 31
   val max_freg = freg 62
   fun nextReg (reg as (R _)) = 
          if (eqRegs reg max_ireg) then min_ireg else (ireg(regNum reg +1))
     | nextReg (reg as (F _)) =
          if (eqRegs reg max_freg) then min_freg else (freg(regNum reg +2))

   fun prevReg (reg as (R _)) = 
          if (eqRegs reg min_ireg) then max_ireg else (ireg(regNum reg -1))
     | prevReg (reg as (F _)) =
          if (eqRegs reg min_freg) then max_freg else (freg(regNum reg -2))

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


   val unsaved_regs  = [Rat, Rzero, Fat, Rsp, Rat2, Fat2]

   val C_caller_saved_regs = 
     [ireg 8,  ireg 9,  ireg 10, ireg 11, ireg 12, ireg 13,
      ireg 16, ireg 17, ireg 18, ireg 19, ireg 20, ireg 21, ireg 22, ireg 23, 
      ireg 24, ireg 25, ireg 26, ireg 27, ireg 28, ireg 29,

      freg 0,  freg 2,  freg 4,  freg 6, freg 8,
      freg 10, freg 12, freg 14, freg 16, freg 18,
      freg 20, freg 22, freg 24, freg 26, freg 28,
      freg 30, freg 32, freg 34, freg 36, freg 38,
      freg 40, freg 42, freg 44, freg 46, freg 48,
      freg 50, freg 52, freg 54, freg 56, freg 58,
      freg 60, freg 62]

   val save_across_C = listintersect([Rheap,Rhlimit,Rexnptr],
				     C_caller_saved_regs)

   val C_int_args = map ireg [8, 9, 10, 11, 12, 13]
   val C_fp_args  = C_int_args  (* The SPARC passes float args in the int regs *)
   val C_ra_reg   = Rra
   val C_int_res  = [ireg 8]
   val C_fp_res   = [freg 0]

   local
(*       val num_indirect_int_args = 14 *)
(*       val num_indirect_fp_args = 21 *)
       val num_indirect_int_args = 32
       val num_indirect_fp_args = 64
   in
       val indirect_int_args = 
	   listdiff(Regset.listItems (regsBelow (ireg (num_indirect_int_args - 1))), special_iregs)
       val indirect_fp_args  = 
	   Regset.listItems (regsBelow (freg (num_indirect_fp_args - 2)))
   end

   val indirect_ra_reg   = Rra
   val indirect_int_res  = indirect_int_args
   val indirect_fp_res   = indirect_fp_args
   val indirect_caller_saved_regs = 
     listunion ([indirect_ra_reg],
		listunion(indirect_int_args, indirect_fp_args))
   val indirect_callee_saved_regs = 
     listdiff(general_regs, indirect_caller_saved_regs)

  fun makeAsmHeader(PROCSIG{framesize,ra_offset,...}) = 
      let val (realized,framesize, ra_offset) = (case (framesize, ra_offset) of
						     (SOME f, SOME r) => (true,f,r)
						   | _ => (false,0,0))
	  val result =  "\t.proc   07\n"
      in  if realized 
	      then result 
	  else ("error: framesize/ra_offset unrealized\n" ^ result)
      end


  fun msRegList l = 
    let fun doer(r,acc) = acc ^ " " ^ (msReg r)
    in  foldr doer "" l 
    end
  fun msRegSet s = msRegList (setToList s)


  val programHeader = []
  fun procedureHeader label = [" \t.align 4\n", "\t.global " ^ (msLabel (label)) ^ "\n"]
  fun procedureTrailer s = ["\t.size " ^ s ^ ",(.-" ^ s ^ ")\n"]
  val textStart = ["\t.text\n"]
  val dataStart = ["\t.data\n"]
  fun CodeLabelDecl _ = "" 

end

