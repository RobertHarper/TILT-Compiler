(*$import Sparc PRINTUTILS MACHINEUTILS ORD_MAP ORD_SET TRACKSTORAGE BinarySetFn Util Stats *)

functor SparcTrackstorage(structure Printutils : PRINTUTILS 
			      where type Machine.specific_instruction = Sparc.specific_instruction
			      where type Machine.instruction = Sparc.Machine.instruction
			  structure Machineutils : MACHINEUTILS)
    :> TRACKSTORAGE =
struct

  open Printutils  
  open Machineutils Sparc
  open Machine
  open Core

  datatype info = INFO of {callee_saves: Regset.set,
			   regs_destroyed : Regset.set ref,
			   stackmap  : stacklocation Regmap.map ref,
			   num_permanent_resident : int,
			   num_ints_spilled : int ref,
			   num_fps_spilled : int ref,
			   num_args  : int ref}

  val debug = Stats.ff("SparcTrackstorage")
  fun inc x = x := (!x + 1)

  val error = fn s => Util.error "Alpha/trackstorage.sml" s

  datatype summary = SUMMARY of 
                    {registers_used    : Machine.register list,
		     stackframe_size   : int,
		     prevframe_maxoffset : int,
		     callee_save_slots : (Machine.register * stacklocation) list,
		     fixStackOffset    : Machine.stacklocation -> 
		                         Machine.stacklocation}

  fun newInfo {callee_saves, max_on_stack, max_C_args, regs_destroyed, 
	       stack_resident} =
    INFO {callee_saves     = listToSet callee_saves,
	  regs_destroyed   = ref (listToSet regs_destroyed),
	  stackmap         = ref stack_resident,
	  (* num_???_spilled's are counted funny; 
	     They're 1 less than the actual # *)
	  num_permanent_resident = Regmap.numItems stack_resident,
	  num_ints_spilled = ref (Regmap.numItems stack_resident-1),
	  num_fps_spilled  = ref ~1,   
	  (* num_args: actual max # of arguments passed on stack *)
	  num_args         = ref max_on_stack }


     
(* Stackframe layout:  (Stack is growing DOWNWARDS)

      (high memory)

    | Args passed to |
    | this procedure |
    | in memory      | 
    | 8 bytes ea.    |
    |================| <--- Previous $sp
    | Spilled FPs    |
    |   (unordered)  |
    | 8 bytes ea.    | 
    |----------------|
    | 0/4 pad bytes  | 
    |----------------|
    | Spilled Ints   | 
    |   (unordered)  |
    | 4 bytes ea.    | 
    |----------------|
    | FP Callee- $f31| } Here showing regs are
    | Save Regs    : | } in numerical order; not
    | 8 bytes ea. $f0| } all 32 are saved!
    |----------------|
    | Int Callee- $31| } Ditto, but does not include
    | Save Regs    : | } return address register!
    | 4 bytes ea. $0 | }
    |----------------|
    | Return Address |
    | stored here    |
    |----------------|
    | Additional or  |
    | non-int args   |
    |----------------|
    | hidden param   | <--- we don't use this
    |----------------|
    | 6 words which  | 
    | callee may     | 
    | store int args |
    |----------------|
    | 16 words to    | 
    | save "in" and  |                 
    | "local" regs   |                 
    | of  window     | 
    |================| <--- Current $sp

      (low memory)
 
*)

    structure Intkey : ORD_KEY = 
      struct
	type ord_key = int
	val compare = Int.compare
      end
    structure IntSet = BinarySetFn(Intkey)


  fun stackOffset (INFO{callee_saves, stackmap, num_fps_spilled,
			num_permanent_resident, num_ints_spilled, ...}) (neighbors,reg) =
      (case (Regmap.find (! stackmap, reg)) of
	 NONE =>
	   (case reg of
	      R _ => 
		let fun folder (n,set) = (case Regmap.find(!stackmap,n) of
					      NONE => set
					    | SOME (SPILLED_INT pos) => IntSet.add(set,pos)
					    | _ => set)
		    val used = foldl folder IntSet.empty neighbors
		    fun loop n = if (IntSet.member(used,n))
				     then loop (n+1)
				 else n
		    val unused = loop num_permanent_resident
		    val newpos = if (!num_ints_spilled < unused)
				     then (inc num_ints_spilled;
					   SPILLED_INT (! num_ints_spilled))
				 else SPILLED_INT unused
		in
		  stackmap := Regmap.insert(! stackmap, reg, newpos);
		  newpos
		end
	    | F _ =>
		let val _ = inc num_fps_spilled
                    val newpos = SPILLED_FP (! num_fps_spilled)
		in
		  stackmap := Regmap.insert(! stackmap, reg, newpos);
		  newpos
		end)
       | SOME spos => spos )
      handle e => (print "exception in StackOffset\n"; raise e)

  fun noteUsed (INFO{regs_destroyed,...}) reg = 
    regs_destroyed := Regset.add(! regs_destroyed, reg)

  fun doubleAlign n = ((n + 7) div 8) * 8     (* round up to multiple of 8 *)
  fun stackAlign n  = ((n + 15) div 16) * 16  (* round up to multiple of 16 *)

  fun summarize (INFO{num_permanent_resident,
		      callee_saves, regs_destroyed, stackmap, 
		      num_ints_spilled, num_fps_spilled,
		      num_args}) = 
    let

      fun stride4 cur [] = []
	| stride4 cur (x::xs) = (x,ACTUAL4 cur) :: (stride4 (cur+4) xs)
      fun stride8 cur [] = []
	| stride8 cur (x::xs) = (x,ACTUAL8 cur) :: (stride8 (cur+8) xs)

      val saved_int_regs = 
	Regset.listItems
	 (Regset.intersection
	  (Regset.intersection(listToSet int_regs, callee_saves), ! regs_destroyed))
      val saved_fp_regs =
	(Regset.listItems o Regset.intersection)
	(Regset.intersection(listToSet fp_regs, callee_saves), ! regs_destroyed)
      val num_ints_saved = length saved_int_regs
      val num_fps_saved = length saved_fp_regs

      val save_offset = 4 * 16
      val hidden_offset = save_offset + 4 * 6
      val extra_args_offset = hidden_offset + 4
      val ra_offset = extra_args_offset + 4 * (!num_args)
      val callee_save_int_offset = ra_offset + 4
      val callee_save_fp_offset  = callee_save_int_offset + 4 * num_ints_saved
      val callee_save_slots = 
	(stride4 callee_save_int_offset saved_int_regs) @
	(stride8 callee_save_fp_offset saved_fp_regs)
 
      val spilled_int_offset = callee_save_fp_offset + 8 * num_fps_saved
      val spilled_fp_offset  = doubleAlign(spilled_int_offset +
					   4 * (! num_ints_spilled + 1))

      val stackframe_size = stackAlign(spilled_fp_offset +
				       8 * (! num_fps_spilled + 1))

      fun fixStackOffset (THIS_FRAME_ARG4 i) = ACTUAL4 (extra_args_offset + 4 * i)
	| fixStackOffset (THIS_FRAME_ARG8 i) = ACTUAL8 (extra_args_offset + 4 * i)
	| fixStackOffset (CALLER_FRAME_ARG4 i) = ACTUAL4(stackframe_size + extra_args_offset + 4 * i)
	| fixStackOffset (CALLER_FRAME_ARG8 i) = ACTUAL8(stackframe_size + extra_args_offset + 4 * i)
	| fixStackOffset FRAME_TEMP = ACTUAL4(hidden_offset)
	| fixStackOffset (SPILLED_INT i) = ACTUAL4(spilled_int_offset + 4*i)
	| fixStackOffset (SPILLED_FP i) = ACTUAL8(spilled_fp_offset + 8*i)
	| fixStackOffset (RETADD_POS) = ACTUAL4 ra_offset
        | fixStackOffset x = x

      val registers_used = Regset.listItems (! regs_destroyed)

      val _ = 
	if (! debug) then
	  (emitString "arg slots = ";
	   print_list print_int [! num_args];
	   emitString "ints saved = ";
	   print_list print_int [num_ints_saved];
	   emitString "fps saved = ";
	   print_list print_int [num_fps_saved];
	   emitString "spilled_ints = ";
	   print_list print_int [! num_ints_spilled + 1];
	   emitString "spilled_fps = ";
	   print_list print_int [! num_fps_spilled + 1];
	   emitString "offsets at ";
	   print_list print_int [ra_offset, callee_save_int_offset,
				 callee_save_fp_offset, spilled_int_offset,
				 spilled_fp_offset];
	   emitString "total size = ";
	   print_list print_int [stackframe_size])
	else
	  ()
    in
      SUMMARY {registers_used = registers_used,
	       stackframe_size = stackframe_size,
	       prevframe_maxoffset = if (!num_args = 0)
					 then 0
				     else ra_offset,
	       callee_save_slots = callee_save_slots,
	       fixStackOffset = fixStackOffset}
    end
       handle e => (print "exception in summarize\n"; 
		    raise e)
end
