functor AlphaTrackstorage(structure Regmap : ORD_MAP
			  structure Regset : ORD_SET
			  structure Decalpha : DECALPHA
			  structure Printutils : PRINTUTILS
			  sharing Printutils.Machineutils.Machine = Decalpha
			  sharing type Regmap.Key.ord_key = Decalpha.register
				  sharing type Regset.Key.ord_key = Decalpha.register
				      ) : TRACKSTORAGE =
struct

  open Printutils Printutils.Machineutils Decalpha

  structure Machineutils = Printutils.Machineutils


  datatype info = INFO of {callee_saves: Regset.set,
			   regs_destroyed : Regset.set ref,
			   stackmap  : stacklocation Regmap.map ref,
			   num_ints_spilled : int ref,
			   num_fps_spilled : int ref,
			   num_args  : int ref}

  val debug = ref false
  fun inc x = x := (!x + 1)

  val error = fn s => Util.error "alpha/trackstorage.sml" s

  datatype summary = SUMMARY of 
                    {registers_used    : Machine.register list,
		     stackframe_size   : int,
		     callee_save_slots : (Machine.register * stacklocation) list,
		     tailcallImpossible : unit -> bool,
		     fixStackOffset    : Machine.stacklocation -> 
		                         Machine.stacklocation}

  fun newInfo {callee_saves, max_on_stack, max_C_args, regs_destroyed, 
	       stack_resident} =
    INFO {callee_saves     = listToSet callee_saves,
	  regs_destroyed   = ref (listToSet regs_destroyed),
	  stackmap         = ref stack_resident,
	  (* num_xxx_spilled's are counted funny; 
	     They're 1 less than the actual # *)
	  num_ints_spilled = ref (Regmap.numItems stack_resident-1),
	  num_fps_spilled  = ref ~1,   
	  (* num_args: actual max # of arguments passed on stack *)
	  num_args         = ref max_on_stack }


     
(* Stackframe layout:  (Stack is growing DOWNWARDS)

      (high memory)

    | Args passed to |
    | this procedure |
    | in memory      | 
    | 8 bytes ea.    | <--- Previous $sp
    |================|
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
    | 8 bytes ea. $0 | }
    |----------------|
    | Return         | 
    | Address        | 
    | 8 bytes        | 
    |----------------|
    | Room for args  |                    } Space is allocated for the most
    | passed in mem. |                    } arguments for any callee.  If a
    | from this proc.|                    } particular call needs fewer args,
    | 8 bytes ea.    | <--- Current $sp   } they start at 0($sp) and go UP;
    |================|                      (ints & fps not separated)

      (low memory)
 
*)

  fun noteStackArg (INFO{stackmap,...}) (reg, arg_num) =
        (case (Regmap.find(! stackmap, reg)) of
	   NONE => stackmap := Regmap.insert(! stackmap, reg, 
					     CALLER_FRAME_ARG arg_num)
         | SOME _ => error "noteStackArg:  Redefining register")

  fun stackOffset (INFO{callee_saves, stackmap, num_fps_spilled,
			num_ints_spilled, ...}) reg =
      (case (Regmap.find (! stackmap, reg)) of
	 NONE =>
	   (case reg of
	      R _ => 
		let val _ = inc num_ints_spilled
                    val newpos = SPILLED_INT (! num_ints_spilled)
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

  fun summarize (INFO{callee_saves, regs_destroyed, stackmap, 
		      num_ints_spilled, num_fps_spilled,
		      num_args}) = 
    let
      val ra_offset = 8 * (! num_args)

      val saved_int_regs = 
	Regset.listItems
	 (Regset.intersection
	  (Regset.intersection(listToSet int_regs, callee_saves), ! regs_destroyed))

      val num_ints_saved = length saved_int_regs
     
      val saved_fp_regs =
	(Regset.listItems o Regset.intersection)
	(Regset.intersection(listToSet fp_regs, callee_saves), ! regs_destroyed)
      val num_fps_saved = length saved_fp_regs

      val callee_save_int_offset = ra_offset + 8
      val callee_save_fp_offset  = callee_save_int_offset + 
	                           8 * num_ints_saved

      fun stride4 cur [] = []
	| stride4 cur (x::xs) = (x,ACTUAL4 cur) :: (stride4 (cur+4) xs)
      fun stride8 cur [] = []
	| stride8 cur (x::xs) = (x,ACTUAL8 cur) :: (stride8 (cur+8) xs)

      val callee_save_slots = 
	(stride8 callee_save_int_offset saved_int_regs) @
	(stride8 callee_save_fp_offset saved_fp_regs)


      fun filterIRegs [] = []
        | filterIRegs ((reg as R _, _) :: rest) = reg :: (filterIRegs rest)
	| filterIRegs (_ :: rest) = filterIRegs rest

      fun filterFRegs [] = []
        | filterFRegs ((reg as F _, _) :: rest) = reg :: (filterFRegs rest)
	| filterFRegs (_ :: rest) = filterFRegs rest

      val spilled_int_regs = filterIRegs (Regmap.listItemsi (! stackmap))
      val spilled_fp_regs = filterFRegs (Regmap.listItemsi (! stackmap))
 
      val spilled_int_offset = callee_save_fp_offset + 
	                       8 * num_fps_saved
      val spilled_fp_offset  = doubleAlign(spilled_int_offset +
					   4 * (! num_ints_spilled + 1))

      val stackframe_size = stackAlign(spilled_fp_offset +
				       8 * (! num_fps_spilled + 1))

      fun fixStackOffset (THIS_FRAME_ARG i) = ACTUAL8 (8 * i)
	| fixStackOffset (SPILLED_INT i) = ACTUAL4(spilled_int_offset + 4*i)
	| fixStackOffset (SPILLED_FP i) = ACTUAL8(spilled_fp_offset + 8*i)
	| fixStackOffset (CALLER_FRAME_ARG i) = ACTUAL8(stackframe_size + 
							8 * i)
	| fixStackOffset (RETADD_POS) = ACTUAL8 ra_offset
        | fixStackOffset x = x
      fun tailcallImpossible () = (! num_args) > 0

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
	   print_list print_int [! num_ints_spilled + 1, 
				 length (spilled_int_regs)];
	   emitString "spilled_fps = ";
	   print_list print_int [! num_fps_spilled + 1, 
				 length (spilled_fp_regs)];
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
	       callee_save_slots = callee_save_slots,
	       tailcallImpossible = tailcallImpossible,
	       fixStackOffset = fixStackOffset}
    end
       handle e => (print "exception in summarize\n"; 
		    raise e)
end
