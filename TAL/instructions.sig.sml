signature TALINSTRUCTIONS = 
  sig
    (* Note: lists are backwards (bottom to top of instruction stream) *)

    val store_float_and_pop  : Tal.genop -> Tal.instruction

    val load_float  : Tal.genop -> Tal.instruction
      
    val fpstack_pop : unit -> Tal.instruction list

    val fp_mov : Tal.genop -> Tal.genop -> Tal.instruction list

    (* coerce_slot_to_junk offset size *)
    val coerce_slot_to_junk : Tal.int32 -> Tal.int32 -> Tal.instruction
  end