structure TalInstructions :> TALINSTRUCTIONS = 
  struct
    fun store_float_and_pop gop = Tal.FPsomeargs(Tal.Fstp,Tal.FPgenop(Tal.B8,gop))

    fun load_float gop = Tal.FPsomeargs (Tal.Fld,Tal.FPgenop(Lil.B8,gop))

    fun fpstack_pop () = 
      [Tal.FPnoargs Tal.Fincstp,
       Tal.FPsomeargs (Tal.Ffree,Tal.FPstack 0)
       ]

    fun fp_mov gop1 gop2 = [store_float_and_pop gop1,load_float gop2]

    (* coerce_slot_to_junk offset size *)
    fun coerce_slot_to_junk offset size = Tal.Coerce (Tal.Reg Tal.Esp,[Tal.Slot (offset,size)])
      
  end