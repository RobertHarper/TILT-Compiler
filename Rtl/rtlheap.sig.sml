(*$RTLHEAP: *)

(* heap will be stored in quadwords which are (highword * lowword) *)
signature RTLHEAP = 
  sig
    type w32 = Word32.word 
    type instr
    datatype iword    = WORD of Word32.word | INSTR of instr 
    datatype quad_val = LONGS of (iword * iword) | QFLOAT of real

    val heapsize    : int;
    val reset_heap  : unit              -> unit
    val uninit_val  : w32 ref;
    val lookupquad  : w32               -> quad_val
    val storequad   : w32 * quad_val    -> unit
    val lookuplong  : w32               -> w32
    val storelong   : w32 * w32         -> unit
    val lookupinstr : w32               -> instr
    val storeinstr  : w32 * instr       -> unit
    val lookupfloat : w32               -> real
    val storefloat  : w32 * real        -> unit
    val storebyte   : w32 * int         -> unit
    val showheap    : w32 * int         -> unit (* int specifies # quads to show *)
  end;






