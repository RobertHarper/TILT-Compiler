signature REGISTERSET = 
  sig
    type instr
    type regi
    type regf

    datatype iword    = WORD of Word32.word | INSTR of instr 
    datatype quad_val = LONGS of (iword * iword) | QFLOAT of real

    val uninit_val : quad_val ref
    val suppress_warning : bool ref
    val ireg_size : int;
    val freg_size : int;
    val reset_register   : unit -> unit;
    val update_ireg      : regi * quad_val -> unit
    val update_freg      : regf * quad_val -> unit
    val lookup_ireg      : regi -> quad_val
    val lookup_freg      : regf -> quad_val
    val lookupval_ireg   : regi -> Word32.word * Word32.word
    val lookupval_freg   : regf -> real
    val register_save    : 
      regi list * regf list -> quad_val list * quad_val list
    val register_restore : 
      (regi list * regf list) -> (quad_val list * quad_val list) -> unit
    val register_parmove : 
      (regi list * regf list) -> (regi list * regf list) -> unit
    val quad2str : quad_val -> string
    val iword2str : iword -> string
    val showreg : unit -> unit
    val showreg_nonzero : unit -> unit
  end;
