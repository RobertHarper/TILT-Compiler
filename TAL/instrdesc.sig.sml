signature INSTRDESC =
  sig
    type instrdesc 


    (* add_blocks after current instruction stream*)
    val add_blocks : instrdesc -> Tal.code_block list -> instrdesc

    (* add blocks before current instruction stream *)
    val add_blocks_before : instrdesc -> Tal.code_block list -> instrdesc

    val emit : instrdesc
               -> Tal.instruction
                  -> instrdesc

    (* Emit current instruction steam with a label *)
    val emit_block : instrdesc
                     -> Tal.label
                        -> Tal.con option
                           -> instrdesc

    val empty_instrdesc : unit -> instrdesc

    val flush_blocks : instrdesc
                        -> instrdesc
    val flush_instrs : instrdesc
                        -> instrdesc
    val get_blocks : instrdesc -> Tal.code_block list

  end