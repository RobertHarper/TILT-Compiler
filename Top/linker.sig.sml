signature LINKER = 
  sig

    structure Crc : CRC

    val link : {uo_args : string list,        (* Current directory, or *)
                uo_result : string} -> unit   (* absolute path. Strings *)
                                              (* should contain extension. *)
    val mk_exe : {uo_arg : string,
                  exe_result : string} -> unit

    val mk_uo : {imports : (string * Crc.crc) list,   (* the strings here are *)
                 exports : (string * Crc.crc) list,   (* unit names - no ext. *)
		 uo_result : string,
                 emitter : BinIO.outstream -> unit} -> unit

  end
