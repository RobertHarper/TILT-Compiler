(*$import General Crc *)
signature LINKER = 
  sig

    structure Crc : CRC

    val link : {base_args : string list,        (* Current directory, or *)
                base_result : string} -> unit   (* absolute path. Strings *)
                                                (* should not contain extension. *)

    val mk_exe : {base_arg : string,              (* we create an executable by linking units *)
                  exe_result : string} -> unit    (* to the runtime system. Also, we link to code *)
	                                          (* for initializing clients; label client_entry *)

    val mk_uo : {imports : (string * Crc.crc) list,   (* the strings here are *)
                 exports : (string * Crc.crc) list,   (* unit names - no ext. *)
		 base_result : string} -> unit

  end
