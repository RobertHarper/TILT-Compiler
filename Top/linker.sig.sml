(*$import General Crc *)
signature LINKER = 
  sig

    type package = {unit : string, base : string, 
		    uiFile : string, uoFile : string, oFile : string}

    val mk_exe : {units : package list,           (* we create an executable by linking units *)
                  exe_result : string} -> unit    (* to the runtime system. Also, we link to code *)
	                                          (* for initializing clients; label client_entry *)

    val mk_uo : {imports : (string * Crc.crc) list,   (* the strings here are *)
                 exports : (string * Crc.crc) list,   (* unit filenames - no ext. *)
		 base_result : string} -> string

  end
