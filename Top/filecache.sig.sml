(*$import Time Crc *)

signature FILECACHE = 
    sig
	val makeBackups : bool ref	(* Write foo.BACKUP before overwriting file foo. *)
	type internal
	val flushAll : unit -> unit
	val flushSome : string list -> unit
	val exists : string -> bool
	val modTime : string -> Time.time
	val lastModTime : string list -> string option * Time.time
	val size : string -> int
	val read : string -> bool * internal         (* Was it cached? *) 
	val write : string * internal -> bool        (* Did we write?  *)
	val updateCache : string * internal -> bool  (* Update the cache if object cached; succeed? *)
	val crc : string -> Crc.crc
	val tick : unit -> unit
    end 
