signature FILECACHE =
    sig
	type name
	type internal
	val flushAll : unit -> unit
	val flushSome : string list -> unit
	val exists : string -> bool
	val modTime : string -> Time.time
	val lastModTime : string list -> string option * Time.time
	val size : string -> int
	val read : name -> internal
	val write : string * internal -> unit
	val crc : string -> Crc.crc
	val remove : string -> unit
	val tick : unit -> unit
    end
