(*
	Fs writes use a temporary file.  The target should be in a
	directory created by TILT so that the name of the temporary
	does not conflict with user files.  There is only one
	temporary file per process per directory.

	Identity is a machine- and process-specific file name.
*)

signature FS =
sig

	type file = string			(* filename *)
	type pinterface = LinkIl.pinterface
	type units = Name.label list

	val identity : unit -> file

	val mkdirs : file -> unit
	val copy : file * file -> unit		(* old, new *)

	(*
		Uncached file access.
	*)
	val write' : (file -> unit) -> file -> unit
	val write : (Blaster.outstream -> 'a -> unit) -> file -> 'a -> unit
	val read : (Blaster.instream -> 'a) -> file -> 'a

	val flush : unit -> unit
	val exists : file -> bool
	val crc : file -> Crc.crc
	val remove : file -> unit

	val read_pinterface : file -> pinterface
	val read_pinterface_parm : file -> units
	val read_pinterface' : file -> pinterface option
	val write_pinterface : file * pinterface -> unit

end
