(*
	Position information to refer to the user's project
	description files.
*)

signature POS =
sig
    type pos

    val pos : string * int -> pos	(* file, lineno *)
    val pos' : string -> pos		(* file *)
    val nopos : pos

    val valid : pos -> bool
    val file : pos -> string
    val file' : pos -> string option
    val lineno : pos -> int
    val lineno' : pos -> int option

    val tostring : pos -> string	(* file:line *)

    val blastOutPos : Blaster.outstream -> pos -> unit
    val blastInPos : Blaster.instream -> pos

    val pp_pos : pos -> Formatter.format
end
