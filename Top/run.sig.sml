(*$import TopLevel *)

(* Used in conjunction with Bin/run to pass arguments to a TILT program. *)

signature RUN =
sig
    (* If Bin/run was used to pass us arguments then invoke main.
     * Otherwise invoke main'.
     *)
    val run : {main : string * string list -> OS.Process.status,
	       main' : unit -> OS.Process.status} -> unit
end
