signature FILE =
sig

    val flush_dir_cache : unit -> unit
    val mkdirs : string -> unit

    type file = string		(* filename *)

    val copy : file * file -> unit	(* old, new *)

    val write' : (file -> unit) -> file -> unit
    val write : (Blaster.outstream -> 'a -> unit) -> file -> 'a -> unit
    val read : (Blaster.instream -> 'a) -> file -> 'a

end
