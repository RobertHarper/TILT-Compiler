signature LILTOTAL = 
  sig
    val allocateModule : string -> Lil.module -> unit
    val allocateInterface : string -> Lil.interface -> unit
    (* channel -> filename -> import files -> export files -> unit *)
    val write_imp_headers : TextIO.outstream -> string -> string list -> string list -> unit
  end