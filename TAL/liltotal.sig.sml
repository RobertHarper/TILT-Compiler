signature LILTOTAL = 
  sig
    (* filename -> unit name -> import files -> export files -> module -> unit *)
    val allocateModule : string -> string -> string list -> string list -> Lil.module -> unit
    val allocateInterface : string -> Lil.interface -> unit
  end