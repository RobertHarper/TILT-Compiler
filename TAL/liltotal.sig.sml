signature LILTOTAL = 
  sig
    (* filename -> _efilname -> unit name -> import files -> export files -> module -> unit *)
    val allocateModule : string -> (string * string) option -> string -> string list -> string list -> Lil.module -> unit
    val allocateInterface : string -> Lil.interface -> unit
  end