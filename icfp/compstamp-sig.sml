
signature COMPSTAMP =
sig

    (* abstract type of comparable stamps *)
    type stamp

    val compare : stamp * stamp -> order

    val newstamp : unit -> stamp

    val toString : stamp -> string

end