signature ABBREV = 
  sig
    type identifier
    type index
    type result

    (* Note: abbreviate memoizes by identifier. *)
    val abbreviate : identifier -> result -> unit
    val share : index -> result -> result
    val find : index -> result option
    val abbrevs : unit -> (identifier * result) list
    val reset : unit -> unit
  end
