signature EXN_HANDLER =
sig
    val Interactive : bool ref
    val errorMsg : exn -> string
    val print : exn -> unit
    val printAndExit : exn -> 'a
end
