signature SLAVE =
sig
    val Standalone : bool ref
    val SlaveDiag : bool ref

    val slave : unit -> 'a
end
