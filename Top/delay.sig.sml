(*$import TopLevel *)

signature DELAY =
sig
    eqtype 'a value
    val delay : (unit -> 'a) -> 'a value
    val force : 'a value -> 'a
end
