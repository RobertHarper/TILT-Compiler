(*$import TopLevel *)

signature MAIN =
sig
    val main : string * string list -> OS.Process.status
    val main' : unit -> OS.Process.status
end
