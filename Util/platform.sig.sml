signature PLATFORM =
sig
    datatype platform = DUNIX | SOLARIS | LINUX | GENERIC

    val platform : unit -> platform
    val platformName : platform -> string
    val hostname : unit -> string
    val pid : unit -> Word32.word
    val sleep : real -> unit
end
