(*$import Prelude Word32 *)

signature PLATFORM = 
sig
    datatype platform = NT | DUNIX | SOLARIS | LINUX
	
    val platform : unit -> platform
    val hostname : unit -> string
    val pid : unit -> Word32.word
    val sleep : real -> unit
end