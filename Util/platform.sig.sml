(*$import Word32 *)

signature PLATFORM = 
sig
    datatype platform = NT | DUNIX | SOLARIS | LINUX
	
    val platform : platform
    val sleep : real -> unit
    val pid : unit -> Word32.word
end