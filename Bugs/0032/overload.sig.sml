(*$import Prelude TopLevel Word8 *)

signature OVERLOAD =
sig

    (* special constant overloading *)
    val int : int
    val word : word
    val word8 : Word8.word
    (* val word31 : Word31.word -- unsupported *)
    val real : real
    val char : char
    val string : string
	
    (* default types at overloaded special constants and operators *)
    structure Defaults :
    sig
	val int : int
	val word : word
	val real : real
	val string : string
	val char : char
	    
	val plus : int * int -> int
	val minus : int * int -> int
	val times : int * int -> int
	val div : int * int -> int
	val mod : int * int -> int
	val / : real * real -> real
	val ~ : int -> int
	val abs : int -> int
	val lt : int * int -> bool
	val gt : int * int -> bool
	val lte : int * int -> bool
	val gte : int * int -> bool
    end
end
