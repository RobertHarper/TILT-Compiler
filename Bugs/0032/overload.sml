(*$import Prelude TopLevel Word8 OVERLOAD OvldDefaults *)

structure Overload :> OVERLOAD =
struct

    (* special constant overloading *)
    val int : int = 0
    val word : word = 0w0
    val word8 : Word8.word = 0w0	(* XXX - was failing *)
	
    (* val word31 : Word31.word = 0w0 -- unsupported *)
    val real : real = 0e0
    val char : char = #"\000"
    val string : string = ""
	
    (* operator default types *)
    structure Defaults = OvldDefaults	(* XXX - was failing *)

end
