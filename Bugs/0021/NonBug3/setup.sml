(*$import Prelude Int Bool *)

signature SIG =
sig
    type t
    val a : t
end

functor F (type t'
	   val init : t'
	   val toString : t' -> string)
    :> SIG where type t = t' =
struct
    type t = t'
    val a = init
end
