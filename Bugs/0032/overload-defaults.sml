(*$import Prelude TopLevel *)
(* default types at overloaded special constants and operators *)
(* compiled separately so signature ascription doesn't provide
   any constraints *)
structure OvldDefaults =
struct
    val int = 0
    val word = 0w0
    val real = 0e0
    val string = ""
    val char = #"\000"
	
    val plus = op +
    val minus = op -
    val times = op *
    val div = op div
    val mod = op mod
    val / = op /
    val ~ = ~
    val abs = abs
    val lt = op <
    val gt = op >
    val lte = op <=
    val gte = op >=
end
