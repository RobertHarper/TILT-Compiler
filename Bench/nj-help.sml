(*structure Int = Int32
open Int
*)
fun Ccall (f,arg) = f arg
structure Array2 = struct open Array2 type 'a array2 = 'a array end;
val cos = Math.cos
val sqrt = Math.sqrt
val sin = Math.sin
