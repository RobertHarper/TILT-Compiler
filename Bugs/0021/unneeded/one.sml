(*$import *)

functor F (val a : int) = struct val b = a end

structure One = F(val a = 0)
