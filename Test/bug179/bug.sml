	fun folder (i:int, x:unit, acc:unit) : unit = print(Int.toString i ^ "\n")

	val xs : unit array = Array.array (10, ())

	val () = Array.foldri folder () (xs,0,NONE)
