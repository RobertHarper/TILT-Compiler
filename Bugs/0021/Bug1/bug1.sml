(*$import setup *)

structure bug1 = F(type a = int
		   val f = Int.compare)
