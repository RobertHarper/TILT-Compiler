(*$import setup *)

structure bug2 = F(type a = string
		   val f = String.compare)
