(*$import Word8Vector *)
val b : Word8Vector.vector = Word8Vector.fromList [];
val test10a:unit = Word8Vector.app (fn x => 0w2*x) b
