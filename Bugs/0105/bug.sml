(*$import Word8 Word8Vector *)
val b = Word8Vector.fromList (map Word8.fromInt [44,55,66]);
val test10a:unit = Word8Vector.app (fn x => 0w2*x) b
