structure S =
    struct
	val b : Word8Vector.vector = Word8Vector.fromList []
	val test10a:unit = ignore(Word8Vector.map (fn x => 0w2*x) b)
	val test10b:unit = ignore(Word8Vector.map (fn x => 0w2*x) b)
    end
