
structure Ppm =
struct

    type w8 = Word8.word

    type rgb = w8 * w8 * w8

    type ppm = rgb Array2.array

    val black = (0w0, 0w0, 0w0) : w8 * w8 * w8

    exception Ppm of string
	
    fun colortorgb (r, g, b) =
	(Word8.fromInt (Real.round(r * 255.0)),
	 Word8.fromInt (Real.round(g * 255.0)),
	 Word8.fromInt (Real.round(b * 255.0)))

    fun ppm (height, width) =
	Array2.array (height, width, black)

    fun pxl (x, y, c, p) =
	(* array2 wants row, col: *)
	Array2.update (p, y, x, c)

    fun write (ppm, file) =
	raise Ppm "unimplemented"

end