
structure Ppm =
struct

    type w8 = Word8.word

    type rgb = w8 * w8 * w8

    type ppm = rgb Array2.array

    val black = (0w0, 0w0, 0w0) : w8 * w8 * w8

    val teamstring = "Team \\f.(\\x.f(x x))(\\x.f(x x)) - 2000"

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

    (* string to word8 vector *)
    fun byt s = Word8Vector.fromList (map (Word8.fromInt o Char.ord)
				      (explode s))

    fun write (ppm, file) =
	let
	    val (w,h) = Array2.dimensions ppm
	    val out = BinIO.openOut file

	    fun dorow r = Vector.app (fn b => BinIO.output1 (out, b))

	    fun go n = if n = h then {} 
		       else (dorow (Array2.row (ppm, n));
			     go (n + 1))
	in
	    BinIO.output (out, byt ("P6\n# " ^ teamstring));
	    BinIO.output (out, byt ((Int.toString w) ^ " " ^
			       (Int.toString h) ^ "\n"));
	    BinIO.output (out, byt "255\n");
	    go 0;
	    BinIO.closeOut out
	end handle _ => raise Ppm ("IO failed in ppm")
	  

end