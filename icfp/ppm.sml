
structure Ppm =
struct

    type w8 = Word8.word

    type rgb = w8 * w8 * w8

    type ppm = rgb Array2.array

    val black = (0w0, 0w0, 0w0) : w8 * w8 * w8

    val teamstring = "Team \\f.(\\x.f(x x))(\\x.f(x x)) - 2000"

    exception Ppm of string
	
    fun colortorgb (r, g, b) =
	let fun convert x = 
	    let val x = Real.round(x * 255.0)
	    in  Word8.fromInt(Int.min(255, x))
	    end
	in  (convert r, convert g, convert b)
	end

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
	    val (h,w) = Array2.dimensions ppm
	    val out = BinIO.openOut file

	    fun dopixel (row,col) = 
		let val (r,g,b) = Array2.sub (ppm, row, col)
		in  BinIO.output1 (out, r);
		    BinIO.output1 (out, g);
		    BinIO.output1 (out, b)
		end

	in
	    BinIO.output (out, byt ("P6\n# " ^ teamstring ^ "\n"));
	    BinIO.output (out, byt ((Int.toString w) ^ " " ^
			       (Int.toString h) ^ "\n"));
	    BinIO.output (out, byt "255\n");
	    Base.for (0, h, fn row =>
		      Base.for(0, w, fn col =>
			       dopixel(row,col)));
	    BinIO.closeOut out
	end handle _ => raise Ppm ("IO failed in ppm")
	  

end
