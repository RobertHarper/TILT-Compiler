signature PPM = 
    struct

	type bmp : int * int * Eval.color Array2.array

	(* Write bitmap out to file in PPM format.  True if successful *)
	val writePPM : bmp * string -> bool

    end
