
signature PPM =
sig

    type w8 = Word8.word

    type rgb = w8 * w8 * w8

    type ppm

    exception Ppm of string

    val colortorgb : Eval.color -> rgb
	
(* height, width *)
    val ppm : int * int -> ppm
	
(* x, y, color, ppm *)
    val pxl : int * int * rgb * ppm -> unit

(* ppm, filename *)
    val write : ppm * string -> unit

end
