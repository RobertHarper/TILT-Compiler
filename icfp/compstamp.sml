
structure CompStamp :> COMPSTAMP =
struct

    (* abstract type of comparable stamps -- PS they're really ints *)
    type stamp = int

    val compare = Int.compare 


    val counter = ref 0

    fun newstamp () = (counter := !counter + 1;
		       !counter)

    val toString = Int.toString

end