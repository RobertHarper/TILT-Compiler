(*$import *)

(* TILT was failing to compile this code at one point.  See bug 159. *)

datatype t = A
    
val () =
    let
	datatype t = datatype t
    in  ()
    end
