(*$import ORD_MAP Nil *)

(* Maps from expressions and constructors *)

signature EXPTABLE =
sig
    
    structure ExpKey :
	sig 
	    (* Even though the type this takes is an arbitrary expression, 
	     if it's not constant or a variable we're going to raise an exception *) 
	    type ord_key = Nil.exp
	    val compare : Nil.exp * Nil.exp -> order
	end 

    structure Expmap : ORD_MAP where type Key.ord_key = Nil.exp
    structure Conmap : ORD_MAP where type Key.ord_key = Nil.con

end

