
signature EXPTABLE =
sig
    structure Nil : NIL
    
    structure ExpKey :
	sig 
	    (* Even though the type this takes is an arbitrary expression, 
	     if it's not constant or a variable we're going to raise an exception *) 
	    type ord_key = Nil.exp
	    val compare : Nil.exp * Nil.exp -> order
	end 

    (* structure Valmap : ORD_MAP
    sharing type Valmap.Key.ord_key = (Nil.con, Nil.exp) value *)

    structure Expmap : ORD_MAP
    sharing type Expmap.Key.ord_key = Nil.exp
end

