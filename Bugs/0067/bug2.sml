(*$import *)

signature RTL =
sig
    type T0
    structure S1 :
    sig
	type T1
	structure S2 :
	sig
	    type T
	    val t_in : unit -> T
	    val t_eq : T * T -> int
	end
	type T1'
    end
    type T0'
end

functor bug (structure Rtl : RTL) =
struct
    fun cmpf2s c = Rtl.S1.S2.t_eq (c, Rtl.S1.S2.t_in ())
end
