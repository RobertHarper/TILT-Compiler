(* Check for exceptions. *)
(* Does not check for bogus values. *)

functor RealArith (structure R : REAL
		   val name : string) :> ARITH =
struct
    fun test () =
	let
	    val zero = R.fromInt 0
	    val one = R.fromInt 1
	    val maxint = R.fromInt (valOf (Int.maxInt))
	    val minint = R.fromInt (valOf (Int.minInt))
	    val inf = R.posInf
	    val nan = R./(zero, zero)
	    val unordered = (zero,nan)
	    val bigger = R.+(one, maxint)
	    val smaller = R.-(minint, one)
		
	    fun OVF f arg s = ArithHelp.OVF f arg (name ^ "." ^ s)
	    fun DIV f arg s = ArithHelp.DIV f arg (name ^ "." ^ s)
	    fun DOM f arg s = ArithHelp.DOM f arg (name ^ "." ^ s)
	    fun UNO f arg s = ArithHelp.UNO f arg (name ^ "." ^ s)
	in
	    DOM R.sign       nan       "sign";
	    UNO R.compare    unordered "compare";
	    (* no test for fmt *)
	    (* no test for toString *)
	    (* no test for scan *)
	    OVF R.checkFloat inf       "checkFloat";
	    DIV R.checkFloat nan       "checkFloat";
	    OVF R.floor      inf       "floor";
	    OVF R.floor      bigger    "floor";
	    OVF R.floor      smaller   "floor";
	    DOM R.floor	     nan       "floor";
	    OVF R.ceil       inf       "ceil";
	    OVF R.ceil       bigger    "ceil";
	    OVF R.ceil       smaller   "ceil";
	    DOM R.ceil	     nan       "ceil";
	    OVF R.trunc      inf       "trunc";
	    OVF R.trunc      bigger    "trunc";
	    OVF R.trunc      smaller   "trunc";
	    DOM R.trunc	     nan       "trunc";
	    OVF R.round      inf       "round";
	    OVF R.round      bigger    "round";
	    OVF R.round      smaller   "round";
	    DOM R.round	     nan       "round"
	end
end
