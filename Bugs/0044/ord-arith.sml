(*$import Prelude TopLevel CHAR ARITH ArithHelp *)

(* Check for exceptions. *)
(* Does not check for bogus values. *)

functor OrdArith (structure C : CHAR
		  val name : string) :> ARITH =
struct
    fun test () =
	let
	    val minOrd = C.ord C.minChar
	    val smaller = minOrd - 1
	    val larger = C.maxOrd + 1
		
	    fun CHR f arg s = ArithHelp.CHR f arg (name ^ "." ^ s)
	    fun OVF f arg s = ArithHelp.OVF f arg (name ^ "." ^ s)
	    fun DIV f arg s = ArithHelp.DIV f arg (name ^ "." ^ s)
	    fun DOM f arg s = ArithHelp.DIV f arg (name ^ "." ^ s)
	    fun UNO f arg s = ArithHelp.UNO f arg (name ^ "." ^ s)
	in
	    CHR C.chr        smaller   "chr";
	    CHR C.chr        larger    "chr";
	    CHR C.succ       C.maxChar "succ";
	    CHR C.pred       C.minChar "pred"
	end
end
