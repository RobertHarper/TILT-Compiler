(*$import ARITH ArithHelp *)

(* Check for exceptions. *)
(* Does not check for bogus values. *)

structure PrimArith :> ARITH =
struct
    fun test () =
	let
	    val _ = print "TiltPrim tests\n"
	    val min_int = ~2147483648
	    val max_int = 2147483647
	    open ArithHelp
	in
	    OVF TiltPrim.ineg   min_int        "ineg";
	    OVF TiltPrim.imult  (min_int, ~1)  "imult";
	    OVF TiltPrim.iquot  (min_int, ~1)  "iquot";
	    DIV TiltPrim.iquot  (1, 0)         "iquot";
	    DIV TiltPrim.irem   (1, 0)         "irem";
	    OVF TiltPrim.iplus  (max_int, 1)   "iplus";
	    OVF TiltPrim.iminus (min_int, 1)   "iminus"
	end

end
