(*$import INTEGER ARITH ArithHelp *)

(* Check for exceptions. *)
(* Does not check for bogus values. *)

functor IntArith (structure I : INTEGER
		  val name : string) :> ARITH =
struct
    fun test () =
	let
	    val min_int = valOf I.minInt
	    val max_int = valOf I.maxInt
	    val one = I.fromInt 1
	    val zero = I.fromInt 0
	    val neg_one = I.fromInt ~1

	    val big_pair = (min_int, neg_one)
	    val div_zero = (one, zero)

	    fun f s = name ^ "." ^ s

	    open ArithHelp
	in
	    (* no test for toInt *)
	    OVF I.~    min_int        (f "~");
	    OVF I.*    big_pair       (f "*");
	    OVF I.div  big_pair       (f "div");
	    DIV I.div  div_zero       (f "div");
	    DIV I.mod  div_zero       (f "mod");
	    OVF I.quot big_pair       (f "quot");
	    DIV I.quot div_zero       (f "quot");
	    DIV I.rem  div_zero       (f "rem");
	    OVF I.+    big_pair       (f "+");
	    OVF I.-    (min_int, one) (f "-");
	    OVF I.abs  min_int        (f "abs")
	    (* no test for fromString *)
	    (* no test for scan *)
	end	    
end
