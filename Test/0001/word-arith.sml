(* Check for exceptions. *)
(* Does not check for bogus values. *)

functor WordArith (structure W : WORD
		   val name : string) :> ARITH =
struct
    val biggerThanInt = W.wordSize >= valOf (Int.precision)
    val biggerThanLargeInt = W.wordSize >= valOf (LargeInt.precision)
	
    fun test () =
	let
	    val zero = W.fromInt 0
	    val max_word = W.notb zero
	    val div_zero = (W.fromInt 1, zero)
	    val toobig = "1" ^ (W.toString max_word)
		
	    infix WHEN
	    fun f WHEN false = ()
	      | f WHEN true = f()

	    fun OVF f arg s = ArithHelp.OVF f arg (name ^ "." ^ s)
	    fun DIV f arg s = ArithHelp.DIV f arg (name ^ "." ^ s)
	in
	    (fn () => OVF W.toLargeInt max_word "toLargeInt") WHEN biggerThanLargeInt;
	    (fn () => OVF W.toInt      max_word "toInt")      WHEN biggerThanInt;
	    DIV W.div        div_zero "div";
	    DIV W.mod        div_zero "mod";
	    OVF W.fromString toobig   "fromString"
	    (* no test for scan *)
	end	    
end
