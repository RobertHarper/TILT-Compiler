(* Check that rounding operations do the right thing on negative inputs. *)

structure FlooredArith :> ARITH =
struct

    fun expect (eq,toString) (name, result, expected) =
	let val _ = print ("testing " ^ name ^ " rounding ...")
	    val ok = eq (result, expected)
	in  if ok then print "ok\n"
	    else print ("failed [got " ^ toString result ^ ", expected " ^ toString expected ^ "]\n")
	end

    val expectInt = expect (op=, Int.toString)

    val expectReal = expect (Real.==, Real.toString)

    fun testInt () =
	(expectInt ("Int.div", Int.div (7, ~3), ~3);
	 expectInt ("Int.mod", Int.mod (7, ~3), ~2);
	 expectInt ("Int.quot", Int.quot (7, ~3), ~2);
	 expectInt ("Int.rem", Int.rem (7, ~3), 1))

    fun testReal () = (* No test for toInt, toLargeInt, fromLarge, toDecimal, fromDecimal *)
	(expectReal ("Real.realFloor", Real.realFloor ~1.2, ~2.0);
	 expectReal ("Real.realFloor", Real.realFloor 1.2, 1.0);
	 expectReal ("Real.realCeil", Real.realCeil ~1.7, ~1.0);
	 expectReal ("Real.realCeil", Real.realCeil 1.7, 2.0);
	 expectReal ("Real.realTrunc", Real.realTrunc ~1.2, ~1.0);
	 expectReal ("Real.realTrunc", Real.realTrunc 1.2, 1.0);
	 expectReal ("Real.realTrunc", Real.realTrunc ~1.7, ~1.0);
	 expectReal ("Real.realTrunc", Real.realTrunc 1.7, 1.0);
	 expectInt ("Real.floor", Real.floor ~1.2, ~2);
	 expectInt ("Real.floor", Real.floor 1.2, 1);
	 expectInt ("Real.floor", Real.floor ~1.7, ~2);
	 expectInt ("Real.floor", Real.floor 1.7, 1);
	 expectInt ("Real.ceil", Real.ceil ~1.2, ~1);
	 expectInt ("Real.ceil", Real.ceil 1.2, 2);
	 expectInt ("Real.ceil", Real.ceil ~1.7, ~1);
	 expectInt ("Real.ceil", Real.ceil 1.7, 2);
	 expectInt ("Real.trunc", Real.trunc ~1.2, ~1);
	 expectInt ("Real.trunc", Real.trunc 1.2, 1);
	 expectInt ("Real.trunc", Real.trunc ~1.7, ~1);
	 expectInt ("Real.trunc", Real.trunc 1.7, 1))

    fun test () = (testInt(); testReal())
end
