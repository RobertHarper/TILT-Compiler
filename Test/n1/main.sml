(* TestProgram: D-005-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


abstype  Color = Red | White
withtype A = int
with
    val x = Red;
end;

val y = Red;


(******************************************************************************

  Expected:

        abstype Color
        type A
        val x = <abstract> : Color
        Error: Unknown variable Red

 ******************************************************************************)


