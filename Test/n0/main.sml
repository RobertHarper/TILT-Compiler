(* TestProgram: D-005-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


abstype  Color = Red | White
withtype A = int
with
    val x = Red;
    val y = Red;
end;

val test = not (x = y);


(******************************************************************************

  Expected:

        abstype Color
        type A
        val x = <abstract> : Color
        val y = <abstract> : Color

        Error:
        Can't unify: Color
        With:        ''a
        in expression: =(x,y)

 ******************************************************************************)


