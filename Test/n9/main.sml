(* TestProgram: M-003-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: tests equality types

*)


val f = fn x => x;
val x = f;
val y = f;
val test = (x = y);


(******************************************************************************

  Expected:

        val f = fn : 'a -> 'a
        val x = fn : 'a -> 'a
        val y = fn : 'a -> 'a
        Error:
        Can't unify :   'a -> 'a)
        With        :   ''b
        in expression:  (= (x,y))

 ******************************************************************************)


