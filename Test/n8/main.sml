(* TestProgram: M-003-A-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: tests equality types

*)


exception A;
val x = A;
val y = A;
val test = x = y;


(******************************************************************************

  Expected:

       exception A;
       val x = A : exn;
       val y = A : exn;
       Error:
       Can't unify: exn
       With:        ''a
       in expression: =(x,y)

 ******************************************************************************)


