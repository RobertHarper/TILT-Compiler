(* TestProgram: R-013-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype test = testcon;    (* 19, 29, 30, *)

val x = raise testcon;      (* 17, 26, 42, 35, Error in 13, 9, 4, *)


(******************************************************************************

  Expected:

       datatype test;
       con testcon : test;
       Error:
       Can't unify: exn
       With:        test
       in expression: raise testcon

 ******************************************************************************)


