(* TestProgram: R-012-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


exception exc_test;                 (* 21, 31, *)

val y = 1 handle exc_test => true;  (* 17, 26, 42, 35, Error in 12, 9, 1, 15,
                                       16, 42, 37, 9, 1, *)


(******************************************************************************

  Expected:

      Error: expression and handler doesn't agree
      body          : int
      handler range : bool

 ******************************************************************************)


