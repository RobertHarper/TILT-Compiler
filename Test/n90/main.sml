(* TestProgram: R-012-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


exception exc_test;                (* 21, 31, *)

val x = 1 handle exc_test => 2;    (* 17, 26, 42, 35, 12, 9, 1, 15, 16, 42, 37,
                                      9, 1, *)


(******************************************************************************

  Expected:

      exception exc_test
      val x = 1 : int

 ******************************************************************************)


