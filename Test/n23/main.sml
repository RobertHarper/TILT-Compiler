(* TestProgram: R-012-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype test = testcon;        (* 19, 29, 30, *)

val y = 1 handle testcon => 2;  (* 17, 26, 42, 35, Error in 12, 9, 1, 15, 16,
                                   42, 37, 9, 1, *)


(******************************************************************************

  Expected:

      datatype test
      con testcon : test

      Error:
      Can't unify: exn
      With:        test
      in pattern: testcon

 ******************************************************************************)


