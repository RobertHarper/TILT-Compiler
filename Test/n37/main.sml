(* TestProgram: R-021-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


exception exc_test of int;    (* 21, 31, 49, *)

val exc_test = 1;             (* 17, 26, Error in 44, 37, 9, 1, *)


(******************************************************************************

  Expected:

      exception exc_test : int -> exn

      Error:
      Can't unify: int -> exn
      With:        'a
      in pattern: exc_test

 ******************************************************************************)


