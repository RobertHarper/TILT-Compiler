(* TestProgram: R-021-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


exception exc_test of int;    (* 21, 31, 49, *)

val x = exc_test 1;           (* 17, 26, 42, 35, 10, 9, 4, 1, *)


(******************************************************************************

  Expected:

      exception exc_test : int -> exn
      val x = (exc_test 1) : exn

 ******************************************************************************)


