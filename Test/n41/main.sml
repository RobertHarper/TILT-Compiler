(* TestProgram: R-026-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = 12 and y = x;    (* 17, 26, 42, 35, 9, 1, 42, 35, 9, Error in 2, *)


(******************************************************************************

  Expected:

      Error: unbound variable x

 ******************************************************************************)


