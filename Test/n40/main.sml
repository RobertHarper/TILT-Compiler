(* TestProgram: R-026-A-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x : real = 12;    (* 17, Error in 26, 45, 49, 9, 1, *)


(******************************************************************************

  Expected:

       Error:
       Can't unify: int
       With:        real
       in expression: 12

 ******************************************************************************)


