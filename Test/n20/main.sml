(* TestProgram: R-011-D-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: Comments (Rule 11).

*)


val x = 1 : 'a;    (* 17, 26, 42, 35, Error in 11, 9, 1, 47, *)


(******************************************************************************

  Expected:

       Error:
       Can't unify: int
       With:        'a

 ******************************************************************************)


