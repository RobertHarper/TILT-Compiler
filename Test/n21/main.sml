(* TestProgram: R-011-E-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: Comments (Rule 11).

*)


val f = (fn x => x) : 'a -> 'b;   (* 17, 26, 42, 35, Error in 11, 9, 7, 14, 15,
                                     16, 42, 35, 9, 2, 50, 47, 47, *)


(******************************************************************************

  Expected:

       Error:
       Can't unify: 'a
       With:        'b
       in expression

 ******************************************************************************)


