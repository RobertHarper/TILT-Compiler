(* TestProgram: R-011-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: Comments (Rule 11).

*)


val x : 'a list : 'b list = [];    (* 17, 26, Error in 45, 45, 42, 35, 49, 49,
                                      9, 3, *)


(******************************************************************************

  Expected:

       Error:
       Can't unify: 'a list
       With:        'b list
       in pattern

 ******************************************************************************)


