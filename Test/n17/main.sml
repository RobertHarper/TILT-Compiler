(* TestProgram: R-010-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = rev {1 = 7, 2 = 6, 3 = 5};   (* 17, 26, 42, 35, Error in 10, 9, 2, 5,
                                        8, 9, 1, 9, 1, 9, 1, *)


(******************************************************************************

  Expected:

        Error:
        Can't unify: 'a * 'b * 'c
        With:        'd list

 ******************************************************************************)


