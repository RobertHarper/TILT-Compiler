(* TestProgram: R-027-D-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


fun fll [] = []                          (* 17, 27, 42, 35, 14, 15, 16, 42, 36,
                                            9, 3, *)
|   fll (xs::xsr) = fll xs @ fll xsr;    (* 16, 42, 39, 43, 38, 41, 42, 35, 42,
                                            35, 10, 9, 2, 5, 8, Error in 10, 9,
                                            2, 2, 10, 9, 2, 2, *)


(******************************************************************************

  Expected:

        Error:
        Can't unify: 'a list
        With:        'a
        in expression: fll xs

 ******************************************************************************)


