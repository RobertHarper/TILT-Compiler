(* TestProgram: R-015-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val f = fn x => not x     (* 17, 26, 42, 35, 14, Error in 15, 16, 42, 35, 10,
                             9, 2, 2, *)
        |  y => y + 3;    (* 15, 16, 42, 35, 10, 9, 2, 5, 8, 9, 2, 9, 1, *)


(******************************************************************************

  Expected:

           Error:
           Can't unify: bool
           With:        int
           in expression: y + 3

 ******************************************************************************)


