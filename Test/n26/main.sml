(* TestProgram: R-015-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val f = fn true => 1   (* 17, 26, 42, 35, 14, Error in 15, 16, 42, 36, 9, 1, *)
        |  x    => x;  (* 16, 42, 35, 9, 2, *)


(******************************************************************************

  Expected:

        Error:
        Can't unify: bool
        With:        int
        in pattern

 ******************************************************************************)


