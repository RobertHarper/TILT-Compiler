(* TestProgram: R-027-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val f = fn 0 => 1               (* 17, 26, 42, 35, 14, 15, 16, 42, 34, 9, 1, *)
        |  x => x * f (x-1);    (* 16, 42, 35, 10, 9, 2, 5, 8, 9, 2, 10, 9,
                                   Error in 2, 7, 10, 9, 2, 5, 8, 9, 2, 9, 1, *)


(******************************************************************************

  Expected:

     Error: unbound variable f

 ******************************************************************************)


