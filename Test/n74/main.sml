(* TestProgram: S-004-D-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val rec f = 1 :: f;


(******************************************************************************

  Expected:

        Error: fn binding needed in val rec binding

 ******************************************************************************)


