(* TestProgram: S-004-E-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


exception test;
val rec f = raise test;


(******************************************************************************

  Expected:

        Error: fn binding needed in val rec binding

 ******************************************************************************)


