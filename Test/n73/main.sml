(* TestProgram: S-004-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val rec f = let in fn x => x end;


(******************************************************************************

  Expected:

        Error: fn binding needed in val rec binding

 ******************************************************************************)

