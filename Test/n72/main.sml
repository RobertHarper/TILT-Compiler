(* TestProgram: S-004-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val rec f = (fn y => y)(fn x => x);


(******************************************************************************

  Expected:

        Error: fn binding needed in val rec binding

 ******************************************************************************)


