(* TestProgram: M-001-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: tests further restrictions p.30

*)


val f = fn {a=x, ... } => x;


(******************************************************************************

  Expected:

        Error: unresolved pattern

 ******************************************************************************)


