(* TestProgram: S-003-D-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype ('a,'c) t = Empty | Leaf of 'a * 'b * 'c;


(******************************************************************************

  Expected:

        Error: unbound type variable

 ******************************************************************************)


