(* TestProgram: S-003-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype ('a,'a) t = Empty | Leaf of 'a * 'a;


(******************************************************************************

  Expected:

        Error: duplicate type variable in datatype binding

 ******************************************************************************)


