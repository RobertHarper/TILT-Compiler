(* TestProgram: S-002-F-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype color = Red | Blue
and      color = Black | White;


(******************************************************************************

  Expected:

        Error: duplicate id in datatype declaration

 ******************************************************************************)


