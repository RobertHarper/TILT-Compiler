(* TestProgram: S-002-G-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype color1 = Red | Blue
and      color2 = Black | Red;


(******************************************************************************

  Expected:

        Error: duplicate construtor id in datatype declaration

 ******************************************************************************)


