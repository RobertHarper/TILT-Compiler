(* TestProgram: R-028-D-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


type x = int and y = x;    (* 18, 28, 42, 35, 49, Error in 49, *)


(******************************************************************************

  Expected:

      Error: unbound type constructor x

 ******************************************************************************)


