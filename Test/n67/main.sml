(* TestProgram: S-003-A-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


type ('a,'a) t = 'a * 'a;


(******************************************************************************

  Expected:

        Error: duplicate type variable in type binding

 ******************************************************************************)


