(* TestProgram: R-017-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994


   Note: Applicative type variable free in C isn't quantified
         exp in valbind: non-expansive

*)


val f = fn x => let
                   val y = x
                in
                   not y andalso y = 5
                end;

(* 17, 26, 42, 35, 14, 15, 16, 42, 35, 9, 6,
   17, 26, 42, 35, 9, 2,
   10, 9, 2, 5, 8, 10, 9, 2, 2, Error in 10, 9, 2, 5, 8, 9, 2, 9, 1, *)


(******************************************************************************

  Expected:

      Error:
      Can't unify: bool
      With:        int
      in expression: y = 5

 ******************************************************************************)


