(* TestProgram: R-017-K-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: Tests scope of explicit type variables

*)


val x = (let val Id : 'a -> 'a = fn z => z in Id Id end,
         fn z => z : 'a);

(* 17, 26, 42, 35, 9, 5, 8, 17, 26, 45, 42, 35, 50, 47, 47, 14, 15, 16,
   42, 35, 9, 2, Error in 10, 9, 2, 2,
   14, 15, 16, 42, 35, 11, 9, 2, 47, *)


(******************************************************************************

  Expected:

      Error:
      Can't unify: 'a -> 'a
      With:        'a
      in expression: Id Id

 ******************************************************************************)


