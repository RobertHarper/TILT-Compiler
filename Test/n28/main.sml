(* TestProgram: R-017-E-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994


   Note: Imperative type variable free in C isn't quantified
         exp in valbind: non-expansiv

*)


val f = fn x =>                               (* 17, 26, 42, 35, 14, 15, 16,
                                                 42, 35 *)
                let                           (* 9, 6, *)
                   val y = ref x              (* 17, 26, 42, 35, 10, 9, 3, 2, *)
                in
                   not(!y) andalso !y = 5     (* 10, 9, 2, 5, 8, 10, 9, 2, 7,
                                                 10, 9, 2, 2, 10, 9, 2, 5, 8,
                                                 10, 9, 2, 2, 9, 1, *)
                end;


(******************************************************************************

  Expected:

       Error:
       Can't unify: int
       With:        bool
       in expression: =(!y,5)

 ******************************************************************************)


