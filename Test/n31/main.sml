(* TestProgram: R-017-I-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994


   Note: Imperative type variable free in C isn't quantified
         exp in valbind: expansive

*)


val f = fn x => let                      (* 17, 26, 42, 35, 14, 15, 16, 42, 35,
                                            9, 6, *)
                   val y = ref x         (* 17, 26, 42, 35, 10, 9, 3, 2, *)
                in
                   y := 1 ; y := true    (* 25, 10, 9, 2, 5, 8, 9, 2, 9, 1,
                                            25, 24, 25, 24, Error in 10, 9, 2,
                                            5, 8, 9, 2, 9, 3, *)
                end;


(******************************************************************************

  Expected:

       Error:
       Can't unify: bool
       With:        int
       in expression: y := true

 ******************************************************************************)


