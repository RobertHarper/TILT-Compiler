(* TestProgram: R-017-F-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994


   Note: Applicative type variable free in C isn't quantified
         exp in valbind: expansive

*)


fn x => let                         (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 9,
                                       6, *)
           val g = (fn y => x) 5    (* 17, 26, 42, 35, 10, 9, 7, 14, 15, 16,
                                       42, 35, 9, 2, 1, *)
        in
           (g = 5.5) andalso g      (* 10, 9, 2, 5, 8, 9, 7, Error in 10, 9, 2,
                                       5, 8, 9, 2, 9, 1, 9, 2, *)
        end;


(******************************************************************************

  Expected:

      Error:
      Can't unify: real
      With:        bool
      in expression: andalso((=(g,5.5)),g)

 ******************************************************************************)


