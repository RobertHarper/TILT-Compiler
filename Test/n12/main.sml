(* TestProgram: R-003-A-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: Can't refine imperative type variable with non-imperative

*)


val x : 'a * 'b -> ('a * 'b) ref = ref;    (* 17, 26, 45, 42, 35, 49, 50, 48,
                                              52, 47, 47, 48, 52, 47, 47, 9,
                                              Error in 3, *)


(******************************************************************************

  Expected:

        Error:
        Can't unify: '_a -> '_a ref
        With:        'a * 'b -> ('a * 'b) ref

 ******************************************************************************)


