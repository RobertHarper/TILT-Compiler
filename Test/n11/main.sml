(* TestProgram: R-002-A-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = ref;                             (* 17, 26, 42, 35, 9, 3 *)
val r : 'a * 'b -> ('a * 'b) ref = x;    (* 17, 26, 45, 42, 35, 49, 50, 48, 52,
                                            47, 47, 48, 52, 47, 47, 9, Error
                                            in 2, *)


(******************************************************************************

  Expected:

        Error:
        Can't unify: '_a -> '_a ref
        With:        'a * 'b -> ('a * 'b) ref

 ******************************************************************************)


