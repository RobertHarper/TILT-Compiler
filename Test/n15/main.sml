(* TestProgram: R-006-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val test1 = let                 (* 17, 26, 42, 35, 9, 6, *)
              val x = true      (* 17, 26, 42, 35, 9, 3, *)
            in
              x                 (* 9, 2, *)
            end;

val test2 = test1 andalso x;    (* 17, 26, 42, 35, 10, 9, 2, 5, 8, 9, 2, 9,
                                   2, *)


(******************************************************************************

  Expected:

      val test1 = true : bool;

      Error: unbound variable x
      in expression: andalso(x,test1)


 ******************************************************************************)


