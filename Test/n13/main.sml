(* TestProgram: R-005-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = { name = "Foo", used = true};    (* 17, 26, 42, 35, 9, 5, 8, 9, 1, 9,
                                            3, *)
val y = { used = 2, name = "Foo"};       (* 17, 26, 42, 35, 9, 5, 8, 9, 1, 9,
                                            1, *)

val test = not (x = y);                  (* 17, 26, 42, 35, 10, 9, 2, 7, Error
                                            in 10, 2, 5, 8, 9, 2, 9, 2,  *)


(******************************************************************************

  Expected:

      val x = { name = "Foo", used = true} : {name : string, used : bool};
      val y = { name = "Foo", used = 2} : {name : string, used : int};

      Error:
      Can't unify: {name : string, used : bool}
      With:        {name : string, used : int}
      in expression: =(x,y)

 ******************************************************************************)


