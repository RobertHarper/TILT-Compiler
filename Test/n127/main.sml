(* TestProgram: R-038-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val r = {name = "Foo", used = true };    (* 17, 26, 42, 35, 9, 5, 8, 9, 1, 9,
                                            3, *)
val {name = u, ... } = r;                (* 17, 26, 42, 38, 41, 42, 35, 40, 9,
                                            2, *)
val { ... } = r;                         (* 17, 26, 42, 38, 40, 9, 2, *)


(******************************************************************************

  Expected:

      val r = {name = "Foo", used = true} : {name: string, used: bool}
      val u = "Foo" : string

 ******************************************************************************)


