(* TestProgram: R-014-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val f = fn x => x : int;    (* 17, 26, 42, 35, 11, 14, 15, 16, 42, 35, 9, 2,
                               49, *)
val x = f(2) : int;         (* 17, 26, 42, 35, 11, 10, 9, 2, 7, 9, 1, 49, *)


(******************************************************************************

  Expected:

      val f = fn : int -> int
      val x = 2 : int

 ******************************************************************************)


