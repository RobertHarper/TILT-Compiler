(* TestProgram: R-015-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val f = fn true => 1    (* 17, 26, 42, 35, 14, 15, 16, 42, 36, 9, 1, *)
        |  x => 0;      (* 15, 16, 42, 35, 9, 1, *)
val g = fn x => x;      (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 9, 2, *)


(******************************************************************************

  Expected:

      val f = fn : bool -> int;
      val g = fn : 'a -> 'a;

 ******************************************************************************)


