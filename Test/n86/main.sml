(* TestProgram: R-007-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = 1              (* 17, 26, 42, 35, 9, 1, *)
val y = ((1))          (* 17, 26, 42, 35, 9, 7, 9, 7, 9, 1, *)
val z = (());          (* 17, 26, 42, 35, 9, 7, 9, 5, *)

val test1 = x = y      (* 17, 26, 42, 35, 10, 9, 2, 5, 8, 9, 2, 9, 2, *)
val test2 = (x = y)    (* 17, 26, 42, 35, 9, 7, 10, 9, 2, 5, 8, 9, 2, 9, 2, *)


(******************************************************************************

  Expected:

      val x = 1 : int;
      val y = 1 : int;
      val z = () : unit;
      val test1 = true : bool;
      val test2 = true : bool;

 ******************************************************************************)


