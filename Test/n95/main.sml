(* TestProgram: R-016-B-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = 14;               (* 17, 26, 42, 35, 9, 1, *)
val f = fn x => not x;    (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 10, 9, 2, 2, *)
val g = fn x => x : 'a;   (* 17, 26, 42, 35, 11, 14, 15, 16, 42, 35, 9, 2,
                             47, *)


(******************************************************************************

  Expected:

      val x = 14 : int;
      val f = fn : bool -> bool;
      val g = fn : 'a -> 'a;

 ******************************************************************************)


