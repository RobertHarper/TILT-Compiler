(*$import *)
(* TestProgram: R-006-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = 5                    (* 17, 26, 42, 35, 9, 1, *)

val test = let               (* 17, 26, 42, 35, 9, 6, *)
             val x = true    (* 17, 26, 42, 35, 9, 3, *)
           in
             x               (* 9, 2, *)
           end


(******************************************************************************

  Expected:

      val x = 5 : int;
      val test = true : bool;

 ******************************************************************************)


