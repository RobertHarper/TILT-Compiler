(* TestProgram: R-025-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = 5;                 (* 17, 26, 42, 35, 9, 1, *)
val y = 6;                 (* same as above *)

val i = 5                  (* same as above *)
val j = 6                  (* same as above *)

val test1 = i = x          (* 17, 26, 42, 35, 10, 9, 2, 5, 8, 9, 2, 9, 2, *)
val test2 = j = y          (* same as above *)

val x = 10                 (* 17, 26, 42, 35, 9, 1, *)

val test3 = not (i = x)    (* 17, 26, 42, 35, 10, 9, 2, 7, 10, 9, 2, 5, 8, 9,
                              2, 9, 2, *)


(******************************************************************************

  Expected:

      val x = 5 : int;
      val y = 6 : int;
      val i = 5 : int;
      val j = 6 : int;
      val test1 = true : bool;
      val test2 = true : bool;
      val x = 10 : int;
      val test3 = true : bool;

 ******************************************************************************)


