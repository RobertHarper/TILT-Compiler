(* TestProgram: R-027-B-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val f = fn x => x;              (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 9, 2, *)

val f = fn 0 => 1               (* 17, 26, 42, 35, 14, 15, 16, 42, 34, 9, 1, *)
        |  x => x * f (x-1);    (* 16, 42, 35, 10, 9, 2, 5, 8, 9, 2, 10, 9, 2,
                                   7, 10, 9, 2, 5, 8, 9, 2, 9, 1, *)

val test = f 6 = 30;            (* 17, 26, 42, 35, 10, 9, 2, 5, 8, 10, 9, 2, 1,
                                   9, 1, *)


(******************************************************************************

  Expected:

      val f = fn: 'a -> 'a;
      val f = fn: int -> int;
      val test = true : bool;

 ******************************************************************************)


