(* TestProgram: R-039-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x as y = 10;                     (* 17, 26, 46, 42, 35, 9, 1, *)
val (m as n) = 10;                   (* 17, 26, 42, 39, 46, 42, 35, 9, 1, *)

val test = x = y andalso (m = n);    (* 17, 26, 42, 35, 10, 9, 2, 5, 8, 10, 9,
                                        2, 5, 8, 9, 2, 9, 2, 10, 9, 2, 5, 8, 9,
                                        2, 9, 2, *)


(******************************************************************************

  Expected:

       val x = 10 : int;
       val y = 10 : int;
       val m = 10 : int;
       val n = 10 : int;
       val test = true : bool;

 ******************************************************************************)


