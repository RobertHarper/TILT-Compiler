(* TestProgram: R-050-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: This program also tests rule 51.

*)


type t = (int -> ((int) * bool));    (* 18, 51, 50, 49, 51, 48, 51, 49, 49, *)

val f : t = fn x => (x,true);        (* 17, 26, 45, 42, 35, 47, 14, 15, 16,
                                        42, 35, 9, 5, 8, 9, 2, 9, 3, *)


(******************************************************************************

  Expected:

       type t = int -> (int * bool);
       val f = fn : t;

 ******************************************************************************)


