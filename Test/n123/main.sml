(* TestProgram: R-034-B-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = (1,17);         (* 17, 26, 42, 35, 9, 5, 8, 9, 1, 9, 1, *)
val (1,w) = x;          (* 17, 26, 42, 38, 41, 42, 34, 42, 35, 9, 2, *)


(******************************************************************************

  Expected:

       val x = (1,17) : int * int;
       val w = 17 : int;

 ******************************************************************************)


