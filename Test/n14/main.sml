(* TestProgram: R-005-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = {1=3};     (* 17, 26, 42, 35, 9, 5, 8, 9, 1, *)
val y = (3);       (* 17, 26, 42, 35, 9, 7, 9, 1, *)
val test = x=y;    (* 17, 26, 42, 35, Error in 10, 9, 2, 5, 8, 9, 2, 9, 2, *)


(******************************************************************************

  Expected:

       val x = { 1 = 3 } : { 1 : int };
       val y = 3 : int;

       Error:
       Can't unify: { 1 : int }
       With:        int
       in expression: =(x,y)

 ******************************************************************************)


