(* TestProgram: R-019-D-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


let datatype t = A of u and u = B of t | C in C = C end;

(* 17, 26, 42, 35, 9, 6, 19, 29, 30, 49, 49, 10, 9, 2, 5, 8, 9, 3, 9, 3, *)


(******************************************************************************

  Expected:

        val it = true : bool;

 ******************************************************************************)


