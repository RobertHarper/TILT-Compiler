(* TestProgram: R-020-D-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


abstype t = A of t | B     (* 20, 29, 30, 49, *)
with
    val test = B = B;      (* 17, 26, 42, 35, 10, 9, 2, 5, 8, 9, 3, 9, 3, *)
end;


(******************************************************************************

  Expected:

       abstype t
       val test = true : bool;
   
 ******************************************************************************)


