(* TestProgram: R-029-E-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype t = A of u          (* 19, 29, 30, 49, *)
     and u = B of t | C;     (* 30, 49, *)


(******************************************************************************

  Expected:

       datatype t
       datatype u
       con A : u -> t;
       con B : t -> u;
       con C : u;

 ******************************************************************************)


