(* TestProgram: R-029-B-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


let                            (* 17, 26, 42, 35, 9, 6, *)
   datatype 'a t = A of 'a     (* 19, 29, 30, 49, 47, *)
in
   ( A 3, A true )             (* 9, 5, 8, 10, 9, 3, 1, 10, 9, 3, 3, *)
end;


(******************************************************************************

  Expected:

       val it = (A 3, A true) : int t * bool t;

 ******************************************************************************)


