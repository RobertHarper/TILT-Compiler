(* TestProgram: R-030-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype color = Red | White of int * bool;  (* 19, 29, 30, 48, 52, 49, 49, *)
infix White;
val x : color = Red;                         (* 17, 26, 45, 42, 35, 49, 9, 3, *)
val y = 13 White true;                       (* 17, 26, 42, 35, 10, 9, 3, 5, 8,
                                                9, 1, 9, 3, *)
nonfix White;
val z = White (1,false);                     (* 17, 26, 42, 35, 10, 9, 3, 5, 8,
                                                9, 1, 9, 3, *)


(******************************************************************************

  Expected:

        datatype color
        con Red : color
        con White : (int * bool) -> color
        val x = Red : color
        val y = (White (13,true)) : color
        val z = (White (1,false)) : color

 ******************************************************************************)


