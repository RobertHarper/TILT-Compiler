(*$import *)
(* TestProgram: R-010-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = rev [7,6,5];    (* 17, 26, 42, 35, 10, 9, 2, 10, 9, 3, 5, 8, 9, 1, 10,
                           9, 3, 5, 8, 9, 1, 10, 9, 3, 5, 8, 9, 1, 9, 3, *)


(******************************************************************************

  Expected:

      val x = [5,6,7] : int list;

 ******************************************************************************)


