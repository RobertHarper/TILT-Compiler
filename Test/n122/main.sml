(* TestProgram: R-034-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = (false,17);    (* 17, 26, 42, 35, 9, 5, 8, 9, 3, 9, 1, *)
val (false,w) = x;     (* 17, 26, 42, 38, 41, 42, 36, 42, 35, 9, 2, *)


(******************************************************************************

  Expected:

        val x = (false,17) : bool * int;
        val w = 17 : int;

 ******************************************************************************)


