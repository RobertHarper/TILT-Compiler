(* TestProgram: R-011-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = 12 : int;                             (* 17, 26, 42, 35, 11, 9, 1,
                                                 49, *)

val y = (12,(true,3.3)) : int*(bool*real);    (* 17, 26, 42, 35, 11, 9, 5, 8,
                                                 9, 1, 9, 5, 8, 9, 3, 9, 1,
                                                 48, 52, 49, 51, 48, 52, 49,
                                                 49, *)

val z = (rev : int list -> int list) [7,6,5]; (* 17, 26, 42, 35, 10, 9, 7, 11,
                                                 9, 2, 50, 49, 49, 49, 49, 9,
                                                 3, 5, 8, 9, 1, 10, 9, 3, 5,
                                                 8, 9, 1, 10, 9, 3, 5, 8, 9,
                                                 1, 9, 3, *)


(******************************************************************************

  Expected:

      val x = 12 : int
      val y = (12,(true,3.3)) : int * (bool * real)
      val z = [5,6,7] : int list

 ******************************************************************************)


