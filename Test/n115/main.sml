(* TestProgram: R-028-C-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


type x = int;               (* 18, 28, 49, *)
type x = real and y = x;    (* 18, 28, 49, 49, *)

val t : y = 2;              (* 17, 26, 45, 42, 35, 49, 9, 2, *)


(******************************************************************************

  Expected:

        type x = int
        type x = real
        type y = x
        val t = 2 : y

 ******************************************************************************)


