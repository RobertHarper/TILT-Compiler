(* TestProgram: R-028-B-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


type ('a,'b) func = 'a -> 'b ;       (* 18, 28, 50, 47, 47, *)

val f : ('a,'a) func = fn x => x;    (* 17, 26, 45, 35, 49, 48, 52, 47, 47, 14,
                                        15, 16, 42, 35, 9, 2, *)

val y = f true;                      (* 17, 26, 42, 35, 10, 9, 2, 3, *)


(******************************************************************************

  Expected:

        type ('a , 'b) func = 'a -> 'b
        val f = fn : ('a , 'a) func
        val y = true : bool

 ******************************************************************************)


