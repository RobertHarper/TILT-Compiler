(* TestProgram: R-028-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


type 'a pair = 'a * 'a ;             (* 18, 28, 48, 52, 47, 47, *)
type ('a , 'b) pair2 = 'a * 'a;      (* 18, 28, 48, 52, 47, 47, *)

val x : int pair = (1,2);            (* 17, 26, 45, 42, 35, 49, 49, 9, 5,
                                        8, 9, 1, 9, 1, *)
val y : (int,real) pair2 = (1,2);    (* 17, 26, 45, 42, 35, 49, 48, 52, 49, 49,
                                        9, 5, 8, 9, 1, 9, 1, *)

val test = x=y;                      (* 17, 26, 42, 35, 10, 9, 2, 5, 8, 9, 2,
                                        9, 2, *)


(******************************************************************************

  Expected:

        type 'a pair = 'a * 'a
        type ('a , 'b) pair2 = 'a * 'a
        val x = (1,2) : int pair
        val y = (1,2) : (int , real) pair2
        val test = true : bool

 ******************************************************************************)


