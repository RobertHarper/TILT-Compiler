(* TestProgram: R-050-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: This program also tests rule 51.

*)


type t = (int -> ((int) * bool));    (* 18, 28, 51, 50, 49, 51, 48, 51, 49,
                                        49, *)
val f : t = fn x => (x,x);           (* 17, 26, Error in 45, 42, 35, 47, 14,
                                        15, 16, 42, 35, 9, 5, 8, 9, 2, 9, 2, *)


(******************************************************************************

  Expected:

       type t = int -> (int * bool);

       Error:
       Can't unify: bool
       With:        int

 ******************************************************************************)


