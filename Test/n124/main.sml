(* TestProgram: R-034-C-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val f = fn x => x;      (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 9, 2, *)
val x = (f,nil);        (* 17, 26, 42, 35, 9, 5, 8, 9, 2, 9, 3, *)
val (g,[]) = x;         (* 17, 26, 42, 38, 41, 42, 35, 42, 36, 9, 2, *)


(******************************************************************************

  Expected:

        val f = fn : 'a -> 'a
        val x = (fn,nil) : ('a -> 'a) * 'b list
        val g = fn : 'a -> 'a

 ******************************************************************************)


