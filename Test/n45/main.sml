(* TestProgram: R-029-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype tree = Lf;   (* 19, 29, 30, *)
val y = Lf;           (* 17, 26, 42, 35, 9, 3, *)
datatype tree = Lf;   (* 19, 29, 30, *)

val x = Lf;           (* 17, 26, 42, 35, 9, 3, *)
val test = x = y;     (* 17, 26, 42, 35, Error in 10, 9, 2, 5, 8, 9, 2, 9, 2, *)


(******************************************************************************

  Expected:

        datatype tree
        con Lf = Lf : tree
        val y = Lf : tree
        datatype tree
        con Lf = Lf : tree
        val x = Lf : tree

        Error:
        Can't unify: tree
        With:        tree
        in expression: =(x,y)

 ******************************************************************************)


