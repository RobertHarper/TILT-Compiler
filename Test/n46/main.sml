(* TestProgram: R-029-D-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype tree1 = Lf;   (* 19, 29, 30, *)
val y = Lf;            (* 17, 26, 42, 35, 9, 3, *)
datatype tree2 = Lf;   (* 19, 29, 30, *)

val x : tree1 = Lf;    (* 17, Error in 26, 45, 42, 35, 49, 9, 3, *)


(******************************************************************************

  Expected:

        datatype tree1
        con Lf = Lf : tree1
        val y = Lf : tree1
        datatype tree2
        con Lf = Lf : tree2

        Error:
        Can't unify: tree2
        With:        tree1
        in expression: val x:tree1 = Lf;

 ******************************************************************************)


