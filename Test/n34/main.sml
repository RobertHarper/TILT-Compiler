(* TestProgram: R-019-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)

val _ =
let datatype t = A of (t -> t) | B in B = B end;

(* 17, 26, 42, 35, 9, 6, 19, 29, 30, 50, 49, 49,
   Error in 10, 9, 2, 5, 8, 9, 3, 9, 3, *)


(******************************************************************************

  Expected:

        Error:
        Can't unify: ''a
        With:        t
        in expression: =(B,B)

 ******************************************************************************)


