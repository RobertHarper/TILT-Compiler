(* TestProgram: R-025-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val _ =
let datatype t = A    (* 17, 26, 42, 35, 9, 6, 19, 29, 30 *)
    datatype u = A    (* 25, 19, 29, 30 *)
in  A : t : u end;    (* 11, Error in 11, 9, 3, 49, 49, *)


(******************************************************************************

  Expected:

        Error:
        Can't unify: t
        With:        u
        in expression: A : t : u

 ******************************************************************************)


