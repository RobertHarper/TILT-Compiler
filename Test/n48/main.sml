(*$import *)
(* TestProgram: R-031-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


let                                   (* 17, 26, 42, 35, 9, 6, *)
   exception A of '_a                 (* 21, 31, 47, *)
in
   A 5                                (* 10, 9, 3, 1, *)
end;


(******************************************************************************

  Expected:

        Error:
        Can't unify: int
        With:        '_a
        in expression: A 5

 ******************************************************************************)


