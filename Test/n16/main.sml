(*$import *)
(* TestProgram: R-006-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


let                     (* 17, 26, 42, 35, 9, 6, *)
   datatype t = A       (* 19, 29, 30, *)
in
                        (* Error in 10, 9, 2, 5, 8, *)
   let                  (* 9, 6, *)
      datatype t = A    (* 19, 29, 30, *)
   in
      A                 (* 9, 3, *)
   end
   = A                  (* 9, 3, *)
end;


(******************************************************************************

  Expected:

       Error:
       Can't unify: t
       With:        t
       in expression: =(let datatype t = A in A end, A)

 ******************************************************************************)


