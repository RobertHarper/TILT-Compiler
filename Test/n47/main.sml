(* TestProgram: R-031-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val _ =
let                                   (* 17, 26, 42, 35, 9, 6, *)
   exception A of 'a                  (* 21, Error in 31, 47, *)
in
   (raise A 5) handle A 5 => true     (* 12, 9, 7, 13, 10, 9, 3, 1, 15, 16,
                                         43, 34, 9, 3, *)
end;


(******************************************************************************

  Expected:

        Error:
        non-imperative exception type

 ******************************************************************************)


