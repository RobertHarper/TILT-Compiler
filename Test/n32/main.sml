(* TestProgram: R-017-J-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: Tests if U ï tyvars VE' = 

*)

fn x => let                  (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 9, 6, *)
           val y : 'a = x    (* Error in 17, 26, 45, 42, 35, 47, 9, 2, *)
        in
           y                 (* 9, 2, *)
        end;


(******************************************************************************

  Expected:

      Error:
      Can't unify: 'b
      With:        'a
      in expression: 'a = x

 ******************************************************************************)


