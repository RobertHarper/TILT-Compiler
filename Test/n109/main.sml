(* TestProgram: R-024-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = 2;;;       (* 17, 26, 42, 35, 9, 1, 24, 24, 24, *)
let ; in x end;    (* 17, 26, 42, 35, 9, 6, 25, 24, 24, 9, 2, *)
local ;; in ; end; (* 22, 24, 24, 24, 24, 24, *)

(******************************************************************************

  Expected:

       val x = 2 : int;
       val it = 2 : int;

 ******************************************************************************)


