(* TestProgram: R-032-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


exception A of int*bool and B and C;    (* 21, 31, 48, 52, 49, 49, *)
exception B = A;                        (* 21, 32, *)


(******************************************************************************

  Expected:

        exception A : int * bool -> exn
        exception B
        exception C
        exception B : int * bool -> exn

 ******************************************************************************)


