(* TestProgram: R-036-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val f = fn x :: xr => xr     (* 17, 26, 42, 35, 14, 15, 16, 43, 38, 41, 42, 35,
                                42, 35, 9, 2, *)
        |  true::_ => [];    (* 16, 43, 38, 41, 42, 36, 42, 33, 9, 3, *)


(******************************************************************************

  Expected:

      val f = fn: bool list -> bool list;

 ******************************************************************************)


