(* TestProgram: R-017-D-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994


   Note: Imperative type variable not free in C is quantified
         exp in valbind: non-expansive

*)


let                                (* 17, 26, 42, 35, 9, 6, *)
   val f = fn x => ref x           (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 10,
                                      9, 3, 2, *)
in
   f 3 ; f true                    (* 25, 24, 25, 10, 9, 2, 2, 10, 9, 2, 3, *)
end;


(******************************************************************************

  Expected:

       val it = ref true : bool ref;

 ******************************************************************************)


