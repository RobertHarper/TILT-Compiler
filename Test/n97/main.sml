(* TestProgram: R-017-C-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994


   Note: Applicative type variable not free in C is quantified
         exp in valbind: non-expansive

*)


val test = let
              val f = fn z => z
           in
              (f 3 = 3) andalso f true
           end;

(* 17, 26, 42, 35, 9, 6,
   17, 26, 42, 35, 14, 15, 16, 42, 35, 9, 2,
   10, 9, 2, 5, 8, 9, 7, 10, 9, 2, 5, 8, 10, 9, 2, 1, 9, 1, 10, 9, 2, 3, *)


(******************************************************************************

  Expected:

      val test = true : bool

 ******************************************************************************)


