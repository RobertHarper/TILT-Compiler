(* TestProgram: R-018-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val one = 1;                (* 17, 26, 42, 35, 9, 1, *)

type testtype = int;        (* 18, 28, 49, *)

val one' : testtype = 1;    (* 17, 26, 45, 42, 35, 49, 9, 1, *)

val test = one = one';      (* 17, 26, 42, 35, 10, 9, 2, 5, 8, 9, 2, 9, 2, *)


(******************************************************************************

  Expected:

      val one = 1 : int;
      type testtype
      val one' = 1 : testtype
      test = true : bool

 ******************************************************************************)


