(* TestProgram: R-022-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


local                        (* 22, *)
  val one = 1                (* 17, 26, 42, 35, 9, 1, *)
in
  val f = fn x => x + one    (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 10, 9, 2,
                                5, 8, 9, 2, 9, 2, *)
end

val three = f 2;             (* 17, 26, 42, 35, 10, 9, 2, 1, *)
val four = 3+one;            (* 17, 26, 42, 35, 10, 9, 2, 5, 8, 9, 1, 9, Error
                                in 2, *)


(******************************************************************************

  Expected:

      val f = fn: int -> int;
      val three = 3 : int;

      Error: unbound variable one

 ******************************************************************************)


