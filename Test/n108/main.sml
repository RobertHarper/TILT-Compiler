(* TestProgram: R-022-B-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val f = fn x => not x;    (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 10, 9, 2, 2, *)

local                     (* 22, *)
  val f = fn x => 3*x     (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 10, 9, 2, 5,
                             8, 9, 1, 9, 2, *)
in
  val g = fn y => f y     (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 10, 9, 2, 2, *)
end

val test = f false;       (* 17, 26, 42, 35, 10, 9, 2, 3, *)
val nine = g 3;           (* 17, 26, 42, 35, 10, 9, 2, 1, *)


(******************************************************************************

  Expected:

      val f = fn: bool -> bool;
      val g = fn: int -> int;
      val test = true : bool;
      val nine = 9 : int;

 ******************************************************************************)

