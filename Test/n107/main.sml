(* TestProgram: R-022-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val z = false              (* 17, 26, 42, 35, 9, 3, *)

local                      (* 22, *)
  val z = 2                (* 17, 26, 42, 35, 9, 1, *)
in
  val g = fn y => y + z    (* 17, 26, 42, 35, 14, 15, 16, 42, 35, 10, 9, 2, 5,
                              8, 9, 2, 9, 2, *)
end

val test = not z;          (* 17, 26, 42, 35, 10, 9, 2, 2, *)
val five = g 3;            (* 17, 26, 42, 35, 10, 9, 2, 1, *)



(******************************************************************************

  Expected:

      val z = false : bool;
      val g = fn: int -> int;
      val test = true : bool
      val five = 5 : int;

 ******************************************************************************)

