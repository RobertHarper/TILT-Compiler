(* TestProgram: R-020-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


abstype stacktype = stack of int list         (* 20, 29, 30, 49, 51, 49, *)
with
    val clear = stack([]);                    (* 17, 26, 42, 35, 10, 9, 3, 7,
                                                 9, 3, *)
    fun insert (x,stack(l)) = stack(x::l);    (* 17, 27, 42, 35, 14, 15, 16,
                                                 42, 38, 41, 42, 35, 43, 39,
                                                 42, 35, 10, 9, 3, 7, 10, 9,
                                                 3, 5, 8, 9, 2, 9, 2, *)
end;

val t = stack([1,2]);                         (* 17, 26, 42, 35, 10, 9, Error
                                                 in 2, 7, 10, 9, 2, 5, 8, 9, 1,
                                                 10, 9, 2, 5, 8, 9, 1, 9, 3, *)


(******************************************************************************

  Expected:

      type stacktype
      val clear = <abstract> : stacktype
      val insert = fn : (int * stacktype) -> stacktype

      Error:
      Unbound variable or constructor stack

 ******************************************************************************)

