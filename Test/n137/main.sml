(* TestProgram: R-052-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994.  Updated 1994-09-14

*)


exception test;                                         (* 21, 31, *)
val f = fn x => x;                                      (* 17, 26, 42, 35, 14,
                                                           15, 16, 42, 35, 9,
                                                           2, *)
type t = {name : int -> int, used : bool, ok : exn};    (* 18, 28, 50,
                                                           49, 49, 49, 49, *)
val x : t = {name = f, used = true, ok = test } ;       (* 17, 26, 45, 42, 35,
                                                           47, 9, 5, 8, 9, 2,
                                                           9, 3, 9, 2, *)


(******************************************************************************

  Expected:

       exception test;
       val f = fn : 'a -> 'a;
       type t = { name : int -> int, used : bool, ok : exn};
       val x = {name = fn, used = true, ok = test } : t;

 ******************************************************************************)


