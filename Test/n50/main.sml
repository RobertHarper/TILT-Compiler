(* TestProgram: R-044-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


exception A of int;                    (* 21, 31, 49, *)
val r = { name = "Foo", ex = A 5};     (* 17, 26, 42, 35, 9, 5, 8, 9, 1, 10,
                                          9, 3, 1, *)
val { ex = A true, ... } = r;          (* 17, 26, 42, 38, 41, 44, 36, 40, 9,
                                          2, *)


(******************************************************************************

  Expected:

        exception A : int -> exn
        val r = { name = "Foo", ex = A 5 } :
                { name : string, used : exn };

        Error:
        Can't unify: int
        With:        bool
        in pattern:  A true

 ******************************************************************************)


