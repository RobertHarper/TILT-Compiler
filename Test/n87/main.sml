(* TestProgram: R-008-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val z = { 1 = true };                  (* 17, 26, 42, 35, 9, 5, 8, 9, 3, *)

val x = { name = "Foo", used = true};  (* 17, 26, 42, 35, 9, 5, 8, 9, 1, 9,
                                          3, *)
val y = { used = true, name = "Foo"};  (* 17, 26, 42, 35, 9, 5, 8, 9, 3, 9,
                                          1, *)

val test1 = # 1 z;                     (* 17, 26, 42, 35, 10, 14, 15, 16, 42,
                                          38, 41, 42, 35, 40, 9, 2, 2, *)
val test2 = (x = y);                   (* 17, 26, 42, 35, 9, 7, 10, 9, 2, 5,
                                          8, 9, 2, 9, 2, *)


(******************************************************************************

  Expected:

      val z = { 1 = true } : {1 : bool};
      val x = { name = "Foo", used = true} : {name : string, used : bool};
      val y = { name = "Foo", used = true} : {name : string, used : bool};
      val test1 = true : bool;
      val test2 = true : bool;

 ******************************************************************************)


