(* TestProgram: R-005-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val z = {};                               (* 17, 26, 42, 35, 9, 5, *)
val x = { name = "Foo", used = true};     (* 17, 26, 42, 35, 9, 5, 8, 9, 1, 9,
                                             3, *)
val y = {used = false , name = "Foo"};    (* same as above           *)

val test = not (x = y);                   (* 17, 26, 42, 35, 10, 9, 2, 7, 10,
                                             2, 5, 8, 9, 2, 9, 2,  *)


(******************************************************************************

  Expected:

      val z = () : unit;
      val x = { name = "Foo", used = true} : {name : string, used : bool};
      val y = { name = "Foo", used = false} : {name : string, used : bool};
      val test = true : bool;

 ******************************************************************************)


