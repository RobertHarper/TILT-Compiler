(* TestProgram: R-046-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val r = { name = "Foo", used = true };    (* 17, 26, 42, 35, 9, 5, 8, 9, 1, 9,
                                             3, *)
val { name = x as t , used = test } = r;  (* 17, 26, 42, 38, 41, 46, 42, 35,
                                             42, 35, 9, 2, *)


(******************************************************************************

  Expected:

       val r = { name = "Foo", used = true } :
               { name : string, used : bool };
       val x = "Foo" : string;
       val t = "Foo" : string;
       val test = true : bool;

 ******************************************************************************)


