(* TestProgram: R-044-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994.  Updated 1994-09-14

*)


exception exc of string;                      (* 21, 31, 49, *)

val r = { name = exc "Foo", used = true };    (* 17, 26, 42, 35, 9, 5, 8, 10,
                                                 9, 4, 1, 9, 3, *)
val { name = exc x , used = test } = r;       (* 17, 26, 42, 38, 41, 44, 35,
                                                 42, 35, 9, 2, *)


(******************************************************************************

  Expected:

       exception exc: string -> exn
       val r = { name = exc "Foo", used = true } :
               { name : exn, used : bool };
       val x = "Foo" : string;
       val test = true : bool;

 ******************************************************************************)


