(* TestProgram: R-046-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val r = { name = "Foo", used = true };              (* 17, 26, 42, 35, 9, 5, 8,
                                                       9, 1, 9, 3, *)
val { name = x :int as t :real , used = s } = r;    (* 17, 26, 42, 38, 41,
                                                       Error in 46, 49, 45, 42,
                                                       49, 35, 9, 2, *)


(******************************************************************************

  Expected:

       val r = { name = "Foo", used = true } :
               { name : string, used : bool };
       Error:
       Can't unify: int
       With:        real

 ******************************************************************************)


