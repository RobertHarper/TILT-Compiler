(* TestProgram: S-001-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val x = { name = "Foo", used = true}
val { name = u, name = n } = x;


(******************************************************************************

  Expected:

        Error: duplicate id in pattern row

 ******************************************************************************)


