(* TestProgram: M-001-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: tests further restrictions p.30

*)


val _ =
let
   val f = fn {a=x, ... } => x
in
   f { a = 2.2, b = "Foo", c = "Bar" }
end;


(******************************************************************************

  Expected:

       val it = 2.2 : real;

 ******************************************************************************)


