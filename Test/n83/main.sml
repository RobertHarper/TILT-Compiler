(* TestProgram: M-002-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: tests type generalisation

*)


val test = let
              val f = fn (x,y) => x = y
           in
              f (2,2)
           end;


(******************************************************************************

  Expected:

       val test = true : bool;

 ******************************************************************************)

