(* TestProgram: M-002-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: tests type generalisation

*)


val g = fn x => x;
val test = let
              val f = fn (x,y) => x = y
           in
              f (g,g)
           end;


(******************************************************************************

  Expected:

       val g = fn: 'a -> 'a;

       Error:
       Can't unify: ''b
       with:        'a -> 'a
       in expression: f (g,g)

 ******************************************************************************)


