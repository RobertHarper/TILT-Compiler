(* TestProgram: D-006-D-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


abstype Color = Red
with
    "Foo";
end;


(******************************************************************************

  Expected:

        Error: Syntax error.

 ******************************************************************************)


