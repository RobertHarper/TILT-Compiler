(* TestProgram: S-003-E-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)

abstype ('a,'c) t = Empty | Leaf of 'a * 'b * 'c
with
end


(******************************************************************************

  Expected:

        Error: unbound type variable

 ******************************************************************************)

