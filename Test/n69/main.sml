(* TestProgram: S-003-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


abstype ('a,'a) t = Empty | Leaf of 'a * 'a
with
end


(******************************************************************************

  Expected:

        Error: duplicate type variable in datatype binding

 ******************************************************************************)


