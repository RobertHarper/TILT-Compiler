(* TestProgram: R-006-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)
(* Modified: David Swasey *)

structure X1 = struct datatype t = A end;
structure X2 = struct datatype t = A end;
val _ = X1.A = X2.A;
