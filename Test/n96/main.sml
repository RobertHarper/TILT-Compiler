(* TestProgram: R-017-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: Tests scoping of explicit type variables, Definition p. 20

*)


val x = (let val Id1 : 'a -> 'a = fn z => z in Id1 Id1 end,
         let val Id2 : 'a -> 'a = fn z => z in Id2 Id2 end);

(* 17, 26, 42, 35, 9, 5, 8, 9, 6, 17, 26, 45, 42, 35, 50, 47, 47, 14, 15, 16,
   42, 35, 9, 2, 10, 9, 2, 2,
   9, 6, 17, 26, 45, 42, 35, 50, 47, 47, 14, 15, 16, 42, 35, 9, 2, 10, 9, 2,
   2, *)


(******************************************************************************

  Expected:

        val x = (fn, fn) : ('a -> 'a) * ('b -> 'b)

 ******************************************************************************)


