(* TestProgram: R-013-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


exception exc_test;                                               (* 21, 31, *)

val x = (( raise exc_test ) + 43) handle exc_test => 1;
(* 17, 26, 42, 35, 12, 9, 7, 10, 9, 2, 5, 8, 9, 7, 13, 9, 4, 9, 1, 15, 16,
   42, 37, 9, 1, *)

val y = (not ( raise exc_test )) handle exc_test => true;
(* 17, 26, 42, 35, 12, 9, 7, 10, 9, 2, 7, 13, 9, 4, 15, 16, 42, 37, 9, 3, *)

val z = (( raise exc_test ) 43) handle exc_test => 1;
(* 17, 26, 42, 35, 12, 9, 7, 10, 9, 7, 13, 9, 4, 9, 1, 15, 16, 42, 37, 9, 1, *)

val s = (( raise exc_test ) @ [43,1]) handle exc_test => [];
(* 17, 26, 42, 35, 12, 9, 7, 10, 9, 2, 5, 8, 9, 7, 13, 9, 4, 10, 9, 2, 5, 8,
   9, 1, 10, 9, 2, 5, 8, 9, 1, 9, 3, 15, 16, 42, 37, 9, 3, *)

val t = (raise ( raise exc_test )) handle exc_test => exc_test;
(* 17, 26, 42, 35, 12, 9, 7, 13, 9, 7, 13, 9, 4, 15, 16, 42, 37, 9, 4, *)


(******************************************************************************

  Expected:

        exception exc_test
        val x = 1 : int
        val y = true : bool
        val z = 1 : int
        val s = nil : int list
        val t = exc_test : exn

 ******************************************************************************)


