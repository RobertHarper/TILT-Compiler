(* TestProgram: R-031-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


exception Red and White of int * bool;            (* 21, 31, 48, 52, 49, 49, *)

infix White;

val x : exn = Red;                                (* 17, 26, 45, 42, 35, 49, 9,
                                                     3, *)
val y : exn = 13 White true;                      (* 17, 26, 45, 42, 35, 49,
                                                     10, 9, 3, 9, 1, 9, 3, *)
nonfix White;

val t = (raise y) handle White(13,true) => Red;   (* 17, 26, 42, 35, 12, 7,
                                                     13, 9, 2, 15, 16, 43, 38,
                                                     41, 42, 34, 42, 36, 9,
                                                     3, *)


(******************************************************************************

  Expected:

        exception Red
        exception White : int * bool -> exn
        val x = Red : exn
        val y = (White (13,true)) : exn
        val t = Red : exn;

 ******************************************************************************)


