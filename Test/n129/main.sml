(* TestProgram: R-041-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype t = A of int * bool;                  (* 19, 29, 30, 48, 52, 49, 49, *)
infix A;
exception E of string;                         (* 21, 31, 49, *)

val z = {name = E "Foo", testexc = 3 A true};  (* 17, 26, 42, 35, 9, 5, 8,
                                                  10, 9, 3, 1, 10, 9, 3, 5,
                                                  8, 9, 1, 9, 3, *)
val { name = E s, testexc = x, ... } = z;      (* 17, 26, 42, 38, 41, 43,
                                                  35, 42, 35, 40, 9, 2, *)


(******************************************************************************

  Expected:

        datatype t
        con A = int * bool -> t
        exception E : string -> exn
        val z = {name = E "Foo", testexc = A(3, true)}
              : {name: exn, testexc: t}
        val s = "Foo" : string;
        val x = A(3, true) : t

 ******************************************************************************)


