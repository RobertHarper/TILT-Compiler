(* TestProgram: M-003-C-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

   Note: tests equality types

*)


type A = int;
type x = A;
type y = A;
val test = x = y;


(******************************************************************************

  Expected:

        type A = int
        type x = A
        type y = A
        Error:
        unbound variable x
        unbound variable y
        in expression: =(x,y)

 ******************************************************************************)


