(*$import *)
(* TestProgram: D-006-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val it = 2+3+4+5;
val test1 = it = 2+3+4+5;
val it = true::false::[];
val test2 = it = [true,false];
val it = "Foo";
val test3 = it = "Foo";

val alltrue = test1 andalso test2 andalso test3;


(******************************************************************************

  Expected:

        val it = 14 : int
        val test1 = true : bool
        val it = [true, false] : bool list
        val test2 = true : bool
        val it = "Foo" : string
        val test3 = true : bool
        val alltrue = true : bool

 ******************************************************************************)


