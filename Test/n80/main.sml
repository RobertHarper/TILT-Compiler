(* TestProgram: D-005-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


abstype  'a tree = Empty | Leaf of 'a tree and Color = Red | White
withtype A = int and B = int and C = int tree
with
    val x : C * A = (Leaf Empty: int tree ,2);
    val y = (Empty, 2:B);
    val test = not (x = y);
end;


(******************************************************************************

  Expected:

        abstype 'a tree
        abstype Color
        type A
        type B
        type C
        val x = (<abstract>, 2) : int tree * int
        val y = (<abstract>, 2) : 'a tree * int
        val test = true : bool

 ******************************************************************************)


