(* TestProgram: D-004-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype 'a tree = Empty | Leaf of 'a
withtype A = int and C = int tree;


(******************************************************************************

  Expected:

        datatype 'a tree
        con Empty : 'a tree
        con Leaf : 'a -> 'a tree
        type A
        type C

 ******************************************************************************)


