(* TestProgram: R-029-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype 'a testtype = empty | leaf of int * 'a;    (* 19, 29, 30, 48, 52, 49,
                                                       47, *)

type x = bool testtype;                             (* 18, 28, 49, 49, *)
val y : x = leaf (3,true);                          (* 17, 26, 45, 42, 35, 49,
                                                       10, 9, 3, 5, 8, 9, 1, 9,
                                                       3, *)


(******************************************************************************

  Expected:

        datatype 'a  testtype
        con empty : 'a testtype
        con leaf : (int * 'a) -> ('a testtype)
        type x = bool testtype
        val y = (leaf (3,true)) : x

 ******************************************************************************)


