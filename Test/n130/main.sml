(* TestProgram: R-043-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype Branch = Leaf of int * bool;              (* 19, 29, 30, 48, 52, 49,
                                                      49, *)
Leaf (2,true);                                     (* 17, 26, 42, 35, 10, 9,
                                                      3, 5, 8, 9, 1, 9, 3, *)
val r = { noname = Leaf (2,true) , used = true };  (* 17, 26, 42, 35, 9, 5, 8,
                                                      10, 9, 3, 5, 8, 9, 1, 9,
                                                      1, 9, 3, *)
val { noname = Leaf (x,_) , used = test } = r;     (* 17, 26, 42, 38, 41, 43,
                                                      38, 41, 42, 35, 42, 33,
                                                      42, 35, 9, 2, *)


(******************************************************************************

  Expected:

        datatype Branch
        con Leaf = int * bool -> Branch
        val it = Leaf(2, true) : Branch
        val r = {noname = Leaf(2, true), used = true} :
                {noname: Branch, used: bool}
        val x = 2 : int
        val test = true : bool

 ******************************************************************************)


