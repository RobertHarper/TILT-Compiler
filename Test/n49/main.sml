(* TestProgram: R-043-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype Branch = Leaf of int * bool;                 (* 19, 29, 30, 48, 52,
                                                         49, 49, *)
val r = { noname = Leaf (2,true) , used = true };     (* 17, 26, 42, 35, 9, 5,
                                                         8, 10, 9, 3, 5, 8, 9,
                                                         1, 9, 3, 9, 3, *)
val { noname = Leaf (x:real,_) , used = s } = r;      (* 17, 26, 42, 38, 41,
                                                         Error in 43, 38, 41,
                                                         45, 42, 35, 49, 42,
                                                         33, 42, 35, 9, 2, *)


(******************************************************************************

  Expected:

       datatype Branch;
       con Leaf : int * bool -> Branch;
       val r = { name = Leaf (2,true) , used = true } :
               { name : Branch, used : bool };
       Error:
       Can't unify: int * bool
       With         real * 'a

 ******************************************************************************)


