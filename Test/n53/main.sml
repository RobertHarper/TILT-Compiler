(* TestProgram: R-049-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


type 'a t = {name : string, used : 'a } ;          (* 18, 28, 48, 52, 49,
                                                      47, *)
val r = { name = "Foo" , used = true } : int t;    (* 17, 26, 42, 35, Error in
                                                      11, 9, 5, 8, 9, 1, 9, 3,
                                                      49, 49, *)


(******************************************************************************

  Expected:

       type 'a t = {name : string, used : 'a};
       Error:
       Can't unify: bool
       With:        int

 ******************************************************************************)


