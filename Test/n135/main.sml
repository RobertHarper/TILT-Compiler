(* TestProgram: R-049-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


type 'a t = {name : string, used : 'a};             (* 18, 28, 48, 52, 47,
                                                       47, *)
val r = { name = "Foo" , used = true } : bool t;    (* 17, 26, 42, 35, 11, 9,
                                                       5, 8, 9, 1, 9, 3, 49, *)


(******************************************************************************

  Expected:

       type 'a t = {name : string, used : 'a};
       val r = { name = "Foo", used = true } : bool t;

 ******************************************************************************)


