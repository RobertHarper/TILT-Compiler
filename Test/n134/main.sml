(* TestProgram: R-048-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


type t = { name : string, used : bool };       (* 18, 28, 48, 52, 49, 49, *)
val r = { name = "Foo" , used = true } : t;    (* 17, 26, 42, 35, 11, 9, 5, 8,
                                                  9, 1, 9, 3, 49, *)


(******************************************************************************

  Expected:

       type t = {name : string, used : bool } ;
       val r = { name = "Foo", used = true } :
               { name : string, used : bool };

 ******************************************************************************)


