(* TestProgram: R-011-B-FAIL

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


val z = (12,(true,3.3)) : int*bool*real;  (* 17, 26, 42, 35, Error in 11, 9,
                                             5, 8, 9, 1, 9, 5, 8, 9, 3, 9,
                                             1, 48, 52, 49, 49, 49, *)


(******************************************************************************

  Expected:

      Error: expression and constraint doesn't agree
      expression : int * (bool * real)
      constraint : int * bool * real

 ******************************************************************************)


