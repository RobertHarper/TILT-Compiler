(* TestProgram: R-019-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


datatype testtype = yes | no | f of testtype * testtype;   (* 19, 29, 30, 48,
                                                              52, 49, 49, *)
val x = f(no,f(no,yes));                                   (* 17, 26, 42, 35,
                                                              10, 9, 3, 5, 8,
                                                              9, 3, 10, 9, 3,
                                                              5, 8, 9, 3, 9,
                                                              3, *)
val y = f(no,f(yes,no));                                   (* same as above *)
val test = not (x = y);                                    (* 17, 26, 42, 35,
                                                              10, 9, 2, 7, 10,
                                                              9, 2, 5, 8, 9, 2,
                                                              9, 2, *)


(******************************************************************************

  Expected:

      datatype testtype
      con yes : testtype
      con no : testtype
      con f : (testtype * testtype) -> testtype
      val x = f(no,f(no,yes)) : testtype
      val y = f(no,f(yes,no)) : testtype
      val test = true : bool

 ******************************************************************************)


