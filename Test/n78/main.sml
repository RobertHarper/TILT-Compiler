(* TestProgram: D-003-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


local
        fun f 1 true "Foo" = 1
          | f 2 true "Foo" = 2
          | f 1 false "Foo" = 3
          | f _ _ _ = 4;

        val g = fn x => fn y => fn z =>
            case (x,y,z) of
              (1,true,"Foo") => 1
            | (2,true,"Foo") => 2
            | (1,false,"Foo") => 3
            | (_,_,_) => 4;
        val test_1 = (f 1 true "Foo") = (g 1 true "Foo");
        val test_2 = (f 2 true "Foo") = (g 2 true "Foo");
        val test_3 = (f 1 false "Foo") = (g 1 false "Foo");
        val test_4 = (f 3 true "Hi") = (g 7 true "Foo");
in
        val alltrue= test_1 andalso test_2 andalso test_2
                     andalso test_4;
end;

local
     fun fact x = x*fact(x-1);
in
end;


(******************************************************************************

  Expected:

        val alltrue = true : bool

 ******************************************************************************)


