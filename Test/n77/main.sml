(* TestProgram: D-002-A-ACCEPT

   TestSuite for Standard ML

   Michael Vium and Sten Schmidt
   Technical University of Denmark

   September 1994

*)


local
  val r = {};
  val () = r;
  val {} = r;

  val t = { 2 = true , 3 = 1, 1 ="Foo"};
  val (n1, u1, no1) = t;
  val { 3 = no2, 1 = n2, 2 = u2 } = t;
in
  val test1 = (n1 = n2) andalso (u1=u2) andalso (no1=no2);
end;

local
  val t = { used = true , number = 1, name ="Foo"};
  val { number : int , used as x, ... } = t;
  val { name , ... } = t
in
  val test2 = (number = 1) andalso used andalso x andalso (name = "Foo");
end;

local
  type x = { 1 : bool , 3 : int, 2 : string };
  type y = bool * string * int;
  val t :x :y = { 1 = true , 3 = 1, 2 ="Foo"} :x :y ;
in
  val test3 = t = (true,"Foo",1);
end;

val alltrue = test1   andalso test2   andalso test3;


(******************************************************************************

  Expected:

        val test1 = true : bool
        val test2 = true : bool
        val test3 = true : bool
        val alltrue = true : bool

 ******************************************************************************)


