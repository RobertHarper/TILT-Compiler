(*$import *)

signature S =
sig
    type t1    type t2    type t3    type t4    type t5    type t6
    type t7    type t8    type t9    type t10   type t11   type t12
    type t13   type t14   type t15   type t16   type t17   type t18
    type t19   type t20   type t21   type t22   type t23   type t24
    eqtype t25
end
functor Bug (structure Arg : S
	     val x : Arg.t25) =
struct
  fun cmpf2s y = (op=)(x,y)
end


