(*$import Eqtest1 *)

val test1 = fn (a : t, b : t) => a = b

val test2 = fn () => (1,2,3) = (1,2,3)

val test3 = fn (a : t) => a = (1, 2, 3)
