(*$import *)
infix 4 =

structure A : sig type t = int * int end = struct type t = int * int end

val test1 = fn (a : A.t, b : A.t) => a = b
val test2 = fn (a : A.t, b : A.t) => a = b
val test3 = fn (a : A.t, b : A.t) => a = b
