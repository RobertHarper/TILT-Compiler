(*$import *)

(* This is the original code that exhibits the bug.  *)

infix 4 =
type t = int * int * int
val test1 = fn (a : t, b : t) => a = b
val test2 = fn () => (1,2,3) = (1,2,3)
val test3 = fn (a : t) => a = (1, 2, 3)
