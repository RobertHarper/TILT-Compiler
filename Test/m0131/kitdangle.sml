(*$import *)

(*kitdangle.sml*)

infix - + :: =
(*fun op = (x: ''a, y: ''a): bool =           prim ("=", "=", (x, y)) *)

exception Hd_
fun hd_ (x::l) = x
  | hd_ [] = raise Hd_

fun mklist 0 = []
  | mklist n = n :: mklist(n-1)

fun cycle(p as (m,f)) = 
  if m=0 then p
  else cycle(m-1, let val x = [(m, mklist 2000)]
                  in fn () => #1(hd_ x) + f()
                  end)

val r = cycle(1000, fn() => 0);
