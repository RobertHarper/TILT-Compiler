(* check ref's and polymorphism *)
local

  fun f x = x

  val r = ref f

  val _ = r := (fn y => y)

in

  val _ = !r 13 + 1

end
  

