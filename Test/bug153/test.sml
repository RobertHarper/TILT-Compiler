signature S = sig type t eqtype u sharing type t = u val f:int->t end

structure X :> S = struct type t = int type u = int fun f(x) = x end;

val y = X.f(3) = X.f(5)
