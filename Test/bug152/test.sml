signature S = sig type t = int end
signature S' = sig eqtype t end
structure X :> S = struct type t = int end
structure X' :> S' = X
