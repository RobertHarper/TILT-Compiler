structure A :> sig type t end = struct type t = unit end
signature S = sig type t = A.t end
