(*$import *)
signature S = sig
  type ('a,'b) t
  type ('a,'b) u = ('b,'a) t
  type ('a,'b) v = ('b,'a) u
  sharing type t = v
end
