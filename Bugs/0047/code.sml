(*$import *)

type 'a myref = 'a ref
local val a : int myref = ref 0 in val (ref works1) = a end
val fails : 'a myref = ref 0
