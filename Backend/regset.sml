(*$import ORD_SET BinarySetFn *)
(* Sets of registers/pseudoregisters *)

functor Regset (structure Machine : 
		    sig datatype register = R of int | F of int end) 
  : ORD_SET =
struct
  local 
    structure Regkey : ORD_KEY = 
      struct
	type ord_key = Machine.register
	fun compare (Machine.R v, Machine.R v') = Int.compare(v,v')
	  | compare (Machine.F v, Machine.F v') = Int.compare(v,v')
          | compare (Machine.R _, Machine.F _) = LESS
          | compare (Machine.F _, Machine.R _) = GREATER
      end
    structure Regset = BinarySetFn(Regkey)
  in
    open Regset
  end
end
