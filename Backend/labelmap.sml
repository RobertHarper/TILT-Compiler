(* Mapping from labels to whatever (blocks, procedure signatures, etc.) *)

functor Labelmap (structure Machine : MACHINE) : ORD_MAP =
struct
  local 
      open Machine.Rtl
      structure Labelkey : ORD_KEY = 
	  struct
	      type ord_key = Machine.loclabel
	      fun compare (LOCAL_CODE v1, LOCAL_CODE v2) = Int.compare(Name.var2int v1, Name.var2int v2)
		| compare (LOCAL_DATA v1, LOCAL_DATA v2) = Int.compare(Name.var2int v1, Name.var2int v2)
		| compare (LOCAL_CODE _, LOCAL_DATA _) = LESS
		| compare (LOCAL_DATA _, LOCAL_CODE _) = LESS
		  
	  end
    structure Labelmap = BinaryMapFn(Labelkey)
  in
    open Labelmap
  end
end
