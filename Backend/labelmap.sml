(*$import RTL ORD_MAP Int32 BinaryMapFn MACHINE *)
(* Mapping from labels to whatever (blocks, procedure signatures, etc.) *)

functor Labelmap (structure Machine : sig datatype loclabel = LOCAL_CODE of Name.var 
                                                            | LOCAL_DATA of Name.var 
				      end) 
    :> ORD_MAP where type Key.ord_key = Machine.loclabel =
struct
  local 
      open Machine
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
