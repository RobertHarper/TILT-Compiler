(*$import RTL ORD_MAP Int32 BinaryMapFn MACHINE String *)
(* Mapping from labels to whatever (blocks, procedure signatures, etc.) *)

functor Labelmap () 
    :> ORD_MAP where type Key.ord_key = Rtl.label =
struct
  local 
      open Rtl
      structure Labelkey : ORD_KEY = 
	  struct
	      type ord_key = Rtl.label
	      fun compare (LOCAL_CODE s1, LOCAL_CODE s2) = String.compare(s1,s2)
		| compare (LOCAL_DATA s1, LOCAL_DATA s2) = String.compare(s1,s2)
		| compare (C_EXTERN_LABEL s1, C_EXTERN_LABEL s2) = String.compare(s1,s2)
		| compare (ML_EXTERN_LABEL s1, ML_EXTERN_LABEL s2) = String.compare(s1,s2)
		| compare (LOCAL_CODE _, _) = LESS
		| compare (_, LOCAL_CODE _) = GREATER
		| compare (LOCAL_DATA _, _) = LESS
		| compare (_, LOCAL_DATA _) = GREATER
		| compare (C_EXTERN_LABEL _, _) = LESS
		| compare (_, C_EXTERN_LABEL _) = GREATER
	  end
    structure Labelmap = BinaryMapFn(Labelkey)
  in
    open Labelmap
  end
end
