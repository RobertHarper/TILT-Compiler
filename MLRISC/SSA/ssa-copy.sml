(*
 * Some utilities for dealing for parallel copies
 *)
signature SSA_COPIES =
sig

   structure SP : SSA_PROPERTIES
   structure C  : CELLS
      sharing SP.C = C

   val simplifyCopy' : C.register list * C.register list -> 
                          C.register list * C.register list
   val simplifyCopy : SP.opn list * SP.opn list -> SP.opn list * SP.opn list

end

functor SSACopiesFn(SP : SSA_PROPERTIES) : SSA_COPIES =
struct

   structure SP = SP
   structure C  = SP.C

   fun error msg = MLRiscErrorMsg.impossible("SSACopies."^msg)

   fun simplifyCopy'(dst,src) =
   let fun f(d::ds,s::ss,dst,src) =
           if d = s then f(ds,ss,dst,src)
           else f(ds,ss,d::dst,s::src)
         | f([],[],dst,src) = (dst,src)
         | f _ = error "simplifyCopy'"
   in  f(dst,src,[],[]) end

   fun simplifyCopy(dst,src) =
   let fun f([],[],dst,src) = (dst,src)
         | f(d::ds,s::ss,dst,src) = 
               if (case (d,s) of
                    (SP.REG(x,_),SP.REG(y,_)) => x=y
                  | (SP.FIX(x,_),SP.FIX(y,_)) => x=y
                  | _ => false)
               then f(ds,ss,dst,src)
               else f(ds,ss,d::dst,s::src)
         | f _ = error "simplifyCopy"
   in  f(dst,src,[],[])
   end

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:47  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:14:52  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:20  pscheng
# *** empty log message ***
#
 *)
