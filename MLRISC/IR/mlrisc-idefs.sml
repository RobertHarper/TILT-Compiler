functor IDefsFn
   (structure Dom : DOMINATOR_TREE
    structure CFG : CONTROL_FLOW_GRAPH
   ) : MLRISC_IDEFS =
struct

   structure Dom   = Dom
   structure CFG   = CFG
   structure I     = CFG.I
   structure G     = Graph 
   structure IDefs = IDefs

   fun idefs defUse cfg =
   let fun compute_def_use(b,CFG.BLOCK{insns,...}) =
           let fun du([],D,U) = (List.concat D,List.concat U)
                 | du(i::is,D,U) =
                     let val (d,u) = defUse i
                     in  du(is,d::D,u::U) end
           in  du(!insns,[],[])
           end
   in
       IDefs.compute_idefs {cfg=cfg,def_use=compute_def_use}
   end 

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:36  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:04  pscheng
# *** empty log message ***
#
 *)
