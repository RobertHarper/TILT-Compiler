(*$import Stats Option Ppnil NilSubst NilUtil NilHNF NILCONTEXT *)

structure NilContext :> NILCONTEXT where type 'a subst = 'a NilSubst.subst 
				     and type context = NilContextPre.context =
  struct
    open NilContextPre
    val find_std_con = find_std_con_pre (fn (D,c) => NilHNF.reduce_hnf (D,c))
  end