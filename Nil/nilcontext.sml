(*$import Stats Option Ppnil NilSubst NilUtil Normalize NILCONTEXT *)

structure NilContext :> NILCONTEXT where type context = NilContextPre.context =
  struct
    open NilContextPre
    fun to_hnf (D,c) = #2 (Normalize.reduce_hnf (D,c))
    val find_std_con = find_std_con_pre to_hnf
    val insert_exp = insert_exp_pre Normalize.type_of
  end