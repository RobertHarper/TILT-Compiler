(*$import Name Lil *)

(*Code that is specific to the Lil substitutions
 *)
signature LILSUBST = 
  sig
    type exp  = Lil.exp
    type con  = Lil.con
    type kind = Lil.kind
    type bnd  = Lil.bnd
    type var  = Lil.var
    type sv32 = Lil.sv32
    type sv64 = Lil.sv64
    type op32 = Lil.op32
    type op64 = Lil.op64

    type sv32_subst
    type sv64_subst
    type con_subst
    type kind_subst

    val install : {pp_con : con -> unit } -> unit

    structure SV32 : SUBST where type item = sv32
			     and type item_subst = sv32_subst

    structure SV64 : SUBST where type item = sv64
			     and type item_subst = sv64_subst

    structure C : SUBST where type item = con
			  and type item_subst = con_subst

    structure K : SUBST where type item = kind
			  and type item_subst = kind_subst
      

    (* Different ways of substituting.  There are a bajillion more
     * possible combinations, and it's fairly trivial to add them.  
     * So I'm using lazy evaluation, and only exposing the ones that 
     * I've encountered a use for.  
     *)
    val substSv32Sv64InExp : sv32_subst * sv64_subst -> exp -> exp
    val substSv32Sv64InBnd : sv32_subst * sv64_subst -> bnd -> bnd

    val substSv32InSv32 : sv32_subst -> sv32 -> sv32
    val substSv64InSv64 : sv64_subst -> sv64 -> sv64

    val substConInSv32 : con_subst -> sv32 -> sv32
    val substConInOp32 : con_subst -> op32 -> op32
    val substConInOp64 : con_subst -> op64 -> op64
    val substConInExp  : con_subst -> exp -> exp
    val substConInCon  : con_subst -> con -> con
    val substConInBnd  : con_subst -> bnd -> bnd

    val substKindInCon     : kind_subst -> con -> con
    val substKindInKind    : kind_subst -> kind -> kind

    val substSv32Sv64ConInExp  : (sv32_subst * sv64_subst * con_subst ) -> exp -> exp
    val substSv32Sv64ConInSv32 : (sv32_subst * sv64_subst * con_subst ) -> sv32 -> sv32
    val substSv32Sv64ConInSv64 : (sv32_subst * sv64_subst * con_subst ) -> sv64 -> sv64

    val substConKindInCon  : (con_subst * kind_subst) -> con -> con

    (* Substitute for a single term variable *)
    val varSv32ExpSubst  : var -> sv32 -> exp -> exp
    val varSv64ExpSubst  : var -> sv64 -> exp -> exp

    (* Substitute for a single constructor variable *)
    val varConConSubst  : var -> con -> con -> con
    val varConExpSubst  : var -> con -> exp -> exp
    val varConSv32Subst : var -> con -> sv32 -> sv32

    (* Substitute for a single kind variable *)
    val varKindConSubst  : var -> kind -> con -> con
    val varKindKindSubst : var -> kind -> kind -> kind

  end
