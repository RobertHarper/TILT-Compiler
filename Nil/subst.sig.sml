(*$import Name Nil *)
signature NILSUBST = 
  sig
    type exp = Nil.exp
    type con = Nil.con
    type kind = Nil.kind
    type bnd = Nil.bnd
    type conbnd = Nil.conbnd
    type var = Nil.var
    type module = Nil.module

    type 'a subst

    val debug : bool ref

    val empty : unit -> 'a subst
	
    val fromList : (var * 'a) list -> 'a subst 
    val toList   : 'a subst -> (var * 'a) list 

    val add : 'a subst -> (var * 'a) -> 'a subst 

    val substitute : 'a subst -> var -> 'a option

    val is_empty : 'a subst -> bool

    (*Create a substitution which when applies behaves as if the two 
     * substitutions were applies sequentially.
     *
     * val compose : ('a subst -> 'a -> 'a) -> ('a subst * 'a subst) -> 'a subst 
     *  subst_fn (compose subst_fn (subst2,subst1))
     *  is equivalent to (subst_fn subst2) o (subst_fn subst1)
     *)
    val compose : ('a subst -> 'a -> 'a) -> ('a subst * 'a subst) -> 'a subst 

    (* Merge two substs, without looking at the ranges.
     * If the domains overlap at v, the image of v in the 
     * new map is that of the second map
     *)
    val merge : ('a subst * 'a subst) -> 'a subst 

    val print : ('a -> unit) -> 'a subst -> unit

    type con_subst = con subst
    type exp_subst = exp subst

    (*substConInCon theta con => con'
      * PRE:  The domain of theta, the free variables of the range of theta,
      * the bound variables of the range of theta, and the bound variables of 
      * con are all disjoint.
      * POST: All free variables v in con' occuring in the domain of 
      * theta have been mapped to rename(theta(v)).  All binding sites in con'
      * are distinct, and any binding sites substituted in by theta are globally new.
      *)
    val substConInExp : con_subst -> exp -> exp
    val substConInCon : con_subst -> con -> con
    val substConInKind : con_subst -> kind -> kind
    val substConInBnd : con_subst -> bnd -> bnd
    val substConInCBnd : con_subst -> conbnd -> conbnd
    val substExpInExp : exp_subst -> exp -> exp
    val substExpInCon : exp_subst -> con -> con
    val substExpInKind : exp_subst -> kind -> kind
    val substExpConInExp : (exp_subst * con_subst) -> exp -> exp
    val substExpConInCon : (exp_subst * con_subst) -> con -> con
    val substExpConInKind : (exp_subst * con_subst) -> kind -> kind

    (*Create a substitution which when applies behaves as if the two 
     * substitutions were applies sequentially.
     *)
    val con_subst_compose : (con_subst * con_subst) -> con_subst
    val varConKindSubst : var -> con -> kind -> kind
    val varConConSubst : var -> con -> con -> con
    val varConExpSubst : var -> con -> exp -> exp

    val varExpExpSubst : var -> exp -> exp -> exp
    val varExpConSubst : var -> exp -> con -> con
    val varExpKindSubst : var -> exp -> kind -> kind

    val printConSubst : con_subst -> unit
  end
