(*$import Name *)
signature NILSUBST = 
  sig
    type exp
    type con
    type kind
    type bnd

    type var = Name.var

    type 'a subst

    val debug : bool ref

    val empty : unit -> 'a subst
	
    val fromList : (var * 'a) list -> 'a subst 
    val toList   : 'a subst -> (var * 'a) list 

    val add : 'a subst -> (var * 'a) -> 'a subst 

    val substitute : 'a subst -> var -> 'a option

    val get_subst_count : unit -> int
    val reset_subst_count : unit -> unit

    val is_empty : 'a subst -> bool

    (* val compose : ('a subst -> 'a -> 'a) -> ('a subst * 'a subst) -> 'a subst 
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

    val substConInCon : con subst -> con -> con
    val substConInKind : con subst -> kind -> kind
    val substConInExp : con subst -> exp -> exp
    val substConInBnd : con subst -> bnd -> bnd * con subst
    val substExpInExp : exp subst -> exp -> exp
    val substExpConInExp : (exp subst * con subst) -> exp -> exp

    val con_subst_compose : (con subst * con subst) -> con subst
    val varConKindSubst : var -> con -> kind -> kind
    val varConConSubst : var -> con -> con -> con

    val printConSubst : con subst -> unit
  end
