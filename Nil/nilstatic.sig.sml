signature NILSTATIC = 
  sig
    structure Nil : NIL

    type context 
    val debug : bool ref
    val show_calls : bool ref

    val exp_valid : context * Nil.exp -> Nil.exp * Nil.con
    (* con_valid (context,exp) => (exp',con)
      * PRE: context is well formed
      * POST: exp is well-typed, with type I(con)
      *  exp' is the normal form of exp
      *  con is in normal form, and is the most precise kind for exp
      *)

    val con_valid : context * Nil.con -> Nil.con * Nil.kind
    (* con_valid (context,con) => (con',kind)
      * PRE: context is well formed
      * POST: con is well-kinded, with kind kind.
      *  con' is the normal form of kind
      *  kind is in normal form, and is the most precise kind for con
      *)

    val kind_valid : context * Nil.kind -> Nil.kind
    (* kind_valid (context,kind) => kind'
      * PRE: context is well formed
      * POST: kind is well formed
      *   kind' is the normal form of kind
      *)

    val module_valid : context * Nil.module -> Nil.module
    (* module_valid (context,module) => module'
      * PRE: context is well formed
      * POST: module is well formed
      *   module' is the normal form of module
      *)

    val con_equiv : context * Nil.con * Nil.con -> bool
    (* con_equiv(context,con1,con2) => v
      * PRE : context is well formed
      * POST: returns true if con1 is equivalent to con2
      * Note that if con1 and con2 are both fully 
      * normalized, then equivalent results can be 
      * obtained more efficiently by calling NilContext.alpha_equiv_con
      *)

    val kind_equiv : context * Nil.kind * Nil.kind -> bool
    (* kind_equiv(context,kind1,kind2) => v
      * PRE : context, kind1, kind2 are well formed
      * POST: returns true if kind1 is equivalent to kind2
      * Note that if kind1 and kind2 are both fully 
      * normalized, then equivalent results can be 
      * obtained by calling NilContext.alpha_equiv_kind
      *)

    val sub_kind : context * Nil.kind * Nil.kind -> bool
    (* sub_kind(context,kind1,kind2) => v
      * PRE : context, kind1, kind2 are well formed
      * POST: returns true if kind1 is equivalent to kind2
      * Note that if kind1 and kind2 are both fully 
      * normalized, then equivalent results can be 
      * obtained more efficiently by calling sub_kind'
      *)

    val sub_kind' : context * Nil.kind * Nil.kind -> bool
    (* sub_kind'(context,kind1,kind2) => v
      * PRE : context, kind1, kind2 are well formed, *and* in normal form
      * POST: returns true if kind1 is equivalent to kind2
      *)

    val con_reduce : context * Nil.con -> Nil.con
    (* con_reduce (context,con) => con'
      * PRE: context is well formed
      * POST: con is fully normalized.  Note that this does 
      * not check for well-kindedness.  This is currently just an
      * alias to Normalize.con_normalize
      *)

    val kind_reduce : context * Nil.kind -> Nil.kind
    (* kind_reduce (context,kind) => kind'
      * PRE: context is well formed
      * POST: kind is fully normalized. Note that this does 
      * not check for well-kindedness.  This is currently just an
      * alias to Normalize.kind_normalize
      *)

  end