signature NILSTATIC = 
  sig
    structure Nil : NIL

    type context 
    val debug : bool ref

    val exp_valid : context * Nil.exp -> Nil.exp * Nil.con * Nil.kind 

    val con_valid : context * Nil.con -> Nil.con * Nil.kind
    (* con_valid (context,con) => (con',kind)
      * PRE: con is in alpha normal form
      * POST: con is fully beta,eta and otherwise reduced.
      *       constructor is valid, with kind kind.
      *   An error is raised if called on an invalid constructor
      *
      * We say that a constructor is fully normalized if
      * it has been through the constructor valid algorithm.
      * Note that I don't have a precise specification of
      * what that means at the moment, so you probably
      * don't want to roll your own
      *)

    val con_equiv : context * Nil.con * Nil.con -> bool
    (* con_equiv(context,con1,con2) => v
      * PRE : con1 and con2 in alpha normal form
      * POST: returns true if con1 is equivalent to con2
      * Note that if con1 and con2 are both fully 
      * normalized, then equivalent results can be 
      * obtained by calling NilContext.alpha_equiv_con
      *)

    val con_reduce : context * Nil.con -> Nil.con
    (* con_reduce (context,con) => con'
      * PRE: con is in alpha normal form
      * POST: con is fully normalized, and is guaranteed 
      * to be valid.
      *)

    val kind_valid : context * Nil.kind -> Nil.kind
    (* kind_valid (context,kind) => kind'
      * PRE: kind is in alpha normal form
      * POST: kind is fully beta,eta and otherwise reduced.
      *       (That is, for S(c) occurring in kind',
      *        c is fully normalized)
      *
      *   An error is raised if called on an invalid kind
      *
      * We say that a kind is fully normalized if
      * it has been through the kind valid algorithm.
      * Note that I don't have a precise specification of
      * what that means at the moment, so you probably
      * don't want to roll your own
      *)

    val kind_equiv : context * Nil.kind * Nil.kind -> bool
    (* kind_equiv(context,kind1,kind2) => v
      * PRE : kind1 and kind2 in alpha normal form
      * POST: returns true if kind1 is equivalent to kind2
      * Note that if kind1 and kind2 are both fully 
      * normalized, then equivalent results can be 
      * obtained by calling NilContext.alpha_equiv_kind
      *)

    val kind_reduce : context * Nil.kind -> Nil.kind
  (* kind_reduce (context,kind) => kind'
    * PRE: kind is in alpha normal form
    * POST: kind is fully normalized, and is guaranteed 
    * to be valid.
    *)

  end