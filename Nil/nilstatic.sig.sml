signature NILSTATIC =
  sig

    val NilStaticDiag : bool ref

    type context

    val exp_valid : context * Nil.exp -> Nil.con
    (* exp_valid (Gamma,exp) => con
      * PRE: The context is well formed.
      *      The domain of Gamma and the bound variables of exp are disjoint.
      *      Every binding site in Gamma and exp is unique,
      *  with respect to Gamma, and exp.
      *
      * POST: Gamma |- exp : con, i.e. exp is well-formed with
      *  respect to the given context and has type con.
      *       All binding sites in con are unique with respect to Gamma.
      *
      *)


    val con_valid : context * Nil.con -> Nil.kind
    (* con_valid (Gamma,con) => kind
      * PRE: The context is well formed.
      *      The domain of Gamma and the bound variables of con are disjoint.
      *      Every binding site in Gamma, and con is unique,
      *  with respect to Gamma, and con.
      *
      * POST: Gamma |- con : kind, i.e. con is well-formed with
      *  respect to the given context and has kind kind.
      *       All binding sites in kind are unique with respect to Gamma.
      *
      *)


    val kind_valid : context * Nil.kind -> Nil.kind
    (* con_valid (Gamma,kind) => ()
      * PRE: The context is well formed.
      *      The domain of Gamma and the bound variables of kind are disjoint.
      *      Every binding site in Gamma, and kind is unique,
      *  with respect to Gamma, and kind.
      *
      * POST: Gamma |- kind, i.e. kind is well-formed with
      *  respect to the given context
      *)

    val module_valid : context * Nil.module -> unit

    val con_equiv  : context * Nil.con * Nil.con * Nil.kind -> bool
    val type_equiv : context * Nil.con * Nil.con  -> bool

    val kind_equiv : context * Nil.kind * Nil.kind -> bool

    val sub_kind : context * Nil.kind * Nil.kind -> bool
    (* sub_kind(context,kind1,kind2) => v
      * PRE : context, kind1, kind2 are well formed
      * POST: returns true if kind1 is equivalent to kind2
      *)

    val sub_type : context * Nil.con * Nil.con -> bool

    (* bool indicates progress if true *)
(*
    val con_subst : con_subst * Nil.con -> Nil.con
*)

(*
    val con_reduce : context * Nil.con -> Nil.con
    (* con_reduce (context,con) => con'
      * PRE: context is well formed
      * POST: con is fully normalized.  Note that this does
      * not check for well-kindedness.  This is currently just an
      * alias to Normalize.con_normalize
      *)
*)

(*
    val kind_reduce : context * Nil.kind -> Nil.kind
    (* kind_reduce (context,kind) => kind'
      * PRE: context is well formed
      * POST: kind is fully normalized. Note that this does
      * not check for well-kindedness.  This is currently just an
      * alias to Normalize.kind_normalize
      *)
*)

    val kind_of_cbnd : context * Nil.conbnd -> Nil.var * Nil.kind
  (* Given a context and a constructor binding, returns the variable bound and the kind of the binding *)
  end


