(*$import Nil *)

signature NILSTATIC = 
  sig

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

    val con_equiv : context * Nil.con * Nil.con * Nil.kind -> bool

    val kind_equiv : context * Nil.kind * Nil.kind -> bool

    val sub_kind : context * Nil.kind * Nil.kind -> bool

  end
