(*$import Nil *)

signature NORMALIZE = 
  sig

    type kind = Nil.kind
    type con = Nil.con
    type exp = Nil.exp
    type module = Nil.module
    type context 
    type 'a subst

    (*Print debugging messages*)
    val debug : bool ref

    (*Print out all major internal calls as they occur*)
    val show_calls : bool ref
    val show_context : bool ref

    (*Given a constructor, returns the most imprecise kind for that
     * con - i.e, merely the shape info
     *)
    val get_shape : context -> con -> kind
    val make_shape : context -> kind -> kind

    (*Normalizing functions:
     * PRE: context is well formed!
     * POST: result is the normal form version of the argument, with
     * respect to the context
     *)
    val kind_normalize : context -> kind -> kind
    val con_normalize : context -> con -> con
    val exp_normalize : context -> exp -> exp
    val module_normalize : context -> module -> module

    (*Normalizing functions with substitutions.
     * xxx_normalize' (context,subst) xxx is equivalent to
     * xxx_normalize context (Subst.substConInXXX subst xxx)
     *
     * PRE: context is well formed!
     * POST: result is the normal form version of the argumentwith
     * respect to the context
     *)
    val kind_normalize' : (context * (con subst)) -> kind -> kind
    val con_normalize' : (context * (con subst)) -> con -> con
    val exp_normalize' : (context * (con subst)) -> exp -> exp
    val con_reduce_once : context * con subst -> con -> bool * con subst * con


    val is_hnf       : con -> bool
    val reduce_hnf   : context * con -> con
    val reduce_once  : context * con -> con
    val reduce       : context * con -> con
    datatype 'a ReduceResult = REDUCED of 'a | UNREDUCED of con
    val reduce_until : context * (con -> 'a option) * con -> 'a ReduceResult

    (*Perform the given reduction, renormalizing the result if necessary*)

    (*PRE: argument is a well kinded normalized record projection, c.l
     *POST: If c is syntactically a record {....,l=c',...}, then
     *   beta_conrecord c.l => c'
     *)
    val beta_conrecord : con -> con

    (*PRE: argument is a well kinded, normalized function application,
     * c (c1...cn) 
     *POST: If c is syntactically a function lambda(a1...an).c', then
     *   beta_confun D (c (c1...cn)) => con_normalize D ({ci/ai} c')
     *)
    val beta_confun : context -> con -> con

    (*PRE: argument is a well kinded normalized record, {l1=c1,...ln=cn}
     *POST: If forall i, ci == c'.li, and c' has the appropriate
     * shape, then
     * eta_conrecord D {l1=c1,...ln=cn} => c'
     *)
    val eta_conrecord : context -> con -> con


    (*PRE: argument is a well kinded, normalized function, lambda(a1...an).c'
     *POST: If c' is syntactically a function application of the form
     *  c'' (a1,...,an), and a1...1n do not occur free in c'', then
     *  eta_confun lambda(a1...an).c' => c''
     *)
    val eta_confun : con -> con

      
    (*PRE: argument is a well kinded, normalized type switch,
     * case c {ci(v1,...vn) => ci'}
     *POST: If c is syntactically a primitive constructor prim(p,c1'...cn'), then
     *   beta_typecase D (case c {cj(v1,...vn) => cj'}) => con_normalize D ({ci'/vi} cj')
     * where p == cj, or the default case if none match.
     *)
    val beta_typecase : context -> con -> con

    (* gets the type of a well-formed expression *)
    val type_of : context * Nil.exp -> Nil.con

  end
