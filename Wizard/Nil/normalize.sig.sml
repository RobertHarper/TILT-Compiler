(*$import Nil NilContextPre NilSubst *)

signature NORMALIZE = 
  sig

    type kind = Nil.kind
    type con = Nil.con
    type exp = Nil.exp
    type module = Nil.module
    type context = NilContextPre.context
    type con_subst = NilSubst.con_subst

    (*Print debugging messages*)
    val debug : bool ref

    (*Print out all major internal calls as they occur*)
    val show_calls : bool ref
    val show_context : bool ref


    (*Normalizing functions with substitutions.
     * xxx_normalize' (context,subst) xxx is equivalent to
     * xxx_normalize context (Subst.substConInXXX subst xxx)
     *
     * PRE: context is well formed!
     * POST: result is the normal form version of the argumentwith
     * respect to the context
     *)
    datatype progress = PROGRESS | HNF | IRREDUCIBLE

    val is_hnf       : con -> bool
    val reduce_hnf   : context * con -> bool * con   (* bool indicates whether HNF was reached *)
    val reduce_hnf'   : context * con * NilSubst.con_subst -> NilSubst.con_subst * con   
    val reduce_once  : context * con -> con
    val con_reduce   : context * con_subst -> con -> progress * con_subst * con
    datatype 'a ReduceResult = REDUCED of 'a | UNREDUCED of con
    val reduce_until : context * (con -> 'a option) * con -> 'a ReduceResult
    val expandMuType : context * con -> con
    val projectRecordType : context * con * Nil.label -> con
    val projectSumType : context * con * Nil.w32 -> con
    val removeDependence : (Nil.var * con) list -> con -> con

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
    val beta_confun : con -> con


    (*PRE: argument is a well kinded, normalized function, lambda(a1...an).c'
     *POST: If c' is syntactically a function application of the form
     *  c'' (a1,...,an), and a1...1n do not occur free in c'', then
     *  eta_confun lambda(a1...an).c' => c''
     *)
    val eta_confun : con -> con

      

    (* gets the type of a well-formed expression *)
    val reduceToSumtype : context * Nil.con -> TilWord32.word * TilWord32.word option * Nil.con list
    val type_of : context * Nil.exp -> Nil.con

    val allprim_uses_carg : Nil.allprim -> bool

    val context_beta_reduce : (context * Alpha.alpha_context) * con 
      -> (context * Alpha.alpha_context) * con * bool 

    val context_reduce_hnf'' : (context * Alpha.alpha_context) * con 
      -> (context * Alpha.alpha_context) * con * bool 

    val context_reduce_hnf' : (context * Alpha.alpha_context) * con 
      -> (context * Alpha.alpha_context) * con

    val context_reduce_hnf : context * con -> context * con

  end