(*$import Nil *)

(*
 * The NilDefs structure is a central repository for NIL syntactic
 * definitions, and functions for constructing NIL constructs.
 * 
 *)

signature NILDEFS = 
  sig

    (* Is the constructor "small", according to the definition
     * of A-normal form
     *)
    val small_con : Nil.con -> bool

    (* Is the expression "small", according to the definition
     * of A-normal form
     *)
    val small_exp : Nil.exp -> bool

    (* Is the expression syntactically a closed value
     *)
    val is_closed_value : Nil.exp -> bool

    (* The following three functions are predicates on expressions that
     * categorize them according the the kinds of effects they may have.
     * For our purposes, we care about two kinds of effects: control flow
     * effects (non-termination and exceptions) and store effects (reads,
     * writes, and allocates of mutable memory.  See Tarditi's thesis
     * section 5.3.1 for additional discussion.
     *
     * These are only correct if the expression is in a-normal form, since 
     * they do not check the arguments to term-constructors.
     *
     * These are a conservative approximation only, since we do not recurse inside
     * of switches, etc.  The intention is that these should only be used on 
     * small things, to keep asymptotic completexity down (c.f. Tarditi)
     *)

    (* storeEffect e  
     * This function returns true if the expression e may potentially have a 
     * store effect.  In particular, if this function returns false, then the 
     * effect of e is a subset of {E,N}.  If this function returns true, 
     * then the effect of e is a subset of {E,N,A,R,W} (that is, any effect).
     * Note that code that does *not* satisfy this predicate may still raise 
     * exceptions or not terminate.  This means that while you can safely CSE 
     * this term (c.f. Tarditi section 6.1), you cannot eliminate it as dead code. 
     *)
    val storeEffect : Nil.exp -> bool

    (* controlEffect e  
     * This function returns true if the expression e may potentially have a 
     * control effect.  In particular, if this function returns false, then the 
     * effect of e is a subset of {A,R,W}.  If this function returns true, 
     * then the effect of e is a subset of {E,N,A,R,W} (that is, any effect).
     * Note that code that does *not* satisfy this predicate may still depend on 
     * or modify the store.  This means that you cannot safely CSE this term
     * nor eliminate it as dead code.  
     *)
    val controlEffect : Nil.exp -> bool

    (* anyEffect e  
     * This function returns true if the expression e may potentially have some 
     * effect.  In particular, if this function returns false, then the effect 
     * of e is a subset of {}.  If this function returns true, then the effect 
     * of e is a subset of {E,N,A,R,W} (that is, any effect). 
     * Note that code that does *not* satisfy this predicate is 
     * guaranteed to be tantamount to a value.
     *)
    val anyEffect : Nil.exp -> bool

    (* Is the primcon covariant with respect to subtyping
     *)
    val covariant_prim: Nil.primcon -> bool

    (* Does a primitive use it's constructor arguments
     * as data, or just a classifiers.
     *)
    val allprim_uses_carg : Nil.allprim -> bool

    (*Translate between paths represented as lists of labels
     * and constructor paths
     *)
    val path2con : Nil.var * Nil.label list -> Nil.con
    val con2path : Nil.con -> (Nil.var * Nil.label list) option

    (*A tuple of constructors: <c1,...,cn> 
     *)
    val con_tuple : Nil.con list -> Nil.con

    (*The type of a tuple:  c1 x...x cn
     *)
    val tuple_con : Nil.con list -> Nil.con

    (*The kind of a tuple of constructors
     *)
    val tuple_kind : Nil.kind list -> Nil.kind

   (*
         Creates the kind for a "tuple" of types of length n. 
         1-tuples yield kind Type, rather than a record kind.
    *)
    val kind_type_tuple : int -> Nil.kind

    val unit_con : Nil.con  (*unit *)
    val unit_exp : Nil.exp  (*()*)

    val string_con : Nil.con(* Type of strings*)

    val int_con : Nil.con   (* 32-bit ints *)
    val zero_int_exp : Nil.exp

    val char_con : Nil.con  (* 8-bit ints *)

    val ftype64  : Nil.con  (*Type of 64 bit floats*)

    val boxfloat_con : Nil.con
    val unboxfloat_con : Nil.con



    val exn_con : Nil.con   (* Exn type   *)

    (* An internal match exception.  This is not the same exception as
     * the user level exception "Match".  If the compiler is correct,
     * this should never be raised.
     *)
    val internal_match_exn : Nil.exp

    (* Dummy sum type used by toRtl to stand in for some unknown (and irrelevant) types.
     * Actually just the type 1+1.
     *)
    val dummy_con : Nil.con




    (* Create a new record, with an appropriate GCTag attached.
     * mk_record_with_gctag lbls traces types exps name
     * lbls   : record labels
     * traces : optional trace args.  If not present, set to TraceUnknown
     * types  : types of fields
     * exps   : field values
     * name   : optional name for the record.
     *
     * If optional name argument is present, then the record will be bound
     * to that variable, and the expression returned will simply be that var
     *)
    val mk_record_with_gctag : 
      (Nil.label list) * (Nil.niltrace list option) * 
      (Nil.con list) * (Nil.exp list) * Nil.var option->  (Nil.bnd list * Nil.exp) 

  end

