signature LILUTIL = 
  sig
    val w2i : Lil.w32 -> int
    val i2w : int -> Lil.w32

    val wnth : Lil.w32 -> 'a list -> 'a 
    val wnth' : Lil.w32 -> 'a list -> 'a option

    val removewnth  : Lil.w32 -> 'a list -> 'a * 'a list 
    val removewnth' : Lil.w32 -> 'a list -> ('a * 'a list) option
      
    val cmpw32 : (Lil.w32 * Lil.w32) -> order

    val i2size : Prim.intsize -> Lil.size
    val f2size : Prim.floatsize -> Lil.size

    val size2i : Lil.size -> Prim.intsize 
    val size2f : Lil.size -> Prim.floatsize 

    (* Find the strongly connected components of a graph. 
     * Result is a list of list of vars and their info.
     * Each list is a scc.  The sccs are ordered such
     * that there are no edges from an earlier component 
     * to a later one (the opposite of the ordering returned 
     * by GraphUtil).
     *)
    val scc : ((Lil.var * 'info) * Name.VarSet.set) list -> (Lil.var * 'info) list list 
    val break_fix : (Lil.var * Lil.function) list -> (Lil.var * Lil.function) list list 

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
    val storeEffect32 : Lil.op32 -> bool
    val storeEffect64 : Lil.op64 -> bool

    (* controlEffect e
     * This function returns true if the expression e may potentially have a
     * control effect.  In particular, if this function returns false, then the
     * effect of e is a subset of {A,R,W}.  If this function returns true,
     * then the effect of e is a subset of {E,N,A,R,W} (that is, any effect).
     * Note that code that does *not* satisfy this predicate may still depend on
     * or modify the store.  This means that you cannot safely CSE this term
     * nor eliminate it as dead code.
     *)
    val controlEffect32 : Lil.op32 -> bool
    val controlEffect64 : Lil.op64 -> bool

    (* anyEffect e
     * This function returns true if the expression e may potentially have some
     * effect.  In particular, if this function returns false, then the effect
     * of e is a subset of {}.  If this function returns true, then the effect
     * of e is a subset of {E,N,A,R,W} (that is, any effect).
     * Note that code that does *not* satisfy this predicate is
     * guaranteed to be tantamount to a value.
     *)
    val anyEffect32 : Lil.op32 -> bool
    val anyEffect64 : Lil.op64 -> bool


  end
