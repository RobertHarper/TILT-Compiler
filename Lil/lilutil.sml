structure LilUtil :> LILUTIL = 
  struct
    structure VarSet = Name.VarSet
    structure VarMap = Name.VarMap
    structure LO = Listops

    open Prim 
    open Lil

    fun error s = Util.error "lilutil.sml" s

    val w2i = TilWord32.toInt
    val i2w = TilWord32.fromInt

    fun wnth' (iw : w32) (l : 'a list) : 'a option = 
      (case (iw,l)
	 of (0w0,a::aa) => SOME a
	  | (n,a::aa) => wnth' (n-0w1) aa
	  | _ => NONE)
    fun wnth iw l = 
      (case wnth' iw l
	 of SOME a => a
	  | NONE => error "wnth didn't find a value")

    fun removewnth' (iw : w32) (l : 'a list) : ('a * 'a list) option = 
      let
	fun loop (iw,head,tail) = 
	  (case (iw,tail)
	     of (0w0,a::tail) => SOME (a,List.revAppend(head,tail))
	      | (n,a::tail) => loop (n-0w1,a::head,tail)
	      | _ => NONE)
      in loop (iw,[],l)
      end

    fun removewnth iw l = 
      (case removewnth' iw l
	 of SOME a => a
	  | NONE => error "removewnth didn't find a value")

    fun cmpw32 (a,b) =
      if TilWord32.slt (a, b) then
	LESS
      else
	if TilWord32.sgt (a,b) then
	  GREATER
	else EQUAL

  fun value2w32 v =
    (case v
       of int (sz,w64)   => TilWord64.toSignedHalf w64
	| uint (sx,w64)  => TilWord64.toUnsignedHalf w64
	| _ => error "Not a word value")

    fun i2size pis = 
      (case pis 
	 of W8 => B1
	  | W16 => error "unsupported int size"
	  | W32 => B4
	  | W64 => error "unsupported int size")
    fun f2size pfs = 
      (case pfs 
	 of F32 => error "unsupported float size"
	  | F64 => B8)

    fun size2i is = 
      (case is 
	 of B1 => W8
	  | B2 => W16
	  | B4 => W32
	  | B8 => W64)
    fun size2f fs = 
      (case fs 
	 of B1 => error "size has no corresponding floatsize"
	  | B2 => error "size has no corresponding floatsize"
	  | B4 => F32
	  | B8 => F64)


    (* Find the strongly connected components of the graph.
     * Result is a list of list of vars and their info.
     * Each list is a scc.  The sccs are ordered such
     * that there are no edges from an earlier component 
     * to a later one (the opposite of the ordering returned 
     * by GraphUtil).
     *)
    fun scc (vars : ((var * 'a) * VarSet.set) list) : (var * 'a) list list = 
      let
	val var2i = 
	  let
	    val map = foldl (fn ((v,i),m) => VarMap.insert(m,v,i)) VarMap.empty (LO.mapcount (fn (i,((v,a),vars)) => (v,i)) vars)
	  in fn v => valOf (VarMap.find (map,v))
	  end
	val imap = Array.fromList vars
	fun i2va i = #1 (Array.sub(imap,i))
	val nodes = Array.length imap
	fun get_edge (i,(_,edgeset),edges) = VarSet.foldl (fn (v,edges) => (i,var2i v) :: edges) edges edgeset
	val edges = Array.foldli get_edge [] (imap,0,NONE)
	val components = GraphUtil.scc nodes edges
	val components = map (fn l => map i2va l) (rev components)
      in components
      end

      
    (* Break a fix binding into its strongly connected components *)
    fun break_fix (vfs : (var * function) list) : (var * function) list list = 
      (case vfs 
	 of [_] => [vfs]
	  | _ => 
	   let
	     (* The set of functions in the nest*)
	     val fset = VarSet.addList(VarSet.empty,map #1 vfs)
	       
	     (* For each function, get the set of functions from the nest to 
	      * which it refers.  These comprise edges in a graph where the nodes
	      * are the functions.
	      *)
	     fun get_frees (v,f) = 
	       let
		 val frees = VarSet.intersection(fset,LilFrees.E.freeInFunction f)
	       in ((v,f),frees)
	       end

	     val vifrees = map get_frees vfs

	     (* Get the strongly connected components of the graph *)
	     val components = scc vifrees

	   in components
	   end)



    fun primcheck (store,control) = 
      (case (store,control)
	 of (true,true)  => Prim.has_effect
	  | (true,false) => Prim.store_effect
	  | (false,true) => Prim.control_effect
	  | _ => fn _ => true)

    (*See individual comments below
     *)
    fun effect64 (store,control) oper =
      let
	fun check oper =
	  case oper
	    of (Val_64 _)     => false
	     | (Unbox _) => false
	     | (Prim64 (p,_)) => primcheck (store,control) p
	     | (ExternAppf _) => true
      in check oper
      end

    (*See individual comments below
     *)
    fun effect32 (store,control) oper =
      let
	fun check oper =
	  case oper
	    of (Val _)     => false
	     | (Prim32 (p,_,_)) => primcheck (store,control) p
	     | (LilPrimOp32 (lp,_,_,_)) => (lp = Dyntag) andalso store
	     | Switch (Ifthenelse {arg,thenArm,elseArm,rtype}) => 
	      cccheck arg orelse echeck thenArm orelse echeck elseArm
	     | _ => true
	and cccheck cc =
	  (case cc
	     of Exp_cc e => echeck e
	      | Not_cc cc => cccheck cc
	      | And_cc (cc1,cc2) => cccheck cc1 orelse cccheck cc2
	      | Or_cc (cc1,cc2) => cccheck cc1 orelse cccheck cc2)
	and echeck exp = 
	  (case #e exp
	     of Let_e (bnds,e) => (Listops.orfold bcheck bnds) orelse echeck e
	      | Val32_e sv => false)
	and bcheck bnd = 
	  (case bnd
	     of (Exp32_b (_,oper)) => check oper
	      | (Exp64_b (_,oper)) => effect64 (store,control) oper
	      | _ => false)
      in check oper
      end


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
    val storeEffect32 = effect32 (true,false)
    val storeEffect64 = effect64 (true,false)

    (* controlEffect e
     * This function returns true if the expression e may potentially have a
     * control effect.  In particular, if this function returns false, then the
     * effect of e is a subset of {A,R,W}.  If this function returns true,
     * then the effect of e is a subset of {E,N,A,R,W} (that is, any effect).
     * Note that code that does *not* satisfy this predicate may still depend on
     * or modify the store.  This means that you cannot safely CSE this term
     * nor eliminate it as dead code.
     *)
    val controlEffect32 = effect32 (false,true)
    val controlEffect64 = effect64 (false,true)


    (* anyEffect e
     * This function returns true if the expression e may potentially have some
     * effect.  In particular, if this function returns false, then the effect
     * of e is a subset of {}.  If this function returns true, then the effect
     * of e is a subset of {E,N,A,R,W} (that is, any effect).
     * Note that code that does *not* satisfy this predicate is
     * guaranteed to be tantamount to a value.
     *)
    val anyEffect32 = effect32 (true,true)
    val anyEffect64 = effect64 (true,true)

  end