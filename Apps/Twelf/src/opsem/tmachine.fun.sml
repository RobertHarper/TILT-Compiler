(* Abstract Machine for Tracing *)
(* Author: Frank Pfenning *)

functor TMachine (structure IntSyn' : INTSYN
		  structure CompSyn' : COMPSYN
		    sharing CompSyn'.IntSyn = IntSyn'
		  structure Unify : UNIFY
		    sharing Unify.IntSyn = IntSyn'
		  structure Index : INDEX
		    sharing Index.IntSyn = IntSyn'
		  structure Trail : TRAIL
		    sharing Trail.IntSyn = IntSyn'
                  structure CPrint : CPRINT 
                    sharing CPrint.IntSyn = IntSyn'
                    sharing CPrint.CompSyn = CompSyn'
		  structure Names : NAMES
		    sharing Names.IntSyn = IntSyn'
		  structure Trace : TRACE
		    sharing Trace.IntSyn = IntSyn')
  : ABSMACHINE =
struct

  structure IntSyn = IntSyn'
  structure CompSyn = CompSyn'

  local
    structure I = IntSyn
    structure T = Trace
    structure N = Names
    structure C = CompSyn
  in

    fun subgoalNum (I.Nil) = 1
      | subgoalNum (I.App (U, S)) = 1 + subgoalNum S

    (* currently unused *)
    fun goalToType (C.All (D, g), s) =
          I.Pi ((I.decSub (D,s), I.Maybe), goalToType (g, I.dot1 s))
      | goalToType (C.Impl (_, A, _, g), s) =
	  I.Pi ((I.Dec (NONE, I.EClo (A, s)), I.No), goalToType (g, I.dot1 s))
      | goalToType (C.Atom(p), s) =
	  I.EClo (p, s)

  (* We write
       G |- M : g
     if M is a canonical proof term for goal g which could be found
     following the operational semantics.  In general, the
     success continuation sc may be applied to such M's in the order
     they are found.  Backtracking is modeled by the return of
     the success continuation.

     Similarly, we write
       G |- S : r
     if S is a canonical proof spine for residual goal r which could
     be found following the operational semantics.  A success continuation
     sc may be applies to such S's in the order they are found and
     return to indicate backtracking.
  *)

  (* solve ((g, s), dp, sc) => ()
     Invariants:
       dp = (G, dPool) where  G ~ dPool  (context G matches dPool)
       G |- s : G'
       G' |- g  goal
       if  G |- M : g[s]
       then  sc M  is evaluated
     Effects: instantiation of EVars in g, s, and dp
              any effect  sc M  might have
  *)
  fun solve ((C.Atom(p), s), dp as C.DProg (G, dPool), sc) =
      matchAtom ((p,s), dp, sc)
    | solve ((C.Impl(r, A, a, g), s), C.DProg (G, dPool), sc) =
      let
	val D' as I.Dec(SOME(x),_) = N.decUName (G, I.Dec(NONE, I.EClo(A,s)))
	val _ = T.signal (G, T.IntroHyp (I.Const(a), D'))
      in
	solve ((g, I.dot1 s), C.DProg (I.Decl(G, D'), I.Decl (dPool, SOME(r, s, a))),
	       (fn M => (T.signal (G, T.DischargeHyp (I.Const(a), D'));
			 sc (I.Lam (D', M)))))
      end
    | solve ((C.All(D, g), s), C.DProg (G, dPool), sc) =
      let
	val D' as I.Dec(SOME(x),V) = N.decUName (G, I.decSub (D, s))
	val a = I.targetFam V
	val _ = T.signal (G, T.IntroParm (I.Const(a), D'))
      in
	solve ((g, I.dot1 s), C.DProg (I.Decl(G, D'), I.Decl(dPool, NONE)),
	       (fn M => (T.signal (G, T.DischargeParm (I.Const(a),  D'));
			 sc (I.Lam (D', M)))))
      end

  (* rsolve ((p,s'), (r,s), dp, (Hc, Ha), sc) = ()
     Invariants: 
       dp = (G, dPool) where G ~ dPool
       G |- s : G'
       G' |- r  resgoal
       G |- s' : G''
       G'' |- p : H @ S' (mod whnf)
       if G |- S : r[s]
       then sc S is evaluated
       Hc is the clause which generated this residual goal
       Ha is the target family of p and r (which must be equal)
     Effects: instantiation of EVars in p[s'], r[s], and dp
              any effect  sc S  might have
  *)
  and rSolve (ps', (C.Eq(Q), s), C.DProg (G, dPool), HcHa, sc) =
      (T.signal (G, T.Unify (HcHa, I.EClo (Q, s), I.EClo ps'));
       case Unify.unifiable' (G, (Q, s), ps') (* effect: instantiate EVars *)
	 of NONE => (T.signal (G, T.Resolved HcHa);
		     sc I.Nil;		(* call success continuation *)
		     true)		(* deep backtracking *)
	  | SOME(msg) => (T.signal (G, T.FailUnify (HcHa, msg));
			  false)	(* shallow backtracking *)
      )
    | rSolve (ps', (C.And(r, A, g), s), dp as C.DProg (G, dPool), HcHa, sc) =
      let
	(* is this EVar redundant? -fp *)
	val X = I.newEVar (G, I.EClo(A, s))
      in
        rSolve (ps', (r, I.Dot(I.Exp(X), s)), dp, HcHa,
		(fn S => 
		 (T.signal (G, T.Subgoal (HcHa, fn () => subgoalNum S));
		  solve ((g, s), dp,
			 (fn M => 
			  (sc (I.App (M, S))))))))
      end
    | rSolve (ps', (C.Exists(I.Dec(_,A), r), s), dp as C.DProg (G, dPool), HcHa, sc) =
      let
	val X = I.newEVar (G, I.EClo (A,s))
      in
	rSolve (ps', (r, I.Dot(I.Exp(X), s)), dp, HcHa,
		(fn S => sc (I.App(X,S))))
      end

  (* matchatom ((p, s), dp, sc) => ()
     Invariants:
       dp = (G, dPool) where G ~ dPool
       G |- s : G'
       G' |- p : type, p = H @ S mod whnf
       if G |- M :: p[s]
       then sc M is evaluated
     Effects: instantiation of EVars in p[s] and dp
              any effect  sc M  might have

     This first tries the local assumptions in dp then
     the static signature.
  *)
  and matchAtom (ps' as (I.Root(Ha as I.Const(a),_),_), dp as C.DProg (G,dPool), sc) =
      let
        (* matchSig [c1,...,cn] = ()
	   try each constant ci in turn for solving atomic goal ps', starting
           with c1.
        *)
	val tag = T.tagGoal ()
	val _ = T.signal (G, T.SolveGoal (tag, Ha, I.EClo ps'))
	fun matchSig nil =
	    (T.signal (G, T.FailGoal (tag, Ha, I.EClo ps'));
	     ())			(* return indicates failure *)
	  | matchSig ((Hc as I.Const c)::sgn') =
	    let
	      val C.SClause(r) = C.sProgLookup c
	    in
	      (* trail to undo EVar instantiations *)
	      if
		Trail.trail (fn () =>
			     rSolve (ps', (r, I.id), dp, (Hc, Ha),
				     (fn S => (T.signal (G, T.SucceedGoal (tag, (Hc, Ha), I.EClo ps'));
					       sc (I.Root(Hc, S))))))
		then (* deep backtracking *)
		  (T.signal (G, T.RetryGoal (tag, (Hc, Ha), I.EClo ps'));
		   ())
	      else (* shallow backtracking *)
		();
	      matchSig sgn'
	    end

        (* matchDProg (dPool, k) = ()
	   where k is the index of dPool in global dPool from call to matchAtom.
           Try each local assumption for solving atomic goal ps', starting
           with the most recent one.
        *)
	fun matchDProg (I.Null, _) =
	    (* dynamic program exhausted, try signature *)
	    matchSig (Index.lookup a) 
	  | matchDProg (I.Decl (dPool', SOME(r, s, a')), k) =
	    if a = a'
	      then (* trail to undo EVar instantiations *)
		(if
		   Trail.trail (fn () =>
				rSolve (ps', (r, I.comp(s, I.Shift(k))),
					dp, (I.BVar(k), Ha),
					(fn S => sc (I.Root(I.BVar(k), S)))))
		   then (* deep backtracking *)
		     (T.signal (G, T.RetryGoal (tag, (I.BVar(k), Ha), I.EClo ps'));
		      ())
		 else (* shallow backtracking *)
		   ();
		 matchDProg (dPool', k+1))
	    else matchDProg (dPool', k+1)
	  | matchDProg (I.Decl (dPool', NONE), k) =
	      matchDProg (dPool', k+1)
      in
	matchDProg (dPool, 1)
      end

  val solve = fn (gs, dp, sc) =>
                 (T.init (); solve (gs, dp, sc))
  end (* local ... *)

end; (* functor TMachine *)
