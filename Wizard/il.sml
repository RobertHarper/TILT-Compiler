structure IL (* :> IL *) =
    struct

	exception NYI
	exception Impossible

	type tipe_attr = unit		(* for now *)

	(* the external and internal views coincide for types *)
	datatype tipe_ =
	    Unit
	  | Prod of tipe * tipe
	  | Arrow of tipe * tipe

	withtype tipe =
	    Stamp.stamp * tipe_attr * tipe_

	fun eq_tipe ((s, _, _), (s', _, _)) = Stamp.eq (s, s')
	fun tipe_stamp (s, _, _) = s

	fun expose_tipe (_, _, t_) = t_

	structure TipeKey : HASHABLE =
	    struct
		type key = tipe_
		type hashcode = word

		val unit_code = 0w0
		val prod_code = 0w1
		val arrow_code = 0w2

		fun hash Unit = HashVal.nullary unit_code
		  | hash (Prod (t1, t2)) =
		    HashVal.binary (prod_code, tipe_stamp t1, tipe_stamp t2)
		  | hash (Arrow (t1, t2)) =
		    HashVal.binary (arrow_code, tipe_stamp t1, tipe_stamp t2)

		fun eq (Unit, Unit) = true
		  | eq (Prod (t1, t2), Prod (t1', t2')) =
		    eq_tipe (t1, t1') andalso eq_tipe (t2, t2')
		  | eq (Arrow (t1, t2), Arrow (t1', t2')) =
		    eq_tipe (t1, t1') andalso eq_tipe (t2, t2')
		  | eq _ = false
	    end

	structure TipeTable = HCTableFn (structure Key = TipeKey)
	val tipe_table : tipe TipeTable.hc_table = TipeTable.make (1000)

	(* find a way to defer creating t unless it is not found? *)
	fun hide_tipe t_ =
	    let
		val t = (Stamp.new_stamp (), (), t_)
	    in
		TipeTable.find_or_insert tipe_table (t_, t)
	    end

	(* the internal view of terms *)
	datatype term' =
	    FreeVar' of Symbol.symbol
	  | BoundVar' of int
	  | Triv'
	  | Pair' of tipe * tipe * term * term
	  | ProjL' of tipe * tipe * term
	  | ProjR' of tipe * tipe * term
	  | Lam' of tipe * tipe * term
	  | App' of tipe * tipe * term * term

	withtype term = Stamp.stamp * term'

	fun eq_term ((s, _), (s', _)) = Stamp.eq (s, s')

	structure TermKey : HASHABLE =
	    struct
		type key = term'
		type hashcode = word

		val freevar_code = 0w0
		val boundvar_code = 0w1
		val triv_code = 0w2
		val pair_code = 0w3
		val projl_code = 0w4
		val projr_code = 0w5
		val lam_code = 0w6
		val app_code = 0w7

		fun term_stamp (s, _) = s

		fun hash (FreeVar' s) =
		    HashVal.unary (freevar_code, Symbol.hash s)
		  | hash (BoundVar' i) =
		    HashVal.unary (boundvar_code, Word.fromInt i)
		  | hash Triv' = HashVal.nullary triv_code
		  | hash (Pair' (lt, rt, l, r)) =
		    HashVal.quaternary (pair_code, tipe_stamp lt, tipe_stamp rt, term_stamp l, term_stamp r)
		  | hash (ProjL' (lt, rt, a)) =
		    HashVal.ternary (projl_code, tipe_stamp lt, tipe_stamp rt, term_stamp a)
		  | hash (ProjR' (lt, rt, a)) =
		    HashVal.ternary (projr_code, tipe_stamp lt, tipe_stamp rt, term_stamp a)
		  | hash (Lam' (dt, rt, b)) =
		    HashVal.ternary (lam_code, tipe_stamp dt, tipe_stamp rt, term_stamp b)
		  | hash (App' (dt, rt, f, a)) =
		    HashVal.quaternary (app_code, tipe_stamp dt, tipe_stamp rt, term_stamp f, term_stamp a)

		fun eq (FreeVar' s, FreeVar' s') = Symbol.eq (s, s')
		  | eq (BoundVar' i, BoundVar' i') = i=i'
		  | eq (Pair' (lt, rt, l, r), Pair' (lt', rt', l', r')) =
		    eq_tipe (lt, lt') andalso eq_tipe (rt, rt') andalso eq_term (l, l') andalso eq_term (r, r')
		  | eq (ProjL' (lt, rt, p), ProjL' (lt', rt', p')) =
		    eq_tipe (lt, lt') andalso eq_tipe (rt, rt') andalso eq_term (p, p')
		  | eq (ProjR' (lt, rt, p), ProjR' (lt', rt', p')) =
		    eq_tipe (lt, lt') andalso eq_tipe (rt, rt') andalso eq_term (p, p')
		  | eq (Lam' (dt, rt, b), Lam' (dt', rt', b')) =
		    eq_tipe (dt, dt') andalso eq_tipe (rt, rt') andalso eq_term (b, b')
		  | eq (App' (dt, rt, f, a), App' (dt', rt', f', a')) =
		    eq_tipe (dt, dt') andalso eq_tipe (rt, rt') andalso eq_term (f, f') andalso eq_term (a, a')
		  | eq (_, _) = false
	    end

	structure TermTable = HCTableFn (structure Key = TermKey)
	val term_table : term TermTable.hc_table = TermTable.make 1000

	fun make_term t' =
	    let
		val t = (Stamp.new_stamp(), t')
	    in
		TermTable.find_or_insert term_table (t', t)
	    end

	fun dub (s, t) =
	    let
		fun db (i, (_, t')) = make_term (db' (i, t'))
		and db' (i, t' as FreeVar' _) = t'
		  | db' (i, t' as BoundVar' j) =
		    if i=j then FreeVar' s else t' (* j must be less than i *)
		  | db' (i, t' as Triv') = Triv'
		  | db' (i, Pair' (lt, rt, l, r)) =
		    Pair' (lt, rt, db (i, l), db (i, r))
		  | db' (i, ProjL' (lt, rt, p)) = ProjL' (lt, rt, db (i, p))
		  | db' (i, ProjR' (lt, rt, p)) = ProjR' (lt, rt, db (i, p))
		  | db' (i, Lam' (dt, rt, b)) = Lam' (dt, rt, db (i+1, b))
		  | db' (i, App' (dt, rt, f, a)) = App' (dt, rt, db (i, f), db (i, a))
	    in
		db (1, t)
	    end

	fun bind (s, t) =
	    let
		fun bd (i, (_, t')) = make_term (bd' (i, t'))
		and bd' (i, t' as FreeVar' s') =
		    if Symbol.eq (s, s') then
		       BoundVar' i
		    else
		       t'
		  | bd' (i, t' as BoundVar' _) = t'
		  | bd' (i, Triv') = Triv'
		  | bd' (i, Pair' (lt, rt, l, r)) = Pair' (lt, rt, bd (i, l), bd (i, r))
		  | bd' (i, ProjL' (lt, rt, p)) = ProjL' (lt, rt, bd (i, p))
		  | bd' (i, ProjR' (lt, rt, p)) = ProjR' (lt, rt, bd (i, p))
		  | bd' (i, Lam' (dt, rt, b)) = Lam' (dt, rt, bd (i+1, b))
		  | bd' (i, App' (dt, rt, f, a)) = App' (dt, rt, bd (i, f), bd (i, a))
	    in
		bd (1, t)
	    end

	(* the external view of terms *)
	datatype term_ =
	    Var of Symbol.symbol
	  | Triv
	  | Pair of tipe * tipe * term * term
	  | ProjL of tipe * tipe * term 
	  | ProjR of tipe * tipe * term
	  | Lam of tipe * tipe * Symbol.symbol * term
	  | App of tipe * tipe * term * term

	fun expose_term' (FreeVar' s) = Var s
	  | expose_term' (BoundVar' _) = raise Impossible
	  | expose_term' Triv' = Triv
	  | expose_term' (Pair' (lt, rt, l, r)) = Pair (lt, rt, l, r)
	  | expose_term' (ProjL' (lt, rt, p)) = ProjL (lt, rt, p)
	  | expose_term' (ProjR' (lt, rt, p)) = ProjR (lt, rt, p)
	  | expose_term' (Lam' (dt, rt, b)) =
	    let
		val s = Symbol.new_sym ()
		val b' = dub (s, b)	(* we really want to do this lazily *)
	    in
		Lam (dt, rt, s, b')
	    end
	  | expose_term' (App' (dt, rt, f, a)) = App (dt, rt, f, a)

	fun expose_term (_, t') = expose_term' t'

	(* this conversion is the cost of the curtain *)
	fun hide_term' (Var s) = (FreeVar' s)
	  | hide_term' Triv = Triv'
	  | hide_term' (Pair (lt, rt, l, r)) = Pair' (lt, rt, l, r)
	  | hide_term' (ProjL (lt, rt, p)) = ProjL' (lt, rt, p)
	  | hide_term' (ProjR (lt, rt, p)) = ProjR' (lt, rt, p)
	  | hide_term' (Lam (dt, rt, s, b)) =
	    let
		val b' = bind (s, b)	(* we want to do this lazily *)
	    in
		Lam' (dt, rt, b')
	    end
	  | hide_term' (App (dt, rt, f, a)) = App' (dt, rt, f, a)

	fun hide_term t_ = make_term (hide_term' t_)

	(* compute the free variables of a term *)
	fun free_vars (_, t') = free_vars' t'
	and free_vars' (t as FreeVar' s) = Symbol.SymbolSet.singleton (s)
	  | free_vars' (BoundVar' _) = Symbol.SymbolSet.empty
	  | free_vars' Triv' = Symbol.SymbolSet.empty
	  | free_vars' (Pair' (_, _, l, r)) =
	    Symbol.SymbolSet.union (free_vars l, free_vars r)
	  | free_vars' (ProjL' (_, _, p)) = free_vars p
	  | free_vars' (ProjR' (_, _, p)) = free_vars p
	  | free_vars' (Lam' (_, _, b)) = free_vars b (* NB! *)
	  | free_vars' (App' (_, _, f, a)) =
	    Symbol.SymbolSet.union (free_vars f, free_vars a)

	type subst = term Symbol.SymbolMap.map

	(* simultaneous substitution, naive version *)
	(* possible improvement: check if FV(t) intersects dom(S) *)
	(* possible improvement: avoid recomputing stamp in the freevar case *)
	fun subst (S, (_, t')) = make_term (subst' (S, t'))
	and subst' (S, (t' as FreeVar' s)) =
	    (case Symbol.SymbolMap.find (S, s)
	       of NONE => t' | SOME (_, t') => t')
	  | subst' (S, (t' as BoundVar' _)) = t'
	  | subst' (S, Triv') = Triv'
	  | subst' (S, Pair' (lt, rt, l, r)) =
	    Pair' (lt, rt, subst (S, l), subst (S, r))
	  | subst' (S, ProjL' (lt, rt, p)) =
	    ProjL' (lt, rt, subst (S, p))
	  | subst' (S, ProjR' (lt, rt, p)) =
	    ProjR' (lt, rt, subst (S, p))
	  | subst' (S, Lam' (dt, rt, b)) =
	    Lam' (dt, rt, subst (S, b)) (* NB: no need to consider capture *)
	  | subst' (S, App' (dt, rt, f, a)) =
	    App' (dt, rt, subst(S, f), subst (S, a))

	fun format_tipe t = raise NYI
	fun format_term t = raise NYI

    end
