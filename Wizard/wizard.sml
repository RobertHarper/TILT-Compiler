structure Wizard (* :> WIZARD *) =
    struct
	exception Impossible

	(* preliminaries *)
	type stamp = int
	val last_stamp = ref 0
	fun new_stamp () =
	    (
	     last_stamp := !last_stamp + 1;
	     !last_stamp
	     )

	type hashcode = word


	(* type stuff *)

	type tipe_attr = unit

	(* we don't actually need itipe to be different from tipe_ yet, but we will eventually *)
	datatype itipe =
	    IUnit
	  | IProd of tipe * tipe
	  | IArrow of tipe * tipe

	withtype tipe = 
	    itipe * stamp * hashcode * tipe_attr

	datatype tipe_ =
	    Unit
	  | Prod of tipe * tipe
	  | Arrow of tipe * tipe

	fun eq_tipe ((it, s, h, _), (it', s', h', _)) =
	    s = s' orelse
	    (h = h' andalso
	     eq_itipe (it, it'))

	and eq_itipe (IUnit, IUnit) = true
	  | eq_itipe (IProd (t1, t2), IProd (t1', t2')) =
	    eq_tipe (t1, t1') andalso
	    eq_tipe (t2, t2')
	  | eq_itipe (IArrow (t1, t2), IArrow (t1', t2')) =
	    eq_tipe (t1, t1') andalso
	    eq_tipe (t2, t2')
	  | eq_itipe _ = false

	structure TipeKey :> HASH_KEY where type key = itipe =
	    struct
		type key = itipe
		type hashcode = word

		val base = 0w65599

		val unit_code = 0w1
		val prod_code = 0w2
		val arrow_code = 0w3 

		fun hash IUnit = unit_code
		  | hash (IProd (t1, t2)) =
		    prod_code + base * ((#3 t1) + base * (#3 t2))
		  | hash (IArrow (t1, t2)) =
		    arrow_code + base * ((#3 t1) + base * (#3 t2))

		val eq = eq_itipe
	    end

	val hash_tipe = TipeKey.hash

	structure TipeTable = WeakHashTableFun (structure Key = TipeKey)
	val tipe_table : tipe TipeTable.hashtbl = TipeTable.make 10273

	fun hide_tipe t_ =
	    let val it =
		case t_ of
		    Unit => IUnit
		  | Prod (t1, t2) => IProd (t1, t2)
		  | Arrow (t1, t2) => IArrow (t1, t2)
	    in
		TipeTable.find_or_insert tipe_table it
		(fn () => (it, new_stamp (), hash_tipe it, ()))
	    end

	fun expose_tipe (it, _, _, _) =
	    (case it of
		 IUnit => Unit
	       | IProd (t1, t2) => Prod (t1, t2)
	       | IArrow (t1, t2) => Arrow (t1, t2))


	(* term stuff *)

	type term_attr = {maxind : int, fv : Symbol.SymbolSet.set SMLofNJ.Susp.susp}

	datatype iterm =
	    IFreeVar of Symbol.symbol
	  | IBoundVar of int
	  | ITriv
	  | IPair of tipe * tipe * term * term
	  | IProjL of tipe * tipe * term
	  | IProjR of tipe * tipe * term
	  | ILam of tipe * tipe * term
	  | IApp of tipe * tipe * term * term

	withtype term =
	    iterm * stamp * hashcode * term_attr

	datatype term_ =
	    Var of Symbol.symbol
	  | Triv
	  | Pair of tipe * tipe * term * term
	  | ProjL of tipe * tipe * term
	  | ProjR of tipe * tipe * term
	  | Lam of tipe * tipe * Symbol.symbol * term
	  | App of tipe * tipe * term * term

	fun eq_term ((it, s, h, _), (it', s', h', _)) =
	    s = s' orelse
	    (h = h' andalso
	     eq_iterm (it, it'))

	and eq_iterm (IFreeVar s, IFreeVar s') = Symbol.eq (s, s')
	  | eq_iterm (IBoundVar i, IBoundVar i') = i = i'
	  | eq_iterm (ITriv, ITriv) = true
	  | eq_iterm (IPair (lt, rt, t1, t2), IPair (lt', rt', t1', t2')) =
	    eq_tipe (lt, lt') andalso
	    eq_tipe (rt, rt') andalso
	    eq_term (t1, t1') andalso
	    eq_term (t2, t2')
	  | eq_iterm (IProjL (lt, rt, t), IProjL (lt', rt', t')) =
	    eq_tipe (lt, lt') andalso
	    eq_tipe (rt, rt') andalso
	    eq_term (t, t')
	  | eq_iterm (IProjR (lt, rt, t), IProjR (lt', rt', t')) =
	    eq_tipe (lt, lt') andalso
	    eq_tipe (rt, rt') andalso
	    eq_term (t, t')
	  | eq_iterm (ILam (dt, rt, t), ILam (dt', rt', t')) =
	    eq_tipe (dt, dt') andalso
	    eq_tipe (rt, rt') andalso
	    eq_term (t, t')
	  | eq_iterm (IApp (dt, rt, t1, t2), IApp (dt', rt', t1', t2')) =
	    eq_tipe (dt, dt') andalso
	    eq_tipe (rt, rt') andalso
	    eq_term (t1, t1') andalso
	    eq_term (t2, t2')
	  | eq_iterm _ = false

	structure TermKey :> HASH_KEY where type key = iterm =
	    struct
		type key = iterm
		type hashcode = word

		val base = 0w65599

		val freevar_code = 0w1
		val boundvar_code = 0w2
		val triv_code = 0w3
		val pair_code = 0w4
		val projl_code = 0w5
		val projr_code = 0w6
		val lam_code = 0w7
		val app_code = 0w8

		fun hash (IFreeVar s) =
		    freevar_code + base * Symbol.hash s
		  | hash (IBoundVar i) =
		    boundvar_code + base * Word.fromInt i
		  | hash ITriv = triv_code
		  | hash (IPair (lt, rt, l, r)) =
		    pair_code + base * ((#3 lt) + base * ((#3 rt) + base * ((#3 l) + base * (#3 r))))
		  | hash (IProjL (lt, rt, t)) =
		    projl_code + base * ((#3 lt) + base * ((#3 rt) + base * (#3 t)))
		  | hash (IProjR (lt, rt, t)) =
		    projr_code + base * ((#3 lt) + base * ((#3 rt) + base * (#3 t)))
		  | hash (ILam (dt, rt, b)) =
		    lam_code + base * ((#3 dt) + base * ((#3 rt) + base * (#3 b)))
		  | hash (IApp (dt, rt, f, a)) =
		    pair_code + base * ((#3 dt) + base * ((#3 rt) + base * ((#3 f) + base * (#3 a))))

		val eq = eq_iterm
	    end

	val hash_term = TermKey.hash

	structure TermTable = WeakHashTableFun (structure Key = TermKey)
	val term_table : term TermTable.hashtbl = TermTable.make 10273

	local
	    open SMLofNJ.Susp
	    open Symbol.SymbolSet
	in
	    fun compute_attr (IFreeVar s) =
		{maxind = 0, fv = delay (fn () => singleton s)}
	      | compute_attr (IBoundVar i) =
		{maxind = i, fv = delay (fn () => empty)}
	      | compute_attr ITriv =
		{maxind = 0, fv = delay (fn () => empty)}
	      | compute_attr (IPair (_, _, l, r)) =
		let val (_, _, _, attr1) = l
		    val (_, _, _, attr2) = r
		in
		    {maxind = Int.max (#maxind attr1, #maxind attr2),
		     fv = delay (fn () => union (force (#fv attr1), force (#fv attr2)))}
		end
	      | compute_attr (IProjL (_, _, t)) =
		let val (_, _, _, attr) = t
		in
		    attr
		end
	      | compute_attr (IProjR (_, _, t)) =
		let val (_, _, _, attr) = t
		in
		    attr
		end
	      | compute_attr (ILam (_, _, b)) =
		let val (_, _, _, attr) = b
		in
		    {maxind = #maxind attr - 1,
		     fv = #fv attr}
		end
	      | compute_attr (IApp (_, _, f, a)) =
		let val (_, _, _, attr1) = f
		    val (_, _, _, attr2) = a
		in
		    {maxind = Int.max (#maxind attr1, #maxind attr2),
		     fv = delay (fn () => union (force (#fv attr1), force (#fv attr2)))}
		end
	end

	fun make_term it =
	    TermTable.find_or_insert term_table it
	    (fn () => (it,
		       new_stamp (),
		       hash_term it,
		       compute_attr it))

	fun bind (s, t) =
	    let
		fun bd (i, t as (it, _, _, attr)) =
		    if Symbol.SymbolSet.member (SMLofNJ.Susp.force (#fv attr), s) then
			make_term (bd' (i, it))
		    else
			t
		and bd' (i, t' as IFreeVar s') =
		    if Symbol.eq (s, s') then
		       IBoundVar i
		    else
		       t'
		  | bd' (i, t' as IBoundVar _) = t'
		  | bd' (i, ITriv) = ITriv
		  | bd' (i, IPair (lt, rt, l, r)) = IPair (lt, rt, bd (i, l), bd (i, r))
		  | bd' (i, IProjL (lt, rt, p)) = IProjL (lt, rt, bd (i, p))
		  | bd' (i, IProjR (lt, rt, p)) = IProjR (lt, rt, bd (i, p))
		  | bd' (i, ILam (dt, rt, b)) = ILam (dt, rt, bd (i+1, b))
		  | bd' (i, IApp (dt, rt, f, a)) = IApp (dt, rt, bd (i, f), bd (i, a))
	    in
		bd (1, t)
	    end

	fun hide_term t_ =
	    make_term (case t_ of
			   Var s => IFreeVar s
			 | Triv => ITriv
			 | Pair (lt, rt, l, r) => IPair (lt, rt, l, r)
			 | ProjL (lt, rt, t) => IProjL (lt, rt, t)	
			 | ProjR (lt, rt, t) => IProjR (lt, rt, t)
			 | Lam (dt, rt, s, b) =>
			       (* Do this eagerly, because bind coalesces type representations, saving space.
				*   On the other hand, the time saved by doing this lazily might (!) make up
				*   for the cost in terms of space; it would be good to see *)
			       let val b' = bind (s, b)
			       in
				   ILam (dt, rt, b')
			       end
			 | App (dt, rt, f, a) => IApp (dt, rt, f, a))

	(* invariant: no term released through the curtain will have bound variables reaching
	 * outside the term; consequently, t's highest level is 1, and thus thus no index higher
	 * than i will ever be encountered *)
	fun dub (s, t) =
	    let
		fun db (i, t as (it, _, _, attr)) =
		    if #maxind attr < i then
			t
		    else
			make_term (db' (i, it))
		and db' (i, t' as IFreeVar _) = t'
		  | db' (i, t' as IBoundVar j) =
		    if i = j then
			IFreeVar s
		    else
			(* j must be less than i *)
			t'
		  | db' (i, t' as ITriv) = ITriv
		  | db' (i, IPair (lt, rt, l, r)) = IPair (lt, rt, db (i, l), db (i, r))
		  | db' (i, IProjL (lt, rt, p)) = IProjL (lt, rt, db (i, p))
		  | db' (i, IProjR (lt, rt, p)) = IProjR (lt, rt, db (i, p))
		  | db' (i, ILam (dt, rt, b)) = ILam (dt, rt, db (i+1, b))
		  | db' (i, IApp (dt, rt, f, a)) = IApp (dt, rt, db (i, f), db (i, a))
	    in
		db (1, t)
	    end

        fun expose_term (it, _, _, _) =
	    (case it of
		 IFreeVar s => Var s
	       | IBoundVar _ => raise Impossible
	       | ITriv => Triv
	       | IPair (lt, rt, l, r) => Pair (lt, rt, l, r)
	       | IProjL (lt, rt, t) => ProjL (lt, rt, t)
	       | IProjR (lt, rt, t) => ProjR (lt, rt, t)
	       | ILam (dt, rt, b) =>
		     let val s = Symbol.new_sym ()
			 (* this should be done lazily, because dub breaks off distinct copies of types *)
			 val b' = dub (s, b)
		     in
			 Lam (dt, rt, s, b')
		     end
	       | IApp (dt, rt, f, a) => App (dt, rt, f, a))

	fun free_vars (_, _, _, _, attr : term_attr) = SMLofNJ.Susp.force (#fv attr)

    end
