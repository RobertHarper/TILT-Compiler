structure TC (* :> TC *) =
    struct

	type context = IL.tipe Symbol.SymbolMap.map

	(* type equivalence is trivial *)
	fun equiv (G, t, t') = IL.eq_tipe (t, t')

	exception ILTypeError
	exception Impossible

	fun synth (G, t) =
	    synth_ (G, IL.expose_term t)

	and synth_ (G, IL.Var s) =
	    (case Symbol.SymbolMap.find (G, s)
	       of NONE => raise ILTypeError
		| SOME t => t)
	  | synth_ (G, IL.Triv) = IL.hide_tipe (IL.Unit)
	  | synth_ (G, IL.Pair (lt, rt, l, r)) =
	    let
		val _ = anal (G, l, lt)
		val _ = anal (G, r, rt)
	    in
		IL.hide_tipe (IL.Prod (lt, rt))
	    end
	  | synth_ (G, IL.ProjL (lt, rt, p)) =
	    let
		val pt = IL.hide_tipe (IL.Prod (lt, rt))
		val _ = anal (G, p, pt)
	    in
		lt
	    end
	  | synth_ (G, IL.ProjR (lt, rt, p)) =
	    let
		val pt = IL.hide_tipe (IL.Prod (lt, rt))
		val _ = synth (G, p)
	    in
		rt
	    end
	  | synth_ (G, IL.Lam (dt, rt, s, b)) =
	    let
		val G' = Symbol.SymbolMap.insert (G, s, dt)
		val _ = anal (G', b, rt)
	    in
		IL.hide_tipe (IL.Arrow (dt, rt))
	    end
	  | synth_ (G, IL.App (dt, rt, f, a)) =
	    let
		val ft = IL.hide_tipe (IL.Arrow (dt, rt))
		val _ = anal (G, f, ft)
		val _ = anal (G, a, dt)
	    in
		rt
	    end

	and anal (G, tm, tp) =
	    let
		val stp = synth (G, tm)
	    in
		if equiv (G, stp, tp) then
		   ()
		else
		   raise ILTypeError
	    end

	exception ElabError

	fun elab_tipe (_, EL.Unit) =
	    IL.hide_tipe IL.Unit
	  | elab_tipe (G, EL.Prod (t1, t2)) =
	    IL.hide_tipe (IL.Prod (elab_tipe (G, t1), elab_tipe (G, t2)))
	  | elab_tipe (G, EL.Arrow (t1, t2)) =
	    IL.hide_tipe (IL.Arrow (elab_tipe (G, t1), elab_tipe (G, t2)))

	fun get_tipe (G, s) =
	    case Symbol.SymbolMap.find (G, s)
	      of NONE => raise ElabError
	       | SOME t => t

	fun elab_term (G, EL.Var s) =
	    (IL.hide_term (IL.Var s), get_tipe (G, s))
	  | elab_term (_, EL.Triv) =
	    (IL.hide_term (IL.Triv), IL.hide_tipe (IL.Unit))
	  | elab_term (G, EL.Pair (l, r)) =
	    let
		val (l', lt) = elab_term (G, l)
		val (r', rt) = elab_term (G, r)
	    in
		(IL.hide_term (IL.Pair (lt, rt, l', r')), IL.hide_tipe (IL.Prod (lt, rt)))
	    end
	  | elab_term (G, EL.ProjL p) =
	    let
		val (p', pt) = elab_term (G, p)
	    in
		case IL.expose_tipe pt
		  of (IL.Prod (lt, rt)) =>
		     (IL.hide_term (IL.ProjL (lt, rt, p')), lt)
		   | _ => raise ElabError
	    end
	  | elab_term (G, EL.ProjR p) =
	    let
		val (p', pt) = elab_term (G, p)
	    in
		case IL.expose_tipe pt
		  of (IL.Prod (lt, rt)) =>
		     (IL.hide_term (IL.ProjR (lt, rt, p')), rt)
		   | _ => raise ElabError
	    end
	  | elab_term (G, EL.Lam (s, dt, b)) =
	    let
		val dt' = elab_tipe (G, dt)
		val G' = Symbol.SymbolMap.insert (G, s, dt')
		val (b', rt) = elab_term (G', b)
	    in
		(IL.hide_term (IL.Lam (dt', rt, s, b')),
		 IL.hide_tipe (IL.Arrow (dt', rt)))
	    end
	  | elab_term (G, EL.App (f, a)) =
	    let
		val (f', ft) = elab_term (G, f)
		val (a', at) = elab_term (G, a)
	    in
		case IL.expose_tipe ft
		  of IL.Arrow (dt, rt) =>
		     if equiv (G, dt, at) then
			(IL.hide_term (IL.App (dt, rt, f', a')), rt)
		     else
			raise ElabError
		   | _ => raise ElabError
	    end
	  | elab_term (G, EL.Ascription (tm, tp)) =
	    let
		val tp' = elab_tipe (G, tp)
		val (tm', tp'') = elab_term (G, tm)
	    in
		if equiv (G, tp', tp'') then
		   (tm', tp'')
		else
		   raise ElabError
	    end

	fun check_term (G, e, t) =
	    elab_term (G, EL.Ascription (e, t))

    end
