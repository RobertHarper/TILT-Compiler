signature RENAME =
    sig
	structure Nil : NIL
	val rename_kind : Name.var Name.VarMap.map -> Nil.kind -> Nil.kind
	val rename_con : Name.var Name.VarMap.map -> Nil.con -> Nil.con
	val rename_exp : Name.var Name.VarMap.map -> Nil.exp -> Nil.exp
    end

functor Rename (structure Nil : NIL) : RENAME =
    struct
	structure Nil = Nil
	open Nil
	structure VarMap = Name.VarMap
	exception UNIMP
	exception BUG

	fun lookup (s,v) = case VarMap.find(s,v) of
	    SOME var => var
	  | NONE => v

	fun do_list rename (s:var VarMap.map) ((v,w)::rest) =
	    let val newv = Name.fresh_var()
		val (s', rest) = do_list rename  (VarMap.insert(s,v,newv)) rest
	    in
		(s', (newv, rename s w)::rest)
	    end
	| do_list rename s [] = (s, [])


	fun do_bndlist rename (s:var VarMap.map) (bnd::rest) =
	    let val (s1, bnd) = rename s bnd
		val (s, restbnds) = do_bndlist rename s1 rest
	    in
		(s, bnd::restbnds)
	    end
	| do_bndlist rename s [] = (s, [])



	fun rename_vklist (s:var VarMap.map) (l:(var*kind) list) : (var VarMap.map * (var*kind)list)
	    = do_list rename_kind s l
	and rename_vclist s l = do_list rename_con s l
	and rename_bndlist s  l= do_bndlist rename_bnd s l
	and rename_conbndlist s l = do_bndlist rename_conbnd s l

	and rename_kind (s:var VarMap.map) kind =
	    let val selfk = rename_kind s
		val selfc = rename_con s
	    in
		case kind of
		    ( Type_k _ | Word_k _ ) => kind
		  | Singleton_k (phase, kind, con) =>
			Singleton_k (phase, selfk kind, selfc con)
		  | Record_k lvkseq =>
			Record_k (Util.mapsequence (fn ((l,v), k) =>
						    ((l, lookup(s,v)), selfk k)) lvkseq)
		  | Arrow_k (openness, vklist, kind) =>
			let val (s, vklist) = rename_vklist s vklist
			in (Arrow_k (openness, vklist, rename_kind s kind))
			end
	    end

and rename_con s con =
    let val selfc = rename_con s
	val selfcb = rename_conbnd s
	val selfk = rename_kind s
    in
	case con of
	    Prim_c (pc, cons) => Prim_c (pc, map selfc cons)
	  | Mu_c (bool, vcseq) =>
		let val s = Util.foldsequence (fn ((v,con), s)  => VarMap.insert(s,v, Name.fresh_var() )) s vcseq
		in
		    Mu_c (bool,
			  Util.mapsequence (fn (v,c)=> (lookup (s,v), rename_con s c)) vcseq)
		end
	  | AllArrow_c (openness, eff, vklist, clist, w32, con) =>
		let val (s, vklist) = rename_vklist s vklist
		in
		    AllArrow_c (openness, eff, vklist, map (rename_con s) clist, w32, rename_con s con)
		end
	  | Var_c var => Var_c (lookup (s, var))
	  | Let_c (sort, conbnds, con) =>
		let val (s, conbnds) = rename_conbndlist s conbnds
		in
		    Let_c (sort, conbnds, rename_con s con)
		end
	  | Crecord_c (lclist) => Crecord_c (map (fn (l,c) => (l, selfc c)) lclist)
	  | Proj_c (con, l) => Proj_c (selfc con, l)
	  | Closure_c (con, con2) => Closure_c (selfc con, selfc con2)
	  | App_c (con, cons) => App_c ( selfc con, map selfc cons)
	  | Typecase_c { arg, arms, default, kind} =>
		Typecase_c { arg = selfc arg,
			    arms = map (fn (pc, vklist, con) =>
					let val (s, vklist) = rename_vklist s vklist
					in (pc, vklist, rename_con s con) end ) arms,
			    default = selfc default,
			    kind = selfk kind }
	  | Annotate_c (annot, con) => selfc con
    end

and rename_conbnd s conbnd =
    let val selfc = rename_con s
	val selfk = rename_kind s
    in
	case conbnd of
	    Con_cb (var, kind, con) =>
		let val newvar = Name.fresh_var()
		in
		    (VarMap.insert (s, var, newvar), Con_cb ( newvar, selfk kind, selfc con))
		end
	  | ( Open_cb (var, vklist, con, kind) | Code_cb (var, vklist, con, kind) ) =>
		let val newvar = Name.fresh_var()
		    val (funs, vklist) = rename_vklist s vklist
		in
		    (VarMap.insert(s, var, newvar),
		     Open_cb (newvar, vklist, rename_con funs con, rename_kind funs kind))
		end
    end


and rename_exp s exp =
    let val selfe = rename_exp s
	val selfc = rename_con s
	val selff = rename_fcn s
	val selfs = rename_switch s
    in
	case exp of
	    Var_e v => Var_e (lookup(s, v))
	  | Const_e _ => exp
	  | Let_e (sort, bnds, exp) =>
		let val (s, bnds) = rename_bndlist s bnds
		in
		    Let_e (sort, bnds, rename_exp s exp)
		end
	  | Prim_e (ap, cons, exps) =>
		Prim_e (ap, map selfc cons, map selfe exps)
	  | Switch_e sw =>
		Switch_e (selfs sw)
	  | App_e (openness, exp, cons, exps1, exps2) =>
		App_e (openness, selfe exp, map selfc cons, map selfe exps1, map selfe exps2)
	  | Raise_e (exp,con) => Raise_e (selfe exp, selfc con)
	  | Handle_e (exp, fcn) => Handle_e (selfe exp, selff fcn)
    end

and rename_switch s sw =
    let fun rs do_info do_arg do_t { info, arg, arms, default } =
	{ info = do_info info,
	 arg = do_arg arg,
	 arms = map (fn (t, f) => (do_t t, rename_fcn s f)) arms,
	 default = (case default of
			SOME exp => SOME (rename_exp s exp)
		      | NONE => NONE)
	 }

    in
	case sw of
	    Intsw_e sw => Intsw_e (rs (fn x=>x) (rename_exp s) (fn x=>x) sw)
	  | Sumsw_e sw => Sumsw_e (rs (fn (w32, cons) => (w32, map (rename_con s) cons)) (rename_exp s) (fn x=>x) sw)
	  | Exncase_e sw => Exncase_e (rs (fn x=>x) (rename_exp s) (rename_exp s) sw)
	  | Typecase_e sw => Typecase_e (rs (fn x=>x) (rename_con s) (fn x=>x) sw)
    end
and rename_bnd s bnd =
    case bnd of
	Con_b (v,k,c) =>
	    let val newv = Name.fresh_var()
	    in ( VarMap.insert (s, v, newv), Con_b (newv, rename_kind s k, rename_con s c)) end
	| Exp_b (v, c, e) =>
	      let val newv = Name.fresh_var()
	      in ( VarMap.insert (s, v, newv), Exp_b (newv, rename_con s c, rename_exp s e)) end
	| Fixopen_b vfset =>
	      let val s = Util.foldset (fn ((v,c),s) => (VarMap.insert(s, v, Name.fresh_var()))) s vfset
	      in
		  (s, Fixopen_b (Util.mapset (fn (v,c) => ( lookup(s,v), rename_fcn s c)) vfset))
	      end
	| ( Fixcode_b _ | Fixclosure_b _ ) => raise UNIMP


and rename_fcn s (Function (effect, recur, vklist, vclist, vlist, exp, con )) =
    let val (s,vklist) = rename_vklist s vklist
	val (s,vclist) = rename_vclist s vclist
	val (s, vlist) = foldr (fn (v, (s,l)) =>
				let val newvar = Name.fresh_var()
				in (VarMap.insert (s,v,newvar), (newvar::l)) end) (s,[]) vlist
    in
	Function (effect, recur, vklist, vclist, vlist, rename_exp s exp, rename_con s con)
    end


end







