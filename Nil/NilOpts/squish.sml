

functor Squish(structure Nil : NIL
		   ) : SQUISH = 
    struct
	structure Nil = Nil
	exception UNIMP
	open Nil

	fun mapitem func item list = 
	    map (fn x => func item x) list
	    
	fun xbnd (f as { bnd, exp, func, switch}) bind =
	    let val self = xbnd f 
	    in
		case bind of
		    Con_b(var, kind, con) => (#bnd f) (Con_b(var,kind,con))
		  | Exp_b (var, con, exp ) =>
			(#bnd f) ( Exp_b (var, con, xexp f exp ) )
		  | Fixopen_b vfset =>
			let val vflist = Util.set2list vfset
			in
			    (#bnd f) (Fixopen_b (Util.list2set (map (fn (v,fc) => (v, xfunction f fc))  vflist)))
			end
		  | _ => raise UNIMP
	    end    
	and xfunction (f as { bnd, exp, func, switch}) (Function(eff, r, vks, vcs, vs, expr, con)) = 
	    #func f  (Function(eff, r, vks, vcs, vs, xexp f expr, con))
	    
	and xswitch  (f as { bnd, exp, func, switch}) s =
	    let fun id _ x  = x
		fun do_sw  do_t {arms,default,arg,info } =
		    let val newarms = map (fn (t,Function(a,b,c,d,e,exp,h)) => 
					   (do_t f t, Function(a,b,c,d,e,xexp f exp,h))) arms
			val newdefault = case default of 
			    SOME exp => SOME (xexp f exp)
			  | NONE => NONE 
		    in {arms = newarms, default = newdefault, arg = arg, info= info}
		    end
	    in
		case s of
		    Intsw_e sw => Intsw_e (do_sw id sw)
		  | Sumsw_e sw => Sumsw_e (do_sw id sw)
		  | Exncase_e sw => Exncase_e (do_sw xexp sw) 
		  | Typecase_e sw => Typecase_e (do_sw id sw)
	    end
	and xexp (f as { bnd, exp, func, switch})  expr =
	    let val self = xexp f 
	    in
		case expr of 
		    Var_e v => #exp(f) expr
		  | Const_e v => #exp f expr
		  | Let_e (sort, bnds, exp ) =>  
			#exp f (Let_e (sort, mapitem xbnd f bnds, self exp))
		  | Prim_e (allp, cons, exps ) => 
			#exp f (Prim_e (allp, cons, map self exps))
		  | Switch_e (sw) => #exp f (Switch_e (xswitch f sw))
		  | App_e (openness, func, cons, exps1, exps2) =>
			#exp f (App_e (openness, self func, cons, map self exps1,map self exps2))
		  | Raise_e (e,c) => #exp f (Raise_e (xexp f e, c))
		  | Handle_e (e,fc) => #exp f (Handle_e (xexp f e, xfunction f fc))
	    end
	
	fun squish exp = 
	    let val id = fn x => x
		fun sq exp =  
		    case exp of
			Let_e (sort, bndlist, Let_e (sort2, bndlist2, exp)) =>
			    if sort= sort2 then
				Let_e (sort, bndlist @ bndlist2, exp)
			    else exp
		      | _ => exp
			    
	    in 
		xexp { exp = sq, switch = id , bnd = fn bnd:bnd => bnd, func =id  } exp
	    end 
    end (* local for squish *)
