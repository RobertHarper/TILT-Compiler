functor Vararg(structure Nil : NIL
	       structure NilUtil : NILUTIL
	       sharing NilUtil.Nil = Nil)
    =

struct
    
    open Nil
    open NilUtil
    open Name

    (* produces term-level functions with the following signature and definition:
     * produces constructor-level functions with the following signature and definition:
     * i is a compile-time constant that is architecturally dependent
     * 
     * vararg(i) : All t::W,t'::W. (All.[t;]->t') -> Vararg(i)(t,t')
     * onearg(i) : All t::W,t'::W. Vararg(i)(t,t') -> (All.[t;]->t')
     * Vararg(i) : [t::W,t'::W] => Vararg(i)(t,t')
     *
     * vararg(i)[t,t'] f = 
     *   typecase t' of
     *     record[] => \[;].f (record[])
     *   | record[c1,...,cn] => \[x1:T(c1),...,xn:T(cn) ; ].     (n <= i)
     *                                f (record[x1,...,xn])
     *   | _ => f
     *
     * oneargarg(i)[t,t'] f = 
     *   typecase t' of
     *     record[] => \[x1 : record[];]. f [;]
     *   | record[c1,...,cn] => \[x: record(T(c1),...,T(cn)) ; ].     (n <= i)
     *                                f [x.1, ..., x.n; ]
     *   | _ => f
     *
     *
     *)

    fun vararg i : function = 
	let
	    val domain_tvar = fresh_var()
	    val range_tvar = fresh_var()
	    val vklist = [(domain_tvar,Type_k Runtime),
			  (range_tvar,Type_k Runtime)]
	    val funvar = fresh_var()
	    val funtype = AllArrow_c(Open,Partial,[],[Var_c domain_tvar],0w0,Var_c range_tvar)
	    val vclist = [(funvar,funtype)]
	    val newfunvar = fresh_var()
	    val newfuntype = Prim_c(Vararg_c(Open,Partial),[Var_c domain_tvar])
	    val typevar = fresh_var()
	    val restypevar = fresh_var()
	    val typearg = Var_c typevar
	    fun make_arm n = 
		let val labels = Listops.map0count generate_tuple_label n
		    val primcon = Record_c labels
		    val rescon = Var_c restypevar
		    val vklist = Listops.map0count (fn _ => (fresh_var(), Word_k Runtime)) n
		    val vclist = map (fn (v,_) => (fresh_var(), Var_c v)) vklist
		    val body = App_e(Open, Var_e funvar,[],
				     [Prim_e(NilPrimOp(Nil.record labels),
					     map #2 vclist,
					     map Var_e (map #1 vclist))], [])
		    val funbnd = Function(Partial, Nonleaf,[],vclist,[],body,rescon)
		    val newfuncon = AllArrow_c(Open,Partial,[],map #2 vclist,0w0,rescon)
		    val bnd = Fixopen_b(Util.list2set[(newfunvar,funbnd)])
		    val newfun = Let_e(Sequential,[bnd],Var_e newfunvar)
		in  (primcon,Function(Partial,Nonleaf,vklist,[],[],newfun,newfuncon))
		end
	    val arms = Listops.map0count make_arm i
	    val default = SOME (Var_e funvar)
	    val body = Switch_e(Typecase_e{info=(),arg=typearg,arms=arms,default=default})
	in  Function(Partial,Nonleaf,vklist,vclist,[],body,newfuntype)
	end


    fun onearg i : function = 
	let
	    val domain_tvar = fresh_var()
	    val range_tvar = fresh_var()
	    val vklist = [(domain_tvar,Type_k Runtime),
			  (range_tvar,Type_k Runtime)]
	    val funvar = fresh_var()
	    val funtype = Prim_c(Vararg_c(Open,Partial),[Var_c domain_tvar])
	    val vclist = [(funvar,funtype)]
	    val newfunvar = fresh_var()
	    val newfuntype = AllArrow_c(Open,Partial,[],[Var_c domain_tvar],0w0,Var_c range_tvar)
	    val typevar = fresh_var()
	    val restypevar = fresh_var()
	    val typearg = Var_c typevar
	    fun make_arm n = 
		let val labels = Listops.map0count generate_tuple_label n
		    val primcon = Record_c labels
		    val rescon = Var_c restypevar
		    val vklist = Listops.map0count (fn _ => (fresh_var(), Word_k Runtime)) n
		    val argv = fresh_var()
		    val rectypes = map (Var_c o #1) vklist
		    val argtype = Prim_c(Record_c labels, rectypes)
		    fun make_proj l = Prim_e(NilPrimOp(select l), rectypes, [Var_e argv])
		    val projects = map make_proj labels
		    val body = App_e(Open, Var_e funvar,[],projects,[])
		    val funbnd = Function(Partial, Nonleaf,[],[(argv,argtype)],[],body,rescon)
		    val newfuncon = AllArrow_c(Open,Partial,[],[argtype],0w0,rescon)
		    val bnd = Fixopen_b(Util.list2set[(newfunvar,funbnd)])
		    val newfun = Let_e(Sequential,[bnd],Var_e newfunvar)
		in  (primcon,Function(Partial,Nonleaf,vklist,[],[],newfun,newfuncon))
		end
	    val arms = Listops.map0count make_arm i
	    val default = SOME (Var_e funvar)
	    val body = Switch_e(Typecase_e{info=(),arg=typearg,arms=arms,default=default})
	in  Function(Partial,Nonleaf,vklist,vclist,[],body,newfuntype)
	end

    fun Vararg i : (var * kind) list * con * kind = 
	let
	    val domain_tvar = fresh_var()
	    val range_tvar = fresh_var()
	    val vklist = [(domain_tvar,Type_k Runtime),
			  (range_tvar,Type_k Runtime)]
	    val funtype = AllArrow_c(Open,Partial,[],[Var_c domain_tvar],0w0,Var_c range_tvar)
	    val typevar = fresh_var()
	    val restypevar = fresh_var()
	    val typearg = Var_c typevar
	    fun make_arm n = 
		let val labels = Listops.map0count generate_tuple_label n
		    val primcon = Record_c labels
		    val rescon = Var_c restypevar
		    val vklist = Listops.map0count (fn _ => (fresh_var(), Word_k Runtime)) n
		    val rectypes = map (Var_c o #1) vklist
		    val argtype = Prim_c(Record_c labels, rectypes)
		    val newfuncon = AllArrow_c(Open,Partial,[],[argtype],0w0,rescon)
		in  (primcon,vklist,newfuncon,Word_k Runtime)
		end
	    val arms = Listops.map0count make_arm i
	    val default = SOME funtype
	    val body = Typecase_c{arg=typearg,arms=arms,default=default}
	in  (vklist,body,Word_k Runtime)
	end


end
