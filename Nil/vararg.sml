functor Vararg(structure Nil : NIL
	       structure NilUtil : NILUTIL
	       sharing NilUtil.Nil = Nil)
    =

struct
    
    open Nil
    open NilUtil
    open Name
    open Util
    val error = fn s => Util.error "vararg.sml" s

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
		in  (primcon,vklist,newfuncon)
		end
	    val arms = Listops.map0count make_arm i
	    val default = funtype
	    val body = Typecase_c{arg=typearg,arms=arms,default=default,kind=Word_k Runtime}
	in  (vklist,body,Word_k Runtime)
	end


    (* The flatarg transformation takes a term and flattens the arguments of functions
     * if they are records.  The opposite action is taken at applications.  Sometimes
     * the type of the argument is not known and we must then use the vararg/onearg
     * primitives.  We want to keep this to a minimum.  At the type level, we must
     * transform each arrow type by applying Vararg to it.  We only consider functions
     * and arrows that take no type argument and having exactly one term argument.
     *)

    datatype recordtype = NOT_RECORD | RECORD of label list * con list | DYNAMIC | NOT_TYPE

    fun is_record argc = 
	    case argc of
		(Prim_c(((Int_c _) | (Float_c _) | (BoxFloat_c _) | 
			 Exn_c | Array_c | Vector_c |
			 Ref_c | Exntag_c | (Sum_c _) |
			 (Vararg_c _)), _)) => NOT_RECORD
	      | (Prim_c(Record_c labels, cons)) => RECORD(labels,cons)
	      | ((Mu_c _) | (AllArrow_c _) | (Annotate_c _)) => NOT_RECORD
	      | (Crecord_c _) => NOT_TYPE
	      | (Closure_c _) => NOT_TYPE
	      | ((Proj_c _) | (App_c _) | (Typecase_c _) | (Let_c _) | (Var_c _)) => DYNAMIC

    fun xarrow(flatcount : int,openness,effect,argc,resc) : con = 
	let val cr = con_rewrite flatcount
	    fun nochange() = AllArrow_c(openness,effect,[],[cr argc],0w0,cr resc)
	    fun change(labels,cons) = 
		if ((length labels) <= flatcount)
		    then AllArrow_c(openness,effect,[],map cr cons,0w0,cr resc)
		else nochange()
	in  (case is_record argc of
		 NOT_RECORD => nochange()
	       | RECORD(ls,cs) => change(ls,cs)
	       | DYNAMIC => Prim_c(Vararg_c(openness,effect),[cr argc,cr resc]) 
	       | NOT_TYPE => error "ill-formed arrow type")
	end

    and con_rewrite' flatcount (bound, arg_con) : con changeopt = 
	case arg_con of
	    AllArrow_c(openness,effect,[],[argc],0w0,resc) => 
		CHANGE_NORECURSE(xarrow(flatcount,openness,effect,argc,resc))
	  | _ => NOCHANGE

    and exp_rewrite' flatcount ({boundevars,...} : bound, arg_exp) : exp changeopt = 
	let val cr = con_rewrite flatcount
	in  case arg_exp of
	    ((Var_e _) | (Const_e _) | (Let_e _) | (Prim_e _) |
	     (Switch_e _) | (Raise_e _) | (Handle_e _)) => NOCHANGE
	   | (App_e (openness,Var_e v,[],[arg],[])) => 
		 let val arg' = exp_rewrite flatcount arg
		     val con = (case (Name.VarMap.find(boundevars,v)) of
				    SOME c => c
				  | NONE => error "ill-typed App_e")
		     val nochange = App_e(openness,Var_e v, [],[arg'],[])
		     fun change(labels,cons) = 
			 if ((length labels) <= flatcount)
			     then 
				 let fun proj l = Prim_e(NilPrimOp(select l),cons,[arg'])
				     val args' = map proj labels
				 in  App_e(openness,Var_e v, [],args',[])
				 end
			 else nochange
		 in  CHANGE_NORECURSE
		     (case con of
			  AllArrow_c(openness,effect,[],[argc],0w0,resc) =>
			      (case (is_record con) of
				   NOT_RECORD => nochange
				 | RECORD(ls,cs) => change(ls,cs)
				 | DYNAMIC => App_e(openness,
						    Prim_e(NilPrimOp(make_vararg(openness,effect)),
							   [cr argc,cr resc],
							   [Var_e v]),
						    [],[arg'],[])
				 | NOT_TYPE => error "ill-formed application")
			| _ => nochange)
		 end
	   | (App_e _) => NOCHANGE
	end

    and con_rewrite (flatcount : int) con : con = NilUtil.con_rewrite (make_handlers flatcount) con
    and exp_rewrite flatcount exp : exp = NilUtil.exp_rewrite (make_handlers flatcount) exp
    and make_handlers flatcount = (exp_rewrite' flatcount, 
				    fn (_,b : bnd) => NOCHANGE,
				    con_rewrite' flatcount,
				    fn (_,cb : conbnd) => NOCHANGE,
				    fn (_,k : kind) => NOCHANGE)
	 
end
