functor Vararg(val number_flatten : int
	       structure NilContext : NILCONTEXT
	       structure Normalize : NORMALIZE
	       structure NilStatic : NILSTATIC
	       structure NilUtil : NILUTIL
	       structure Subst : NILSUBST
	       structure Ppnil : PPNIL
	       sharing type NilContext.context = NilStatic.context = Normalize.context)
    :> VARARG
    =

struct
    
    open Nil
    open NilUtil
    open Name
    open Util Listops

    val debug = Stats.bool("vararg_debug")
    val _ = debug := false
    val error = fn s => Util.error "vararg.sml" s

(* xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

    (* produces term-level functions with the following signature and definition:
     * produces constructor-level functions with the following signature and definition:
     * i is a compile-time constant that is architecturally dependent
     * 
     * vararg(i) : All t::W,t'::W. (All.[t;]->t') -> Vararg(i)(t,t')
     * onearg(i) : All t::W,t'::W. Vararg(i)(t,t') -> (All.[t;]->t')
     * Vararg(i) : [t::W,t'::W] => Vararg(i)(t,t')
     *
     * vararg(i)[t,t'] f = 
     *   typecase t of
     *     record[] => \[;].f (record[])
     *   | record[c1,...,cn] => \[x1:T(c1),...,xn:T(cn) ; ].     (n <= i)
     *                                f (record[x1,...,xn])
     *   | _ => f
     *
     * onearg(i)[t,t'] f = 
     *   typecase t of
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

xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx *)



    local
	datatype state = STATE of {count : int,
				   ctxt : NilContext.context}
    in  type state = state
	fun new_state i = STATE {ctxt = NilContext.empty(),
				 count = i}
	fun add_kind(STATE{ctxt,count},v,k) = 
	    STATE{ctxt=NilContext.insert_kind(ctxt,v,k), count=count}
	fun add_con(STATE{ctxt,count},v,c) = 
	    STATE{ctxt=NilContext.insert_con(ctxt,v,c), count=count}
	fun get_count(STATE{count,...}) = count
	fun type_of(STATE{ctxt,...},e) = Normalize.type_of(ctxt,e)
	fun reduce (STATE{ctxt,...}) (pred : con -> 'a option) (con : con) = Normalize.reduce_until(ctxt,pred,con)

    end


    fun getarrow(AllArrow_c (openness,effect,[],[argc],0w0,resc)) = SOME(openness,effect,argc,resc)
      | getarrow(Prim_c(Vararg_c (openness,effect), [argc,resc])) = SOME(openness,effect,argc,resc)
      | getarrow _ = NONE

    fun getexn (Prim_c(Exntag_c, [c])) = SOME c
      | getexn _ = NONE

    fun getsum (Prim_c(Sum_c {tagcount,totalcount,known}, [c])) = 
	SOME(TilWord32.toInt tagcount, TilWord32.toInt totalcount, 
	     known, c)
      | getsum _ = NONE

    (* The flatarg transformation takes a term and flattens the arguments of functions
     * if they are records.  The opposite action is taken at applications.  Sometimes
     * the type of the argument is not known and we must then use the vararg/onearg
     * primitives.  We want to keep this to a minimum.  At the type level, we must
     * transform each arrow type by applying Vararg to it.  We only consider functions
     * and arrows that take no type argument and having exactly one term argument.
     *)

    datatype recordtype = NOT_RECORD | RECORD of label list * con list | DYNAMIC | NOT_TYPE

    fun getrecord argc =
	(case argc of
	     (Prim_c(Record_c labels, cons)) => SOME(RECORD(labels,cons))
	   | (Prim_c _) => SOME NOT_RECORD
	   | (Mu_c _) => SOME NOT_RECORD
	   | (AllArrow_c _) => SOME NOT_RECORD
	   | (Crecord_c _) => SOME NOT_TYPE
	   | (Closure_c _) => SOME NOT_TYPE
	   | Proj_c(Mu_c _, _) => SOME NOT_RECORD
	   | _ => NONE)

    fun is_record state arg =
	(case reduce state getrecord arg of
	     Normalize.REDUCED rt => rt
	   | Normalize.UNREDUCED c => (if (!debug) 
					  then (print "is_record returning DYNAMIC with con = \n";
						Ppnil.pp_con c; print "\n")
					else ();
					DYNAMIC))

     (* Transform kinds, cons, and con bindings *)

     fun do_kind (state : state) (kind : kind) : kind = 
	  (case kind of 
	       Type_k => kind
	     | Singleton_k c => Singleton_k(do_con state c)
	     | Record_k(lvk_seq) => let fun folder(((l,v),k),state) = (((l,v),do_kind state k),
								       add_kind(state,v,k))
				    in  Record_k(#1(Sequence.foldl_acc folder state lvk_seq))
				    end
	     | Arrow_k(openness,vklist,k) => let val (vklist,state) = do_vklist state vklist
					     in  Arrow_k(openness,vklist,do_kind state k)
					     end)
	       
      and do_con (state : state) (con : con) : con =
	  (case con of
	       Prim_c(pc,clist) => Prim_c(pc, map (do_con state) clist)
	     | Mu_c(recur,vc_seq) => Mu_c(recur,Sequence.map
					  (fn (v,c) => (v,do_con state c)) vc_seq)
	     | AllArrow_c(openness,effect,[],[argc],0w0,resc) =>
		   do_arrow state (openness,effect,argc,resc)
	     | AllArrow_c(openness,effect,vklist,vclist,numfloats,c) =>
		   let val (vklist,state) = do_vklist state vklist
		   in  AllArrow_c(openness,effect,vklist,
				  map (do_con state) vclist, numfloats, do_con state c)
		   end
	     | Var_c v => con
	     | Crecord_c lclist => Crecord_c(map (fn (l,c) => (l, do_con state c)) lclist)
	     | Proj_c(c,l) => Proj_c(do_con state c, l)
	     | Closure_c(c1,c2) => Closure_c(do_con state c1, do_con state c2)
	     | App_c(c,clist) => App_c(do_con state c, map (do_con state) clist)
	     | Typecase_c _ => error "typecase not handled"
	     | Annotate_c (a,c) => Annotate_c(a,do_con state c)
	     | Let_c(letsort,cbnds,c) => 
		   let val (cbnds,state) = foldl_acc do_cbnd state cbnds
		       val c = do_con state c
		   in  Let_c(letsort,cbnds,c)
		   end)

     and do_arrow state (openness,effect,argc,resc) : con = 
	 let val cr = do_con state
	     val flatcount = get_count state
	     fun nochange() = AllArrow_c(openness,effect,[],[cr argc],0w0,cr resc)
	     fun change(labels,cons) = 
		 if ((length labels) <= flatcount)
		     then AllArrow_c(openness,effect,[],map cr cons,0w0,cr resc)
		 else nochange()
	 in  (case is_record state argc of
		 NOT_RECORD => nochange()
	       | RECORD(ls,cs) => change(ls,cs)
	       | DYNAMIC => Prim_c(Vararg_c(openness,effect),[cr argc,cr resc]) 
	       | NOT_TYPE => error "ill-formed arrow type")
	 end

      and do_cbnd(cbnd : conbnd, state : state) : conbnd * state = 
	  (case cbnd of
	       Con_cb(v,c) => (Con_cb(v,do_con state c), 
			       add_kind(state,v,Singleton_k c))
	     | Open_cb(v,vklist,c,k) => let val state' = add_kind(state,v,Arrow_k(Open,vklist,k))
					    val (vklist,state) = do_vklist state vklist
					in  (Open_cb(v,vklist,
						     do_con state c, do_kind state k), state')
					end
	     | Code_cb(v,vklist,c,k) => error "cpde_cb not handled")

      and do_vklist state vklist =
	  let fun folder((v,k),state) = let val k' = do_kind state k
					in  ((v,k'),add_kind(state,v,k))
					end
	  in  foldl_acc folder state vklist
	  end

     and do_vclist state vclist = foldl_acc (fn ((v,c),acc) => let val c' = do_con state c
							       in  ((v,c'),add_con(acc,v,c))
							       end) state vclist
    (* transform expressions, bindings, switches *)
     fun do_exp (state : state) (exp : exp) : exp = 
	   (case exp of
		  Var_e v => exp
		| Const_e _ => exp
		| Prim_e(p,clist,elist) => Prim_e(p,map (do_con state) clist, 
						  map (do_exp state) elist)
		| Switch_e sw => Switch_e(do_switch state sw)
		| Let_e (letsort,bnds,e) => 
			let val (bnds,state) = do_bnds(bnds,state)
			    val e = do_exp state e
		        in  Let_e(letsort,bnds,e)
			end
		| App_e(Open,f,[],[e],[]) => do_app state (f,e)
		| App_e(openness,f,clist,elist,eflist) => 
			App_e(openness, do_exp state f, map (do_con state) clist, 
			      map (do_exp state) elist, map (do_exp state) eflist)
		| Raise_e(e,c) => Raise_e(do_exp state e, do_con state c)
		| Handle_e(e,v,handler,c) => 
			let val ([(v,_)],state) = do_vclist state [(v,Prim_c(Exn_c,[]))]
			in  Handle_e(do_exp state e, v, do_exp state handler, do_con state c)
			end)

     and do_fun_recur state (funvar,Function(effect,recur,vklist,vclist,vflist,body,resc)) = 
	 let val (vklist,state) = do_vklist state vklist
	     val resc = do_con state resc
	     val (vclist,state) = do_vclist state vclist
	     val body = do_exp state body
	 in  [(funvar,Function(effect,recur,vklist,vclist,vflist,body,resc))]
	 end

     and do_fun state (funvar,f as Function(effect,recur,[],[(v,argc)],[],body,resc)) = 
	 let val argc = do_con state argc
	     val resc = do_con state resc
	     fun change(labels,cons) = 
		 if ((length labels) <= get_count state)
		     then 
			 let val vars = map (Name.fresh_named_var o Name.label2name) labels
			     val vclist = zip vars (map (do_con state) cons)
			     val state = add_con(state,v,argc)
			     val bnd = Exp_b(v,Prim_e(NilPrimOp(record labels),[],map Var_e vars))
			     val body = Let_e(Sequential,[bnd],do_exp state body)
			 in  [(funvar,Function(effect,recur,[],vclist,[],body,resc))]
			 end
		 else do_fun_recur state (funvar,f)
	 in  (case (is_record state argc) of
		  NOT_RECORD => (if (!debug) then (Ppnil.pp_var funvar; print " NOT_RECORD LAMBDA\n") else ();
				 do_fun_recur state (funvar,f))
		| RECORD(ls,cs) => (if (!debug) then (Ppnil.pp_var funvar; print " RECORD LAMBDA\n") else ();
				    change(ls,cs))
		| DYNAMIC => 
		      let val _ = if (!debug) then (Ppnil.pp_var funvar; print " DYNAMIC LAMBDA\n") else ()
			  val copy = Name.derived_var funvar
			  val [(_,f)] = do_fun_recur state (funvar,f)
			  val vf = (copy,f)
			  val fexp = Prim_e(NilPrimOp(make_vararg(Open,effect)),
					    [argc,resc],
					    [Var_e copy])
			  val f2 = Function(effect,recur,[],[(v,argc)],[],
						App_e(Open,fexp,[],[Var_e v],[]),resc)
			  val vf2 = (funvar,f2)
		      in  [vf, vf2]
		      end
		| NOT_TYPE => error "ill-formed lambda")
	 end
       | do_fun state arg = do_fun_recur state arg
	 

     and do_app state (f,arg) =
	 let val f' = do_exp state f
	     val arg' = do_exp state arg
	     val con = type_of(state,f)
	     val nochange = App_e(Open,f', [],[arg'],[])
	     fun change(labels,cons) = 
		 if ((length labels) <= number_flatten)
		     then 
			 let val v = fresh_named_var "funarg"
			     fun proj l = Prim_e(NilPrimOp(select l),cons,[Var_e v])
			     val args' = map proj labels
			 in  Let_e(Sequential,[Exp_b(v,arg')],App_e(Open,f', [],args',[]))
			 end
		 else nochange
	     fun dynamic(openness,effect,argc,resc) =  
		 App_e(openness,
		       Prim_e(NilPrimOp(make_onearg(openness,effect)),
			      [do_con state argc,do_con state resc],
			      [f']),
		       [],[arg'],[])
	 in  (case (reduce state getarrow con) of
		  Normalize.REDUCED(openness,effect,argc,resc) =>
		      (case (is_record state argc) of
			   NOT_RECORD => nochange
			 | RECORD(ls,cs) => change(ls,cs)
			 | DYNAMIC => dynamic(openness,effect,argc,resc)
			 | NOT_TYPE => error "ill-formed application")
		| _ => (print "application in which function does not have arrow type: \n";
			   Ppnil.pp_con con; print "\n";
			   error "application in which function does not have arrow type"))
	 end

     and do_switch (state : state) (switch : switch) : switch = 
	    (case switch of
		 Intsw_e {size,arg,result_type,arms,default} =>
		     let val arg = do_exp state arg
			 val result_type = do_con state result_type
			 val arms = map_second (do_exp state) arms
			 val default = Util.mapopt (do_exp state) default
		     in  Intsw_e {size=size,arg=arg,result_type=result_type,
				  arms=arms,default=default}
		     end
	       | Sumsw_e {sumtype,arg,result_type,bound,arms,default} =>
		     let val arg = do_exp state arg
			 val result_type = do_con state result_type
			 val (tagcount,totalcount,_,carrier) = 
			     (case reduce state getsum sumtype of
				  Normalize.REDUCED quad => quad
				| Normalize.UNREDUCED _ => error "sumcon of sumsw_e not reducible to sum")
			 val sumtype = do_con state sumtype
			 fun make_ssum i = Prim_c(Sum_c{tagcount=TilWord32.fromInt tagcount,
							totalcount=TilWord32.fromInt totalcount,
							known=SOME i},[carrier])
			 fun do_arm(n,body) = 
			     let val ssumtype = make_ssum n
				 val (_,state) = do_vclist state[(bound,ssumtype)]
			     in  (n,do_exp state body)
			     end
			 val arms = map do_arm arms
			 val default = Util.mapopt (do_exp state) default
		     in  Sumsw_e {sumtype=sumtype,bound=bound,arg=arg,result_type=result_type,
				  arms=arms,default=default}
		     end
	       | Exncase_e {arg,result_type,bound,arms,default} =>
		     let val arg = do_exp state arg
			 val result_type = do_con state result_type
			 fun do_arm(tag,body) = 
			     let val tagcon = type_of(state,tag)
				 val tag = do_exp state tag
				 val con = (case (getexn tagcon) of
						SOME c => c
					      | _ => error "type of tag is not exntag_c")
				 val (_,state) = do_vclist state [(bound,con)]
			     in  (tag,do_exp state body)
			     end
			 val arms = map do_arm arms
			 val default = Util.mapopt (do_exp state) default
		     in  Exncase_e {bound=bound,arg=arg,result_type=result_type,
				     arms=arms,default=default}
		     end
	       | Typecase_e _ => error "typecase not done")


     and do_bnds(bnds : bnd list, state : state) : bnd list * state = 
	 let val (bnds_list,state) = foldl_acc do_bnd state bnds
	 in  (List.concat bnds_list,state)
	 end

     and do_bnd (bnd : bnd, state : state) : bnd list * state = 
	 let 
	 in  (case bnd of
		  Exp_b(v,e) => let val c = type_of(state,e)
				    val e = do_exp state e
				    val state = add_con(state,v,c)
				in  ([Exp_b(v,e)], state)
				end
		| Con_b(p,cbnd) => let val (cbnd,state) = do_cbnd(cbnd,state)
				   in  ([Con_b(p,cbnd)], state)
				   end
		| Fixopen_b vfset => 
		       let val vflist = Sequence.toList vfset
			   val state = foldl (fn ((v,f),s) => add_con(s,v,NilUtil.function_type f)) state vflist
			   val vflist_list = map (do_fun state) vflist
			   val vflist = List.concat vflist_list
		       in  ([Fixopen_b(Sequence.fromList vflist)], state)
		       end
		| Fixcode_b vfset => error "fixcode not handled"
		| Fixclosure_b (recur,vclset) => error "fixclosure not handled")
	  end

	fun do_import(ImportValue(l,v,c),state) = (ImportValue(l,v,do_con state c), 
						   add_con(state,v,c))
	  | do_import(ImportType(l,v,k),state)  = (ImportType(l,v,do_kind state k), 
						   add_kind(state,v,k))

	fun do_export(ExportValue(l,e,c),state) = (ExportValue(l,do_exp state e,do_con state c), 
						   state)
	  | do_export(ExportType(l,c,k),state)  = (ExportType(l,do_con state c,do_kind state k), 
						   state)

	fun optimize (MODULE{imports, exports, bnds}) = 
	    let val state = new_state number_flatten
		val (imports,state) = foldl_acc do_import state imports
		val (bnds,state) = do_bnds(bnds,state)
		val (exports,state) = foldl_acc do_export state exports
	    in  MODULE{imports=imports,exports=exports,bnds=bnds}
	    end

end
