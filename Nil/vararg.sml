(*$import Prelude TopLevel Name Util Listops Sequence List TraceInfo Int TilWord32 NilSubst NilRename VARARG Nil NilContext NilUtil Ppnil Normalize ToClosure Reify Stats TraceOps Linearize *)

(* A note about renaming: there is an optimizer invariant that all
   bound variables must be unique.  Constructor level variables get
   duplicated by reduction.  We could rename (in the sense of
   NilRename) all values returned from the normalizer but this would
   lead to some unnecessary work.  Instead, a constructor is renamed
   only if it comes from the normalizer and its being returned as part
   of the vararg translation (as opposed to merely guiding the
   translation).  *)

structure Vararg :> VARARG = 
struct
    
    open Nil
    open NilUtil
    open Name
    open Util Listops

    val flattenThreshold = !Nil.flattenThreshold

    val debug = Stats.ff("vararg_debug")
    val error = fn s => Util.error "vararg.sml" s


    (* produces term-level functions with the following signature and definition:
     * produces constructor-level functions with the following signature and definition:
     * flattenThreshold is a compile-time constant that is architecturally dependent
     * 
     * vararg : All t::W,t'::W. (All.[t;]->t') -> Vararg(i)(t,t')
     * onearg : All t::W,t'::W. Vararg(i)(t,t') -> (All.[t;]->t')
     * Vararg : [t::W,t'::W] => Vararg(i)(t,t')
     *
     * vararg[c,c'] f = 
     *   typecase c of
     *     record[] => \[;].f (record[])
     *   | ...
     *   | record[c1,...,cn] => \[x1:T(c1),...,xn:T(cn) ; ].          (n = flattenThreshold)
     *                                f (record[x1,...,xn])
     *   | _ => f
     *
     * onearg[c,c'] f = 
     *   typecase c of
     *     record[] => \[x1 : record[];]. f [;]
     *   | ...
     *   | record[c1,...,cn] => \[x: record(T(c1),...,T(cn)) ; ].     (n = flattenThreshold)
     *                                f [x.1, ..., x.n; ]
     *   | _ => f
     *
     * Vararg[c,c'] = 
     *   Typecase c of
     *     record[] => Arrow((); c')
     *   | ...
     *   | record[c1,...,cn] => Arrow((c1, ..., cn); c')
     *   | _ => Arrow(c; c')
     *)

    fun vararg () : function = 
	let
	    val domain_tvar = fresh_named_var "domain"
	    val range_tvar = fresh_named_var "range"
	    val typearg = Var_c domain_tvar
	    val vklist = [(domain_tvar,Type_k),
			  (range_tvar,Type_k)]
	    val funvar = fresh_var()
	    val funtype = AllArrow_c{openness=Open,effect=Partial,isDependent=false,
				     tFormals=[],eFormals=[(NONE,Var_c domain_tvar)],fFormals=0w0,
				     body_type=Var_c range_tvar}
	    val vtrclist = [(funvar,TraceKnown TraceInfo.Trace,funtype)]
	    val newfuntype = Prim_c(Vararg_c(Open,Partial),[Var_c domain_tvar, Var_c range_tvar])
	    fun make_arm n = 
		let val labels = Listops.map0count (fn n => generate_tuple_label(n+1)) n
		    val primcon = Record_c (labels, NONE)
		    val vklist = Listops.map0count (fn _ => (fresh_var(),Type_k)) n


		    (*Don't need to rename the types, since they bind no vars*)
		    val vtrclist = map (fn (v,_) => (fresh_var(), TraceCompute v,
						     Var_c v)) vklist

		    val (vars,trs,types) = Listops.unzip3 vtrclist

		    val rvar = Name.fresh_named_var "vararg_record_arg"

		    val (bnds,rcrd) = NilUtil.mk_record_with_gctag(labels,SOME trs,types,map Var_e vars,SOME rvar)

		    val body = 
		      NilUtil.makeLetE Sequential bnds  (App_e(Open, Var_e funvar,[],[rcrd],[]))

		    val rescon = Var_c range_tvar
		    val funbnd = Function{effect=Partial,recursive=Arbitrary,isDependent=false,
					  tFormals=[],eFormals=vtrclist,fFormals=[],
					  body=body,body_type=rescon}
		    val newfuncon = AllArrow_c{openness=Open,effect=Partial,isDependent=false,
					       tFormals=[],fFormals=0w0,
					       eFormals=map (fn (_,_,c) => (NONE,c)) vtrclist,
					       body_type=rescon}
		    val newfunvar = fresh_named_var ("vararg" ^ (Int.toString n))
		    val bnd = Fixopen_b(Sequence.fromList[(newfunvar,funbnd)])
		    val body = Let_e(Sequential,[bnd],Var_e newfunvar)
		in  (primcon,vklist,body)
		end
	    val arms = Listops.map0count make_arm (flattenThreshold+1)
	    val default = Var_e funvar
	    val body = Switch_e(Typecase_e{result_type=newfuntype,arg=typearg,arms=arms,default=default})
	in  Function{effect=Partial,recursive=Arbitrary,isDependent=false,
		     tFormals=vklist,eFormals=vtrclist,fFormals=[],
		     body=body,
		     body_type=newfuntype}
	end


    fun onearg () : function = 
	let
	    val domain_tvar = fresh_named_var "domain"
	    val range_tvar = fresh_named_var "range"
	    val typearg = Var_c domain_tvar
	    val vklist = [(domain_tvar,Type_k),
			  (range_tvar,Type_k)]
	    val funvar = fresh_var()
	    val funtype = Prim_c(Vararg_c(Open,Partial),[Var_c domain_tvar, Var_c range_tvar])
	    val vtrclist = [(funvar,TraceKnown TraceInfo.Trace, funtype)]
	    val newfuntype = AllArrow_c{openness=Open,effect=Partial,isDependent=false,
					tFormals=[],eFormals=[(NONE,Var_c domain_tvar)],fFormals=0w0,
					body_type=Var_c range_tvar}
	    fun make_arm n = 
		let val labels = Listops.map0count (fn n => generate_tuple_label (n+1)) n
		    val primcon = Record_c (labels, NONE)
		    val vklist = Listops.map0count (fn _ => (fresh_var(), Type_k)) n
		    val argv = fresh_var()
		    val rectypes = map (Var_c o #1) vklist
		    val argtr = TraceKnown TraceInfo.Trace
		    val argtype = Prim_c(Record_c (labels,NONE), rectypes)
		    fun make_proj l = Prim_e(NilPrimOp(select l), [],[], [Var_e argv])
		    val projects = map make_proj labels
		    val body = App_e(Open, Var_e funvar,[],projects,[])
		    val rescon = Var_c range_tvar
		    val funbnd = Function{effect=Partial,recursive=Arbitrary,isDependent=false,
					  tFormals=[],fFormals=[],eFormals=[(argv,argtr,argtype)],
					  body=body,body_type=rescon}
		    val newfuncon = AllArrow_c{openness=Open,effect=Partial,isDependent=false,
					       tFormals=[],eFormals=[(NONE,argtype)],fFormals=0w0,
					       body_type=rescon}
		    val newfunvar = fresh_named_var ("onearg" ^ (Int.toString n))
		    val bnd = Fixopen_b(Sequence.fromList[(newfunvar,funbnd)])
		    val newfun = Let_e(Sequential,[bnd],Var_e newfunvar)
		in  (primcon,vklist,newfun)
		end
	    val arms = Listops.map0count make_arm (flattenThreshold+1)
	    val default = Var_e funvar
	    val body = Switch_e(Typecase_e{result_type=newfuntype,arg=typearg,
					   arms=arms,default=default})
	in  Function{effect=Partial,recursive=Arbitrary,isDependent=false,
		     tFormals=vklist,fFormals=[],eFormals=vtrclist,
		     body=body,
		     body_type=newfuntype}
	end

    fun Vararg () : (var * kind) list * con * kind = 
	let
	    val domain_tvar = fresh_var()
	    val range_tvar = fresh_var()
	    val vklist = [(domain_tvar,Type_k),
			  (range_tvar,Type_k)]
	    val funtype = AllArrow_c{openness=Open,effect=Partial,isDependent=false,
				     tFormals=[],eFormals=[(NONE,Var_c domain_tvar)],fFormals=0w0,
				     body_type=Var_c range_tvar}
	    val typevar = fresh_var()
	    val restypevar = fresh_var()
	    val typearg = Var_c typevar
	    fun make_arm n = 
		let val labels = Listops.map0count (fn n => generate_tuple_label (n+1)) n
		    val primcon = Record_c (labels, NONE)
		    val rescon = Var_c restypevar
		    val vklist = Listops.map0count (fn _ => (fresh_var(), Type_k)) n
		    val rectypes = map (Var_c o #1) vklist
		    val argtype = Prim_c(Record_c (labels,NONE), rectypes)
		    val newfuncon = AllArrow_c{openness=Open,effect=Partial,isDependent=false,
					       tFormals=[],eFormals=[(NONE,argtype)],fFormals=0w0,
					       body_type=rescon}
		in  (primcon,vklist,newfuncon)
		end
	    val arms = Listops.map0count make_arm (flattenThreshold+1)
	    val default = funtype
	    val body = Typecase_c{arg=typearg,arms=arms,default=default,kind=Type_k}
	in  (vklist,body,Type_k)
	end

    local
	val oneargLabel = Name.internal_label "onearg"
	val varargLabel = Name.internal_label "vararg"
	val result = ref NONE
    in  fun generate() = 
	(case !result of
	     SOME res => res
	   | NONE => 
		 let val oneargVar = fresh_named_var "onearg"
		     val varargVar = fresh_named_var "vararg"
		     val nilmod = MODULE{bnds = [Fixopen_b(Sequence.fromList[(oneargVar, onearg())]),
						 Fixopen_b(Sequence.fromList[(varargVar, vararg())])],
					 imports = [],
					 exports = [ExportValue(oneargLabel,oneargVar),
						    ExportValue(varargLabel,varargVar)]}
(*		     val _ = Ppnil.pp_module {module = nilmod,
					      name = "vararg nilmode before CC",
					      header = "vararg nilmode before CC",
					      pass = "vararg nilmode before CC"}
*)
		     val nilmod = Linearize.linearize_mod nilmod
		     val nilmod = Reify.reify_mod nilmod 
		     val nilmod = ToClosure.close_mod nilmod
(*
		     val _ = Ppnil.pp_module {module = nilmod,
					      name = "vararg nilmode after CC",
					      header = "vararg nilmode after CC",
					      pass = "vararg nilmode after CC"}
*)
		     val MODULE{bnds,imports=[],
				exports=[ExportValue(oneargLabel',oneargVar),
					 ExportValue(varargLabel',varargVar)]} = nilmod
		     fun getClosureType v [] = error "could not find closure"
		       | getClosureType v ((Fixclosure_b (_,cl))::rest) = 
			 (case Sequence.toList cl of
			      [(v',{tipe , ...})] => if (eq_var(v,v')) then tipe else getClosureType v rest
			    | _ => getClosureType v rest)
		       | getClosureType v (_::rest) = getClosureType v rest
		     val oneargType = getClosureType oneargVar bnds
		     val varargType = getClosureType varargVar bnds
		     val _ = if (eq_label(oneargLabel,oneargLabel') andalso
				 eq_label(varargLabel,varargLabel'))
				 then ()
			     else error "exports went awry during closure-conversion of vararg"
		     val res = {vararg = (varargLabel, varargVar, TraceKnown TraceInfo.Trace, varargType),
				onearg = (oneargLabel, oneargVar, TraceKnown TraceInfo.Trace, oneargType),
				bnds = bnds}
		     val _ = result := SOME res
		 in  res
		 end)
    end

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
	fun add_label(STATE{ctxt,count},l,v) =
	    STATE{ctxt=NilContext.insert_label(ctxt,l,v), count=count}
	fun get_count(STATE{count,...}) = count
	fun type_of(STATE{ctxt,...},e) = Normalize.type_of(ctxt,e)
	fun reduce (STATE{ctxt,...}) (pred : con -> 'a option) (con : con) = Normalize.reduce_until(ctxt,pred,con)
	fun reduce_hnf (STATE{ctxt,...}) c = Normalize.reduce_hnf(ctxt, c)
    end


    datatype arrowType = Transform of openness * effect * bool * con * con
	               | NoTransform of openness * effect * bool * 
	                                (var * kind) list * w32 * (var option * con) list * con
    fun getarrow(AllArrow_c {openness,effect,isDependent,body_type,tFormals,fFormals,eFormals}) = 
	SOME(case (tFormals,fFormals,eFormals) of
		 ([], 0w0, [(_,argc)]) => Transform(openness,effect,isDependent,argc,body_type)
	       | _ => NoTransform(openness,effect,isDependent,tFormals,fFormals,eFormals,body_type))
      | getarrow(Prim_c(Vararg_c (openness,effect), [argc,resc])) = 
				       SOME(Transform(openness,effect,false,argc,resc))
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
	     Prim_c(Record_c (labs,_),cons) => SOME(RECORD(labs, cons))
	   | (Prim_c _) => SOME NOT_RECORD
	   | (Mu_c _) => SOME NOT_TYPE
	   | (AllArrow_c _) => SOME NOT_RECORD
	   | (Crecord_c _) => SOME NOT_TYPE
	   | (Closure_c _) => SOME NOT_TYPE
	   | Proj_c(Mu_c _, _) => SOME NOT_RECORD
	   | _ => NONE)

    fun is_record state arg =
	(case reduce state getrecord arg of
	     Normalize.REDUCED rt => rt
	   | Normalize.UNREDUCED c => (if (!debug) 
					  then (print "is_record returning DYNAMIC with con = ";
						Ppnil.pp_con c; print "\n")
					else ();
					DYNAMIC))

     (* Transform kinds, cons, and con bindings *)

     fun do_kind (state : state) (kind : kind) : kind = 
	  (case kind of 
	       Type_k => kind
	     | SingleType_k c => SingleType_k(do_con state c)
	     | Single_k c => Single_k(do_con state c)
	     | Record_k(lvk_seq) => let fun folder(((l,v),k),state) = (((l,v),do_kind state k),
								       add_kind(state,v,k))
				    in  Record_k(#1(Sequence.foldl_acc folder state lvk_seq))
				    end
	     | Arrow_k(openness,vklist,k) => let val (vklist,state) = do_vklist state vklist
					     in  Arrow_k(openness,vklist,do_kind state k)
					     end)
	       
      and do_con (state : state) (con : con) : con =
	  (case con of
	       Prim_c(pc,clist) => 
		   let val clist = 
		       (case pc of
			    Record_c(labs,SOME vars) => 
				let val (vclist,_) = do_vclist state (Listops.zip vars clist)
				in  map #2 vclist
				end
	                  | _ => map (do_con state) clist)
		   in  Prim_c(pc, clist)
		   end
	     | Mu_c(recur,vc_seq) => 
		   let val state' = Sequence.foldl
		                    (fn ((v,c),state) => add_kind(state,v,Type_k)) state vc_seq
		   in  Mu_c(recur,Sequence.map
			    (fn (v,c) => (v,do_con state' c)) vc_seq)
		   end
	     | AllArrow_c{openness,effect,isDependent,
			  tFormals=[],fFormals=0w0,eFormals=[(argvopt,argc)],body_type=resc} =>
		   do_arrow state (openness,effect,isDependent,argvopt,argc,resc)
	     | AllArrow_c{openness,effect,isDependent,
			  tFormals,eFormals,fFormals,body_type} =>
		   let val (tFormals,state) = do_vklist state tFormals
		       val (eFormals,state) = do_voptclist state eFormals
		       val body_type = do_con state body_type
		   in  AllArrow_c{openness=openness,effect=effect,isDependent=isDependent,
				  tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
				  body_type=body_type}
		   end
	     | ExternArrow_c (clist,c) => ExternArrow_c(map (do_con state) clist, do_con state c)
	     | Var_c v => con
	     | Crecord_c lclist => Crecord_c(map (fn (l,c) => (l, do_con state c)) lclist)
	     | Proj_c(c,l) => Proj_c(do_con state c, l)
	     | Closure_c(c1,c2) => Closure_c(do_con state c1, do_con state c2)
	     | App_c(c,clist) => App_c(do_con state c, map (do_con state) clist)
	     | Coercion_c {vars,from,to} => 
		   let
		     fun folder (v,s) = add_kind(s,v,Type_k)
		     val state = foldl folder state vars
		   in
		     Coercion_c {vars=vars,from=do_con state from,
				 to=do_con state to}
		   end
	     | Typecase_c _ => error "typecase not handled"
	     | Annotate_c (a,c) => Annotate_c(a,do_con state c)
	     | Typeof_c e => Typeof_c(do_exp state e)
	     | Let_c(letsort,cbnds,c) => 
		   let val (cbnds,state) = foldl_acc do_cbnd state cbnds
		       val c = do_con state c
		   in  Let_c(letsort,cbnds,c)
		   end)

     and do_arrow state (openness,effect,isDependent,argvopt,argc,resc) : con = 
	 let val cnr = do_con state
	     val cr = NilRename.renameCon o cnr
	     val flatcount = get_count state
	     fun nochange() = AllArrow_c{openness=openness,effect=effect,isDependent=isDependent,
					 tFormals=[],fFormals=0w0,
					 eFormals=[(argvopt,cr argc)],
					 body_type = cnr resc}
	     fun change(labels,cons) = 
		 if ((length labels) <= flatcount)
		     then AllArrow_c{openness=openness,effect=effect,isDependent=isDependent,
				     tFormals=[],fFormals=0w0,
				     eFormals = map (fn c => (NONE, cr c)) cons,
				     body_type = cnr resc}
		 else nochange()
	 in  (case (is_record state argc) of
		 NOT_RECORD => nochange()
	       | RECORD(ls,cs) => change(ls,cs)
	       | DYNAMIC => Prim_c(Vararg_c(openness,effect),[cr argc,cnr resc])
	       | NOT_TYPE => error "ill-formed arrow type")
	 end

      and do_cbnd(cbnd : conbnd, state : state) : conbnd * state = 
	  (case cbnd of
	       Con_cb(v,c) => (Con_cb(v,do_con state c), 
			       add_kind(state,v,Single_k c))
	     | Open_cb(v,vklist,c) => let val state' = add_kind(state,v,Arrow_k(Open,vklist,Single_k c))
					  val (vklist,state) = do_vklist state vklist
				      in  (Open_cb(v,vklist,do_con state c), state')
				      end
	     | Code_cb _ => error "code_cb not handled")

      and do_vklist state vklist =
	  let fun folder((v,k),state) = let val k' = do_kind state k
					in  ((v,k'),add_kind(state,v,k))
					end
	  in  foldl_acc folder state vklist
	  end

     and do_vclist state vclist = foldl_acc (fn ((v,c),acc) => let val c' = do_con acc c
							       in  ((v,c'),add_con(acc,v,c))
							       end) state vclist

     and do_vtrclist state vclist = foldl_acc (fn ((v,tr,c),acc) => let val c' = do_con acc c
								 in  ((v,tr,c'),add_con(acc,v,c))
								 end) state vclist

     and do_voptclist state voptclist = foldl_acc (fn ((vopt,c),acc) => 
	                                               let val c' = do_con acc c
							   val acc = (case vopt of
									  SOME v => add_con(acc,v,c)
									| _ => acc)
						       in  ((vopt,c'),acc)
						       end) state voptclist

    (* transform expressions, bindings, switches *)
     and do_exp (state : state) (exp : exp) : exp = 
	   (case exp of
		  Var_e v => exp
		| Const_e _ => exp
		| Prim_e(p,trlist,clist,elist) => Prim_e(p,trlist,
							 map (do_con state) clist, 
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
		| ExternApp_e(e,elist) => ExternApp_e(do_exp state e, map (do_exp state) elist)
		| Raise_e(e,c) => Raise_e(do_exp state e, do_con state c)
		| Handle_e{body,bound,handler,result_type} =>
			let val ([(bound,_)],state') = do_vclist state [(bound,Prim_c(Exn_c,[]))]
			in  Handle_e{bound=bound, body=do_exp state body, 
				     handler = do_exp state' handler,
				     result_type = do_con state result_type}
			end
		| Coerce_e (coercion,cargs,exp) =>
		  Coerce_e (do_exp state coercion, map (do_con state) cargs,
			    do_exp state exp)
		| Fold_e (vars,from,to) =>
		  let
		      fun folder (v,st) = add_kind (st,v,Type_k)
		      val state = foldl folder state vars
		      val from = do_con state from
		      val to = do_con state to
		  in
		      Fold_e (vars,from,to)
		  end
		| Unfold_e (vars,from,to) =>
		  let
		      fun folder (v,st) = add_kind (st,v,Type_k)
		      val state = foldl folder state vars
		      val from = do_con state from
		      val to = do_con state to
		  in
		      Unfold_e (vars,from,to)
		  end)

     and do_fun (state, extras)
	        (funvar,Function{effect,recursive,isDependent,
				 tFormals,fFormals,eFormals,
				 body,body_type},
		 pvar) =
	 let fun folder ((v,e),subst) = let val v' = derived_var v
					in  (Exp_b(v',TraceUnknown,NilRename.renameExp e), 
					     NilSubst.E.addr(subst, v, Var_e v'))
					end
	     val (extraBnds,subst) = foldl_acc folder (NilSubst.E.empty()) extras
	     fun change(v,argc,labels,cons) = 
		 let val body_type = do_con state body_type
		     val innerState = add_con(state,v,argc)
(*		     val argc = do_con state argc *)
		     val vars = map (Name.fresh_named_var o Name.label2name) labels
		     val vtrclist = Listops.map2 (fn (v,c) => (v, TraceUnknown, NilRename.renameCon (do_con state c))) (vars,cons)
		     val (_,trs,cons) = unzip3 vtrclist
		     val (recordBnds,_) = NilUtil.mk_record_with_gctag(labels,
								       SOME trs,
								       map NilRename.renameCon cons,
								       map Var_e vars,
								       SOME v)
		     val body = do_exp innerState body
		     val body = NilSubst.substExpInExp subst body
		     val body = makeLetE Sequential (recordBnds @ extraBnds) body
		 in  (funvar,Function{effect=effect,recursive=recursive,isDependent=isDependent,
				       tFormals=[],fFormals=[],eFormals=vtrclist,
				       body=body,body_type=body_type})
		 end
	     fun default fvar =
		 let val (tFormals,state) = do_vklist state tFormals
		     val (eFormals,state) = do_vtrclist state eFormals
		     val body_type = do_con state body_type
		     val body = do_exp state body
		     val body = NilSubst.substExpInExp subst body
		     val body = makeLetE Sequential extraBnds body
		 in  (fvar,Function{effect=effect,recursive=recursive,isDependent=isDependent,
				       tFormals=tFormals, fFormals=fFormals, eFormals=eFormals,
				       body=body, body_type=body_type})
		 end
		 
	 in  (case (tFormals,fFormals,eFormals) of
		  ([],[],[(v,_,argc)]) =>
		      (case (is_record state argc) of
			   NOT_RECORD => default funvar
			 | RECORD(ls,cs) => if ((length ls) <= get_count state)
						then change(v,argc,ls,cs)
					    else default funvar
			 | DYNAMIC => default pvar
			 | NOT_TYPE => error "ill-formed lambda")
		| _ => default funvar)
	 end
	 
     and getExtra state (var,Function{tFormals=[],fFormals=[],eFormals=[(_,_,argc)],body_type,effect,...},
			 pvar) =
	 (case (is_record state argc) of
	      NOT_RECORD => NONE
	    | RECORD _ => NONE
	    | DYNAMIC => SOME(var, Prim_e(NilPrimOp(make_vararg(Open,effect)),[],
					  [argc,do_con state body_type],[Var_e pvar]))
	    | NOT_TYPE => error "ill-formed lambda type")
       | getExtra _ _ = NONE

     and do_app state (f,arg) =
	 let val cr = NilRename.renameCon o (do_con state)
	     val f' = do_exp state f
	     val arg' = do_exp state arg
	     val con = type_of(state,f)
	     val nochange = App_e(Open,f',[],[arg'],[])
	     fun change(labels) = 
		 if ((length labels) <= flattenThreshold)
		     then 
			 let val v = fresh_named_var "funarg"
			     fun proj l = Prim_e(NilPrimOp(select l),[],[],[Var_e v])
			     val args' = map proj labels
			     val call = App_e(Open,f', [],args',[])
			     val result = Let_e(Sequential,[Exp_b(v,TraceUnknown,arg')],call)
			 in  Linearize.linearize_exp result
			 end
		 else nochange
	     fun dynamic(openness,effect,argc,resc) =  
		 let val v = fresh_named_var "oneargVersion"
		     val result = App_e(openness,
					Prim_e(NilPrimOp(make_onearg(openness,effect)), [],
					       [cr argc, cr resc],
					       [f']),
					[],[arg'],[])
		 in  Linearize.linearize_exp result
		 end
	 in  (case (reduce state getarrow con) of
		  Normalize.REDUCED arrowType =>
		    (case arrowType of
			 Transform(openness,effect,isDependent,argc,resc) =>
			     (case (is_record state argc) of
				  NOT_RECORD => nochange
				| RECORD(ls,_) => change(ls)
				| DYNAMIC => dynamic(openness,effect,argc,resc)
				| NOT_TYPE => error "ill-formed application")
	               | NoTransform _ => 
				  (print "application in which function does not have mono arrow type: \n";
				   Ppnil.pp_con con; print "\n";
				   error "application in which function does not have mono arrow type"))
		| _ => (print "application in which function does not have arrow type: \n";
			Ppnil.pp_con con; print "\n";
			error "application in which function does not have arrow type"))
	 end

     and do_switch (state : state) (switch : switch) : switch = 
	    (case switch of
		 Intsw_e {size,arg,arms,default,result_type} =>
		     let val arg = do_exp state arg
			 val arms = map_second (do_exp state) arms
			 val default = Util.mapopt (do_exp state) default
			 val result_type = do_con state result_type
		     in  Intsw_e {size=size,arg=arg,
				  arms=arms,default=default,
				  result_type=result_type}
		     end
	       | Sumsw_e {sumtype,arg,bound,arms,default,result_type=result_type} =>
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
			 fun do_arm(n,tr,body) = 
			     let val ssumtype = make_ssum n
				 val (_,state) = do_vclist state[(bound,ssumtype)]
			     in  (n,tr,do_exp state body)
			     end
			 val arms = map do_arm arms
			 val default = Util.mapopt (do_exp state) default
		     in  Sumsw_e {sumtype=sumtype,bound=bound,arg=arg,
				  arms=arms,default=default,
				  result_type=result_type}
		     end
	       | Exncase_e {arg,bound,arms,default,result_type} =>
		     let val arg = do_exp state arg
			 val result_type = do_con state result_type
			 fun do_arm(tag,tr,body) = 
			     let val tagcon = type_of(state,tag)
				 val tag = do_exp state tag
				 val (_,Prim_c(Exntag_c,[con])) = reduce_hnf state tagcon
(*
				     con = (case (getexn tagcon) of
						SOME c => c
					      | _ => (print "type of tag is not exntag_c";
						      Ppnil.pp_con tagcon;
						      error "type of tag is not exntag_c"))
*)
				 val (_,state) = do_vclist state [(bound,con)]
			     in  (tag,tr,do_exp state body)
			     end
			 val arms = map do_arm arms
			 val default = Util.mapopt (do_exp state) default
		     in  Exncase_e {bound=bound,arg=arg,
				     arms=arms,default=default,
				     result_type=result_type}
		     end
	       | Typecase_e _ => error "typecase not done")


     and do_bnds(bnds : bnd list, state : state) : bnd list * state = 
	 let val (bnds_list,state) = foldl_acc do_bnd state bnds
	 in  (List.concat bnds_list,state)
	 end

     and do_bnd (bnd : bnd, state : state) : bnd list * state = 
	 let 
	 in  (case bnd of
		  Exp_b(v,niltrace,e) => let val c = type_of(state,e)
					     val e = do_exp state e
					     val state = add_con(state,v,c)
					 in  ([Exp_b(v,niltrace,e)], state)
					 end
		| Con_b(p,cbnd) => let val (cbnd,state) = do_cbnd(cbnd,state)
				   in  ([Con_b(p,cbnd)], state)
				   end
		| Fixopen_b vfset => 
		       let val vflist = Sequence.toList vfset
			   val varFunPvar = map (fn (v,f) => (v,f,derived_var v)) vflist
			   val extras = List.mapPartial (getExtra state) varFunPvar
			   val state = foldl (fn ((v,f,_),s) => add_con(s,v,NilUtil.function_type Open f))
			               state varFunPvar
			   val vflist = map (do_fun (state, extras)) varFunPvar
			   val extraBnds = map (fn (v,e) => (Exp_b(v,TraceUnknown,NilRename.renameExp e))) extras
		       in  ((Fixopen_b(Sequence.fromList vflist))::extraBnds, state)
		       end
		| Fixcode_b vfset => error "fixcode not handled"
		| Fixclosure_b (recur,vclset) => error "fixclosure not handled")
	  end

	fun do_import(ImportValue(l,v,tr,c),state) = (ImportValue(l,v,tr,do_con state c), 
						      add_label(add_con(state,v,c),l,v))
	  | do_import(ImportType(l,v,k),state)  = (ImportType(l,v,do_kind state k), 
						   add_label(add_kind(state,v,k),l,v))

	fun do_export(ExportValue(l,v),state) = (ExportValue(l,v),state)
	  | do_export(ExportType(l,v),state)  = (ExportType(l,v),state)

	fun optimize (MODULE{imports, exports, bnds}) = 
	    let val state = new_state flattenThreshold
		val (imports,state) = foldl_acc do_import state imports
		val (bnds,state) = do_bnds(bnds,state)
		val (exports,state) = foldl_acc do_export state exports
		val result = MODULE{imports=imports,exports=exports,bnds=bnds}
		val _ = if not (!debug) orelse NilRename.isRenamedMod result
			    then ()
			else (Ppnil.pp_module {module=result,
					       name = "",
					       pass = "Vararg",
					       header = "bound variable reuse"};
			      error "bound variable reuse")
	    in  result
	    end

end
