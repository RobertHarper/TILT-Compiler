(* vararg/onearg stuff not implemented until reductions are coded elsewhere *)
functor NilEvaluate (structure Nil : NIL
		     structure NilUtil : NILUTIL
		     structure PrimUtil : PRIMUTIL
		     sharing PrimUtil.Prim = Nil.Prim
		     sharing NilUtil.Nil = Nil
		     sharing type PrimUtil.con = Nil.con
		     sharing type PrimUtil.exp = Nil.exp)
     : NILEVAL = 

  struct

      val error = fn s => Util.error "nileval.sml" s
      open Nil
      structure Nil = Nil


      (* ----------- commonly used functions --------------- *)
      val eq_var = Name.eq_var
      val mapsequence = Util.mapsequence
      val sequence2list = Util.sequence2list
      val list2sequence = Util.list2sequence
      val set2list = Util.set2list
      val list2set = Util.list2set
      val member_eq = Listops.member_eq
      val assoc_eq = Listops.assoc_eq
      val map2 = Listops.map2
      local fun map2fun map v = assoc_eq(eq_var,v,map)
      in    
	  fun substConInCon(c,cmap) : con = NilUtil.substConInCon (map2fun cmap) c 
	  fun substConInExp(e,cmap) : exp = NilUtil.substConInExp (map2fun cmap) e
	  fun substExpInExp(e,emap) : exp = NilUtil.substExpInExp (map2fun emap) e
      end

    
      (* ----------- predicates to check if a expression is a value --------------- *)
      fun nilprim_isval(np,clist,elist) = 
	  (case np of
	       ((record _) | (inject _) | (box_float _) | roll | inj_exn) =>
		   Listops.andfold exp_isval elist
	     | ((make_vararg _) | (make_onearg _) | 
		(select _) | (inject_record _) | (project_sum _) | 
		(project_sum_record _) | (unbox_float _) | unroll | make_exntag) => false)
	   
      and exp_isval (Var_e v) = false
	| exp_isval (Const_e v) = true
	| exp_isval (Let_e (_,[bnd],Var_e v)) = 
	  (case bnd of
	       (Con_b _) => false
	     | (Exp_b _) => false
	     | (Fixopen_b vf_set) => (member_eq(eq_var,v,map #1(set2list vf_set)))
	     | (Fixcode_b vf_set) => (member_eq(eq_var,v,map #1(set2list vf_set)))
	     | (Fixclosure_b vc_set) => (member_eq(eq_var,v,map #1(set2list vc_set))))
	| exp_isval (Let_e _) = false
	| exp_isval (Prim_e (NilPrimOp np,clist,elist)) = nilprim_isval(np,clist,elist)
	| exp_isval (Prim_e (PrimOp _,_,_)) = false
	| exp_isval (Switch_e _) = false
	| exp_isval (App_e _) = false
	| exp_isval (Raise_e _) = false
	| exp_isval (Handle_e _) = false

      fun vklist_isval (vklist : (var * kind) list) : bool = 
	  (Listops.andfold (fn (_,k) => kind_isval k) vklist)

      and vclist_isval (vclist : (var * con) list) : bool = 
	  (Listops.andfold (fn (_,c) => con_isval c) vclist)

      and lclist_isval (lclist : (label * con) list) : bool = 
	  (Listops.andfold (fn (_,c) => con_isval c) lclist)

      and kind_isval (kind : kind) : bool = 
	  case kind of
	      Type_k _ => true
	    | Word_k _ => true
	    | Singleton_k (_,k,c) => kind_isval k andalso con_isval c
	    | Record_k lv_k_seq => Listops.andfold (fn ((_,_),k) => kind_isval k) (sequence2list lv_k_seq)
	    | Arrow_k (_,vklist,k) => kind_isval k andalso vklist_isval vklist

      and primcon_isval pc =
	  (case pc of
	       ((Int_c _) | (Float_c _) | (BoxFloat_c _) |
		Exn_c | Array_c | Vector_c | Ref_c | Exntag_c |
		(Sum_c _) | (Record_c _)) => true
	      | (Vararg_c _) => raise Util.UNIMP)

      and con_isval (con : con) : bool = 
	  let fun confun_isval (_,_,vklist,clist,_,c) = 
	      (con_isval c) andalso (Listops.andfold con_isval clist)
	      andalso (vklist_isval vklist)
	  in  case con of
	      Prim_c (pc,clist) => (primcon_isval pc) andalso (Listops.andfold con_isval clist)
	    | Mu_c (vc_seq,v) => vclist_isval (sequence2list vc_seq)
	    | AllArrow_c confun => confun_isval confun
	    | Var_c v => false
	    | Let_c (_,[Open_cb(v,vklist,c,k)],Var_c v') => eq_var(v,v') 
	    | Let_c (_,[Code_cb(v,vklist,c,k)],Var_c v') => eq_var(v,v') 
	    | Let_c (letsort, cbnds, c) => false
	    | Crecord_c lclist => lclist_isval lclist
	    | Proj_c (c,l) => false
	    | Closure_c (c1,c2) => (con_isval c1) andalso (con_isval c2)
	    | App_c (c, clist) => false
	    | Typecase_c {arg, arms, default} => false
	    | Annotate_c (_,c) => con_isval c
	  end

      (* ------------------ Evaluation ---------------- *)
      exception NotFound
      exception RuntimeExn of exp
      local
	  datatype henv = HENV of exp Name.VarMap.map * con Name.VarMap.map
	  val add = Name.VarMap.insert
	  fun find(map,v) = (case (Name.VarMap.find(map,v)) of
				 (SOME x) => x
			       | NONE => raise NotFound)
      in
	  type env = henv
	  val empty_env = HENV(Name.VarMap.empty, Name.VarMap.empty)
	  fun add_var(HENV(venv,cenv),v,e) = HENV(add(venv,v,e),cenv)
	  fun add_convar(HENV(venv,cenv),v,c) = HENV(venv,add(cenv,v,c))
	  fun find_var (HENV(venv,cenv)) v = find(venv,v)
	  fun find_convar (HENV(venv,cenv)) v = find(cenv,v)
      end

      fun kindEval env (kind : kind) : kind = 
	  let val self = kindEval env
	  in  (case kind of
		   (Word_k _) => kind
		 | (Type_k _) => kind
		 | Singleton_k (p,k,c) => Singleton_k(p,self k, conEval env c)
		 | Record_k lv_k_seq => 
		       let fun doer((l,v),k) = ((l,v),self k)
		       in  Record_k(mapsequence doer lv_k_seq)
		       end
		 | Arrow_k (openness,vklist,k) => 
		       let val vklist' = map (fn (v,k) => (v,self k)) vklist
		       in  Arrow_k(openness,vklist',self k)
		       end)
	  end

      and primconEval (pc, clist) : con = 
	  (case pc of
	      ((Int_c _) | (Float_c _) | (BoxFloat_c _) |
	       Exn_c | Array_c | Vector_c | Ref_c | Exntag_c |
	       (Sum_c _) | (Record_c _)) => Prim_c(pc, clist)
	     | (Vararg_c _) => raise Util.UNIMP)

      and conEval env (con : con) : con = 
	  let val self = conEval env
	      fun confunEval (openness,effect,vklist,clist,numfloats,c) = 
		  let val vklist' = map (fn (v,k) => (v, kindEval env k)) vklist
		  in  (openness,effect,vklist',map self clist, numfloats, self c)
		  end
	  in  case con of
	      Prim_c (pc,clist) => primconEval(pc,map self clist)
	    | Mu_c (vc_seq,v) => let fun doer(v,c) = (v,self c)
				 in  Mu_c(mapsequence doer vc_seq, v)
				 end
	    | AllArrow_c confun => AllArrow_c(confunEval confun)
	    | Var_c v => find_convar env v
	    | Let_c (letsort, cbnds, c) => 
		  let fun do_cbnd(Con_cb(v,k,c),env) = add_convar(env,v,self c)
			| do_cbnd(cbnd as ((Open_cb (v,_,_,_))|(Code_cb (v,_,_,_))),env) = 
			  let val cfun = Let_c(Sequential,[cbnd], Var_c v)
			  in  add_convar(env,v,cfun)
			  end
		      val env' = foldl do_cbnd env cbnds
		  in  conEval env' c
		  end
	    | Crecord_c lclist => let fun doer (l,c) = (l, self c)
				      val lclist' = map doer lclist
				  in Crecord_c lclist'
				  end
	    | Proj_c (c,l) => 
		  (case (self c) of
		       Crecord_c lclist => (case (assoc_eq(Name.eq_label,l,lclist)) of
						SOME c => c
					      | NONE => error "bad Proj_c")
		     | _ => error "bad Proj_c")
	    | Closure_c (c1,c2) => Closure_c(self c1, self c2)
	    | App_c (c, clist) => 
		  let val c' = self c
		      val clist' = map self clist
		  in  (case c' of 
			   Let_c(_,[((Open_cb(v,vklist,body,k)) |
				     (Code_cb(v,vklist,body,k)))],Var_c v') =>
			       if (eq_var(v,v'))
				   then let val conmap = map2 (fn ((v,_),c) => (v,c)) (vklist,clist')
					    val body' = substConInCon(c,conmap)
					in self body'
					end
			       else error "bad App_c"
			 | _ => error "bad App_c")
		  end
	    | Typecase_c {arg, arms, default} => 
		  let val arg' = self arg
		      fun loop [] = (case default of
					 NONE => error "no match in Typecase"
				       | SOME c => self c)
			| loop ((pc,vklist,body,k)::rest) = 
			  let val match = 
			      (case (pc,arg') of
				   (Int_c is, Prim_c(Int_c is',_)) => if (is = is') then SOME [] else NONE
				 | (Float_c fs, Prim_c(Float_c fs',_)) => if (fs = fs') then SOME [] else NONE
				 | (BoxFloat_c fs, Prim_c(BoxFloat_c fs',_)) => if (fs = fs') 
										    then SOME [] else NONE
				 | (Exn_c, Prim_c(Exn_c,[c])) => SOME [c]
				 | (Array_c, Prim_c(Array_c,[c])) => SOME [c]
				 | (Ref_c, Prim_c(Ref_c,[c])) => SOME [c]
				 | (Exntag_c, Prim_c(Exntag_c,[c])) => SOME [c]
				 | (Sum_c {tagcount,known}, Prim_c(Sum_c{tagcount=t,known=k},[c])) => SOME [c]
				 | (Record_c lists, Prim_c(Record_c lists',clist)) => SOME clist
				 | (Vararg_c _, _) => error "vararg not permitted in Typecase")
			  in case match of
			      NONE => loop rest
			    | SOME clist => let val conmap = map2 (fn ((v,_),c) => (v,c)) (vklist,clist)
						val body = substConInCon(body,conmap)
					    in  self body 
					    end
			  end
		  in  loop arms
		  end
	    | Annotate_c (_,c) => self c
	  end

      (* by assumption the arguments to nilprimopEval are values *)
      fun nilprimopEval(np,clist,elist) : exp = 
	  let val default = Prim_e(NilPrimOp np,clist,elist)
	  in (case np of
		  record labels => default
		| select label => 
		      (case elist of
			   [Prim_e(NilPrimOp(record labels), _, elist)] =>
			       let val temp = Listops.zip labels elist
			       in  (case assoc_eq(Name.eq_label,label,temp) of
					SOME e => e
				      | NONE => error "select given bad argument")
			       end
			 | _ => error "select given bad argument")
		| inject {tagcount, field} => default
		| inject_record (info as {tagcount, field}) => 
		      let val reccon = List.nth(clist, TilWord32.toInt field)
			  val (labels,recclist) = 
			      (case reccon of
				   Prim_c(Record_c labels, recclist) => (labels, recclist)
				 | _ => error "inject_record given bad argument")
			  val recexp = Prim_e(NilPrimOp(record labels), recclist, elist)
		      in  Prim_e(NilPrimOp (inject info),clist, [recexp])
		      end
		| project_sum {tagcount, sumtype} => 
		      (case elist of
			   [Prim_e(NilPrimOp(inject info), _, [injected])] => injected
			 | _ => error "project_sum given bad arg")
		| project_sum_record {tagcount,sumtype,field} => 
		      (case elist of
			   [Prim_e(NilPrimOp(inject info), _, [injected])] =>
			       (case injected of
				    Prim_e(NilPrimOp(record _), _, recfields) =>
					List.nth(recfields, TilWord32.toInt field)
				  | _ => error "project_sum_record given bad arg")
			 | _ => error "project_sum_record given bad arg")
		| box_float fs => default
		| unbox_float fs => (case elist of
					 [Prim_e(NilPrimOp (unbox_float _),_,[v])] => v
				       | _ => error "unbox_float given bad argument")
		| roll => default
		| unroll => (case elist of
				 [Prim_e(NilPrimOp unroll,_,[v])] => v
			       | _ => error "unroll given bad argument")
		| make_exntag => (case clist of
				      [c] => Const_e(Prim.tag(Name.fresh_tag(),c))
				    | _ => error "make_exntag given bad constr arguments")
		| inj_exn => default
		| make_vararg (openness,effcet) => raise Util.UNIMP
		| make_onearg (openness,effect) => raise Util.UNIMP
		| peq => error "polyequality primitive not supported by NIL interpreter")
	  end

     
      fun doCall(Let_e(Sequential,[((Fixopen_b vfset) | (Fixcode_b vfset))],Var_e v),clist,elist,eflist) = 
	  let val vflist = set2list vfset
	      val Function(_,_,vklist,vclist,vflist,body,_) = (case (assoc_eq(eq_var,v,vflist)) of
								     SOME f => f
								   | NONE => error "doCall failed")
	      val cmap = map2 (fn ((v,_),c) => (v,c)) (vklist,clist)
	      val eimap = map2 (fn ((v,_),e) => (v,e)) (vclist,elist)
	      val efmap = map2 (fn (v,ef) => (v,ef)) (vflist,eflist)
	      val emap = eimap @ efmap
	      val body' = substConInExp(body,cmap)
	      val body'' = substExpInExp(body',emap)
	  in body''
	  end
	| doCall _ = error "doCall failed"

      fun doApp(_, f as Let_e(_,[((Fixopen_b _) | (Fixcode_b _))],_),
		clist,elist,eflist) = doCall(f,clist,elist,eflist)
	| doApp(env, Let_e(Sequential,[Fixclosure_b vcset],Var_e v),clist,elist,eflist) = 
	  let val vclist = set2list vcset
	      val {code=codevar,cenv,venv,tipe} = (case (assoc_eq(eq_var,v,vclist)) of
						       SOME cl => cl
						     | NONE => error "doApp failed")
	      val code = find_var env codevar
	      val clist' = clist @ [cenv]
	      val elist' = elist @ [venv]
	  in  doCall(code,clist',elist',eflist)
	  end

      fun doSwitch env switch =
	  let fun do_sw {info,arg,arms,default} doarg match = 
	      let val arg' = doarg arg
		  fun loop [] = (case default of
				     NONE => raise (RuntimeExn NilUtil.match_exn)
				   | SOME e => expEval env e)
		    | loop ((t,f)::rest) = 
		      (case (match info arg' t) of
			   NONE => loop rest
			 | SOME (cargs,eargs) => 
			       let val funvar = Name.fresh_var()
				   val fb =Fixopen_b(list2set[(funvar,f)])
				   val f = Let_e(Sequential,[fb],Var_e funvar)
				   val reduced = doCall(f,cargs,eargs,[])
			       in  expEval env reduced
			       end)
	      in  loop arms
	      end
	      fun intmatch _ arg w32 = 
		  (case arg of
		       Const_e (Prim.int(_,w64)) => if (TilWord64.equal(w64,TilWord64.fromUnsignedHalf w32))
							then SOME ([],[])
						    else NONE
		     | _ => error "ill-typed int switch")
	      fun summatch (tagcount,conlist) arg t = 
		  (case arg of
		       Prim_e(NilPrimOp(inject {tagcount,field}), clist, elist) =>
			   if (TilWord32.equal(field,t))
			       then SOME(if TilWord32.ult(t,tagcount)
					     then ([],[])
					 else ([],[arg]))
			   else NONE
		     | _ => error "ill-typed sum switch")
	      fun exnmatch () arg texp = 
		  (case (arg,expEval env texp) of
		       (Prim_e(NilPrimOp(inj_exn),clist,[Const_e(Prim.tag(t',_)),carrier]),
			Const_e(Prim.tag(t,_))) =>
			   if (Name.eq_tag(t,t'))
			       then SOME([],[carrier])
			   else NONE
		     | _ => error "ill-typed int switch")
	      fun typematch () arg pc = 
		  (case (pc,arg) of
		       (Int_c is, Prim_c(Int_c is',_)) => if (is = is') then SOME([],[]) else NONE
		     | (Float_c fs, Prim_c(Float_c fs',_)) => if (fs = fs') then SOME([],[]) else NONE
		     | (BoxFloat_c fs, Prim_c(BoxFloat_c fs',_)) => if (fs = fs') then SOME([],[]) else NONE
		     | (Exn_c, Prim_c(Exn_c,[c])) => SOME([c],[])
		     | (Array_c, Prim_c(Array_c,[c])) => SOME([c],[])
		     | (Ref_c, Prim_c(Ref_c,[c])) => SOME([c],[])
		     | (Exntag_c, Prim_c(Exntag_c,[c])) => SOME([c],[])
		     | (Sum_c {tagcount,known}, Prim_c(Sum_c{tagcount=t,known=k},[c])) => SOME([c],[])
		     | (Record_c lists, Prim_c(Record_c lists',clist)) => SOME(clist,[])
		     | (Vararg_c _, _) => error "vararg not permitted in typecase")

	  in  (case switch of
		   Intsw_e sw => do_sw sw (expEval env) intmatch
		 | Sumsw_e sw => do_sw sw (expEval env) summatch
		 | Exncase_e sw => do_sw sw (expEval env) exnmatch
		 | Typecase_e sw => do_sw sw (conEval env) typematch)
	  end

      and expEval env (exp : exp) : exp = 
	  let val self = expEval env
	  in
	      case exp of
		  (Var_e v) => find_var env v
		| (Const_e v) => exp
		| (Let_e (_,bnds,body)) => 
		      let fun folder (bnd,env) = 
			  case bnd of
			      Con_b(v,_,c) => add_convar(env,v,conEval env c)
			    | Exp_b(v,_,e) => add_var(env,v,expEval env e)
			    | ((Fixopen_b vfset) | (Fixcode_b vfset)) =>
				  let val vflist = set2list vfset
				      fun folder((v,_),env) = add_var(env,v,Let_e(Sequential,[bnd],Var_e v))
				  in  foldl folder env vflist
				  end
			    | Fixclosure_b vcset => 
				  let val vclist = set2list vcset
				      fun folder((v,_),env) = add_var(env,v,Let_e(Sequential,[bnd],Var_e v))
				  in  foldl folder env vclist
				  end
			  val env' = foldl folder env bnds
		      in expEval env' body
		      end
		| (Prim_e (NilPrimOp np,clist,elist)) => 
		      let val clist' = map (conEval env) clist
			  val elist' = map self elist
		      in  nilprimopEval(np,clist',elist')
		      end
		| (Prim_e (PrimOp p,clist,elist)) => 
		      let val clist' = map (conEval env) clist
			  val elist' = map self elist
		      in  PrimUtil.apply p clist' elist'
		      end
		| (Switch_e switch) => doSwitch env switch
		| (App_e (openness,func,clist,elist,eflist)) => 
		      let val func' = self func
			  val clist' = map (conEval env) clist
			  val elist' = map self elist
			  val eflist' = map self eflist
			  val res = doApp(env,func',clist',elist',eflist')
		      in  self res
		      end
		| (Raise_e (e,_)) => raise (RuntimeExn (self e))
		| (Handle_e (e1,Function(_,_,[],[(v,_)],[],body,_))) => 
		      ((self e1) 
		       handle RuntimeExn e => let val env' = add_var(env,v,e)
					      in expEval env' body
					      end)
		| (Handle_e _) => error "ill-formed handle_e"
	  end

      datatype result = VALUE of Nil.exp
	              | EXCEPTION of Nil.exp

      fun eval_exp exp = (VALUE (expEval empty_env exp)
			  handle RuntimeExn e => EXCEPTION e)

  end
