structure NilToWizard = 
  struct
    local
      open Nil
      structure WNil = Curtain.Nil
      structure K = Curtain.K
      structure C = Curtain.C
      structure E = Curtain.E

      val mapopt = Util.mapopt
      val error = Util.error
	
      val map_second = Listops.map_second

      fun xopenness p = 
	(case p
	   of Open => WNil.Open
	    | Code => WNil.Code 
	    | Closure => WNil.Closure)

      fun xeffect e = (case e
			 of Total => WNil.Total
			  | Partial => WNil.Partial)

      fun xrec r = (case r 
		      of Leaf => WNil.Leaf
		       | NonRecursive => WNil.NonRecursive
		       | Arbitrary => WNil.Arbitrary)

      fun xtrace t = (case t
			of TraceUnknown => WNil.TraceUnknown
			 | TraceKnown i => 
			  WNil.TraceKnown (case i
					     of TraceInfo.Trace => WTraceInfo.Trace
					      | TraceInfo.Unset => WTraceInfo.Unset
					      | TraceInfo.Notrace_Int => WTraceInfo.Notrace_Int
					      | TraceInfo.Notrace_Code => WTraceInfo.Notrace_Code
					      | TraceInfo.Notrace_Real => WTraceInfo.Notrace_Real
					      | TraceInfo.Label => WTraceInfo.Label
					      | TraceInfo.Locative => WTraceInfo.Locative
					      | TraceInfo.Compute args => WTraceInfo.Compute (args))
			 | TraceCompute var => WNil.TraceCompute var)

      fun xprim nilprim = 
	(case nilprim
	   of record ls => WNil.record ls
	    | partialRecord args => WNil.partialRecord args
	    | select args => WNil.select args
	    | inject args => WNil.inject args
	    | inject_known args => WNil.inject_known args
	    | inject_known_record args => WNil.inject_known_record args
	    | project args => WNil.project args
	    | project_known args => WNil.project_known args
	    | project_known_record args => WNil.project_known_record args
	    | box_float args => WNil.box_float args
	    | unbox_float args => WNil.unbox_float args
	    | roll => WNil.roll
	    | unroll => WNil.unroll
	    | make_exntag => WNil.make_exntag
	    | inj_exn args => WNil.inj_exn args
	    | make_vararg (p,e) => WNil.make_vararg(xopenness p,xeffect e)
	    | make_onearg (p,e) => WNil.make_onearg(xopenness p,xeffect e)
	    | peq => WNil.peq)

      fun xpcon pcon = 
	(case pcon
	   of Int_c i => WNil.Int_c i
	    | Float_c f => WNil.Float_c f
	    | BoxFloat_c f => WNil.BoxFloat_c f
	    | Exn_c => WNil.Exn_c
	    | Array_c => WNil.Array_c
	    | Vector_c => WNil.Vector_c
	    | Loc_c => WNil.Loc_c
	    | Exntag_c => WNil.Exntag_c
	    | Record_c args => WNil.Record_c args
	    | Sum_c args => WNil.Sum_c args
	    | Vararg_c (p,ef) => WNil.Vararg_c(xopenness p,xeffect ef))

      fun xallprim p = 
	(case p
	   of NilPrimOp nilprim => WNil.NilPrimOp (xprim nilprim)
	    | PrimOp p => WNil.PrimOp p)

      fun xsort letsort = 
	(case letsort 
	   of Sequential => WNil.Sequential
	    | Parallel => WNil.Parallel)
	   
      fun xphase p = 
	(case p
	   of Runtime => WNil.Runtime
	    | Compiletime => WNil.Compiletime)
	   
      fun xconbnd cbnd = 
	(case cbnd 
	   of Con_cb(var, con)      => WNil.Con_cb(var,xcon con)
	    | Open_cb (var,vks,con) => WNil.Open_cb(var,map_second xkind vks,xcon con)
	    | Code_cb (var,vks,con) => WNil.Code_cb(var,map_second xkind vks,xcon con))

      and xfunction (Function{effect, recursive, isDependent,
			      tFormals, eFormals, fFormals,
			      body, body_type}) = 
	WNil.Function({effect = xeffect effect , 
		       recursive = xrec recursive, 
		       isDependent = isDependent,
		       tFormals = Listops.map_second xkind tFormals,
		       eFormals = map (fn (v,t,c) => (v,xtrace t,xcon c)) eFormals,
		       fFormals = fFormals,
		       body = xexp body, 
		       body_type = xcon body_type})

      and xclosure {code:var, cenv:con, venv:exp, tipe:con} = 
	{code = code,cenv = xcon cenv,venv = xexp venv,tipe = xcon tipe}

      and xbnd bnd = 
	(case bnd
	   of Con_b (phase,conbnd)  => WNil.Con_b(xphase phase,xconbnd conbnd)	
	    | Exp_b(v,trace,e)      => WNil.Exp_b(v,xtrace trace,xexp e) 
	    | Fixopen_b vfset       => WNil.Fixopen_b (Sequence.map_second xfunction vfset)
	    | Fixcode_b vfset       => WNil.Fixcode_b (Sequence.map_second xfunction vfset)
	    | Fixclosure_b (r,vc)   => WNil.Fixclosure_b (r,Sequence.map_second xclosure vc))
      and xswitch sw = 
	(case sw 
	   of Intsw_e {arg, size, arms, default, result_type} =>
	     WNil.Intsw_e {arg = xexp arg,size = size,
			   arms = Listops.map_second xexp arms,
			   default = mapopt xexp default,
			   result_type = xcon result_type}
	    | Sumsw_e {arg, sumtype, bound, arms, default, result_type} =>
	     WNil.Sumsw_e {arg = xexp arg, 
			   sumtype = xcon sumtype,
			   bound = bound,
			   arms = map (fn (w,t,e) => (w,xtrace t,xexp e)) arms,
			   default = mapopt xexp default,
		      result_type = xcon result_type}
	    | Exncase_e {arg, bound, arms, default, result_type} =>
	     WNil.Exncase_e {arg = xexp arg, 
			     bound = bound,
			     arms = map (fn (e1,t,e2) => (xexp e1,xtrace t,xexp e2)) arms,
			     default = mapopt xexp default,
			     result_type = xcon result_type}
	    | Typecase_e {arg,arms,default, result_type} => 
	     WNil.Typecase_e {arg = xcon arg, 
			      arms = map (fn (pcon,vks,e) => (xpcon pcon,map_second xkind vks,xexp e)) arms,
			      default = xexp default,
			      result_type = xcon result_type})

      and xkind kind = 
	(case kind 
	   of Type_k              => K.Type_k
	    | SingleType_k con    => K.SingleType_k (xcon con)
	    | Single_k con        => K.Single_k (xcon con)
	    | Record_k fieldseq   => K.Record_k (Sequence.map_second xkind fieldseq)
	    | Arrow_k (p,vks, r)  => K.Arrow_k (xopenness p,Listops.map_second xkind vks,xkind r))
      and xcon con = 
	(case con 
	   of Prim_c (pcon,args) => C.Prim_c(xpcon pcon,map xcon args)
	    | Mu_c (flag,defs)   => C.Mu_c (flag,Sequence.map_second xcon defs)
	   | (AllArrow_c {openness, effect, isDependent, tFormals, 
			  eFormals, fFormals, body_type}) =>
	     (C.AllArrow_c{openness = xopenness openness, effect = xeffect effect, isDependent = isDependent,
			   tFormals = Listops.map_second xkind tFormals, 
			   eFormals = Listops.map_second xcon eFormals, 
			   fFormals = fFormals, body_type = xcon body_type})
			| ExternArrow_c (cons,con) => C.ExternArrow_c (map xcon cons,xcon con)
			| Var_c var                => C.Var_c var
			| Let_c (letsort, cbnds, body) => C.Let_c(xsort letsort,map xconbnd cbnds,xcon body)
			| Typeof_c exp             => C.Typeof_c (xexp exp)
			| Closure_c (code,env)     => C.Closure_c (xcon code,xcon env)
			| Crecord_c entries        => C.Crecord_c (Listops.map_second xcon entries)
			| Proj_c (con,lbl)         => C.Proj_c (xcon con,lbl)
			| App_c (cfun,actuals)     => C.App_c (xcon cfun,map xcon actuals)
			| Typecase_c {arg, arms, default, kind} => 
	     let fun doarm (pc,vks,body) = (xpcon pc,Listops.map_second xkind vks,xcon body)
	     in C.Typecase_c {arg=xcon arg, arms = map doarm arms, 
			      default = xcon default, kind = xkind kind}
	     end
			| (Annotate_c (annot,con)) => xcon con)
      and xexp exp = 
	(case exp 
	   of Var_e v   => E.Var_e v
	    | Const_e v => 
	     E.Const_e (case v of
			  (Prim.int s) => Prim.int s
			| (Prim.uint s) => Prim.uint s
			| (Prim.float s) => Prim.float s
			| (Prim.array (c,array)) => error "xexp" "arrays shouldn't happen"
			| (Prim.vector (c,array)) => 
			    let val array = Array.tabulate (Array.length array,fn i => xexp(Array.sub(array,i)))
			    in Prim.vector(xcon c,array)
			    end
			| Prim.refcell (r as (ref e)) => error "xexp" "refcells shouldn't happen"
			| Prim.tag (t,c) => Prim.tag (t,xcon c))
	    | Let_e (sort,bnds,body)  => E.Let_e(xsort sort,map xbnd bnds,xexp body)
	    | Prim_e (ap,clist,elist) => E.Prim_e (xallprim ap,map xcon clist,map xexp elist)
	    | Switch_e switch         => E.Switch_e (xswitch switch)
	    | App_e (openness,func,clist,elist,eflist) => 
	     E.App_e(xopenness openness,xexp func,map xcon clist,map xexp elist,map xexp eflist)
	    | ExternApp_e (exp,args) => E.ExternApp_e (xexp exp,map xexp args)
	    | Raise_e (e,c) => E.Raise_e(xexp e,xcon c)
	    | Handle_e {body,bound,handler,result_type} =>
	     E.Handle_e {body = xexp body,bound = bound,
			 handler = xexp handler, result_type = xcon result_type})
      fun ximport imp = 
	(case imp
	   of ImportValue (label,var,trace,con) => WNil.ImportValue(label,var,xtrace trace,xcon con)
	    | ImportType  (label,var,kind)      => WNil.ImportType (label,var,xkind kind))

      fun xexport exp = (case exp
			   of ExportValue args => WNil.ExportValue args
			    | ExportType args => WNil.ExportType args)

      fun xmod (MODULE {bnds,imports,exports}) = WNil.MODULE {bnds = map xbnd bnds,
							      imports = map ximport imports,
							      exports = map xexport exports}
    in
      fun nil_to_wizard (m : module) = xmod m
    end
  end