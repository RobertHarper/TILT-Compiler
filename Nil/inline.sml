(*$import Nil Listops Name NilUtil NilRewrite NilSubst INLINE *)

(* Inline functions that are non-recursive and either are called once
   or else are sufficiently small and called a sufficiently small number
   of times.

   We perform the translation in two passes -- an analysis pass and a 
   transformation pass.

   The analysis pass calculates and enters into a hash-table a mapping from 
     function variables to the following information:
        (1) size - the size of the function
	(2) num_rec_call - the number of applications
	      of the function variable inside the function's
	      definition (or those recursively defined)
	(2) num_rec_occur - the number of occurrences (in any position)
	      of the function variable inside the function's
	      definition (or those mutually defined)
        (3) num_nonrec_call - the number of calls to the function 
	      from outside the function's definition
	      (or those mutually defined)
        (4) num_nonrec_occur - the number of times the function 
	      occurs (in any position) from outside the function's
	      definition (or those mutually defined)

   Note that num_rec_call <= num_rec_occur and num_nonrec_call <= num_nonrec_occur.

   A function should be inlined if the following are true
        (1) num_nonrec_occur = num_nonrec_call = 1  (* Used once *)
	    AND num_rec_occur = 0                   (* Not at all recursive *)
	    (definition should be removed)
	(2) num_rec_call = 0 AND                    (* Not call-recursive *)
	    num_nonrec_call * size <= threshold     (* Inlining won't cause excessive blowup *)
	    (definition should be retained)

   The transformation pass inlines and removes definitions according to the
   rules above.  Care must be taken when substituting the definition of the
   function in order to avoid variable capture.  Hence, if we are substituting
   {e1/x1,...,en/xn} and hit a binding-occurrence of a variable, then we must
   alpha-convert the variable if it occurs within the free variables of e1,...,en.
   We just alpha-convert everything to be on the safe side.

   Also, when inlining a function more than one time, we need to rename
   its bound term variables to maintain the invariant that term variables
   are unique.
*)
structure Inline :> INLINE =
struct
  open Nil Name NilUtil

  val size_threshold = ref 150
  val occur_threshold = ref 20
  val error = fn s => Util.error "inline.sml" s
  val debug = ref false
  fun debugpr s = if (!debug) then print s else ()
  fun inc(r:int ref) = r := (!r) + 1

  datatype inlineStatus = NoInline | InlineOnce | InlineMany
  type fun_info = 
    {size: int ref,  
     num_rec_calls : int ref, num_rec_occurs : int ref,
     num_nonrec_calls: int ref, num_nonrec_occurs : int ref, 
     definition: function option ref,
     status : inlineStatus ref }

  fun new_info() = 
	 {size = ref 0, 
	  num_rec_calls = ref 0, num_rec_occurs = ref 0,
	  num_nonrec_calls = ref 0, num_nonrec_occurs = ref 0,
	  definition = ref NONE, status = ref NoInline
	  }

  fun analyze_info inline_count ({size,num_nonrec_calls,num_nonrec_occurs,
				  num_rec_calls,num_rec_occurs,
				  definition,status,...} : fun_info) =
      let val usedOnce = (!num_nonrec_calls = 1) andalso (!num_nonrec_occurs = 1)
	  val recursive = !num_rec_occurs > 0
	  val callRecursive = !num_rec_calls > 0
	  val small = (!size) <= (!size_threshold) andalso (!num_nonrec_calls) <= (!occur_threshold)
      in  if (usedOnce andalso not recursive) 
	      then (status := InlineOnce; inc inline_count)
	  else if (not callRecursive andalso small)
		   then (status := InlineMany; inc inline_count)
	       else (status := NoInline; definition := NONE)
      end


  fun print_info(v,{size,num_nonrec_calls,num_rec_calls,num_rec_occurs,num_nonrec_occurs,definition,
		    status}:fun_info) = 
      let val vs = Name.var2string v
      in
	  print vs; print ": size="; print(Int.toString (!size));
	  print ", num_nonrec_calls=";
	  print(Int.toString(!num_nonrec_calls));
	  print ", num_rec_calls="; print(Int.toString(!num_rec_calls));
	  print ", num_rec_occurs="; print(Int.toString(!num_rec_occurs));
	  print ", num_nonrec_occurs="; print(Int.toString(!num_nonrec_occurs));
	  print (case (!definition) of NONE => " not " | _ => " ");
	  print "inlining function\n"
      end

  type varset = Name.VarSet.set
  exception MissingVar

  fun rename_func (f:function) = 
      let val (b,_) = 
	  NilRename.renameBnd (Fixopen_b (Sequence.fromList
					  [(Name.fresh_var(),f)]))
      in
	  case b of
	    Fixopen_b s =>
		(case Sequence.toList s of
		     [(_,f')] => f'
		   | _ => error "rename_func1")
	  | _ => error "rename_func2"
      end

  fun analyze (m:module) = 
  let (* hash table records information for each potentially inlinable
function *)
      val table : (var,fun_info) HashTable.hash_table = 
	  Name.mk_var_hash_table(31,MissingVar)
      (* will hold # of functions to be inlined *)
      val inline_count = ref 0
      val find_fun = HashTable.find table
      fun insert_fun(v) = let val i = new_info() in HashTable.insert table
(v,i); i end
      fun print_table() = HashTable.appi print_info table
      fun update_table() = HashTable.app (analyze_info inline_count) table
      (* analyze kinds *)
      fun alist (f:'a->int) (l:'a list) : int = List.foldl (fn (x,s) => s +
(f x)) 0 l
      fun alist2 f l = List.foldl (fn ((_,x),s) => s + (f x)) 0 l
      fun asequence f s = Sequence.foldl (fn ((_,x),s) => s + (f x)) 0 s
      fun akind (kind:kind):int = 
	(case kind of
	   Type_k => 1
         | SingleType_k c => 1 + (acon c)
         | Single_k c => 1 + (acon c)
	 | Record_k lvks => 1 + (asequence akind lvks)
	 | Arrow_k (ot,vks,k) => 1 + (akind k) + (avks vks)
	)
      (* analyze a construtor *)
      and avks (vks:(var * kind) list):int = alist2 akind vks
      and acon (con:con):int = 
	1 + 
	(case con of
	   Prim_c(pc,cs) => acons cs
         | Mu_c(b,vcs) => asequence acon vcs
         | AllArrow_c{tFormals=vks,eFormals=cs,body_type=c,...} =>
	   let val s1 = avks vks
	       val s2 = acons (map #2 cs)
	       val s3 = acon c
	   in s1 + s2 + s3
	   end
         | ExternArrow_c(cs,c) => acons (c::cs)
         | Var_c v => 0
         | Let_c(ls,cbl,c) => (alist aconbnd cbl) + (acon c)
         | Typeof_c e => aexp e
         | Crecord_c lcs => alist2 acon lcs
         | Proj_c(c,l) => acon c
         | Closure_c(c1,c2) => acons [c1,c2]
         | App_c(c,cs) => acons (c::cs)
         | Typecase_c{arg,arms,default,kind} =>
	   let val s1 = (acon arg) + (akind kind)
               val s2 = (acon default)
	   in
	       s1 + s2 + (alist (fn (pc,vks,c) => (avks vks) + (acon c))
arms)
	   end
         | Annotate_c (ka,c) => acon c
        )
      (* analyze a constructor binding *)
      and aconbnd (conbnd:conbnd):int = 
	1 + (case conbnd of
		 Con_cb(v,c) => acon c
	       | Open_cb arrowcon => aarrowcon arrowcon
	       | Code_cb arrowcon => aarrowcon arrowcon)
      (* analyze a dependent arrow-like constructor *)
      and aarrowcon (v:var,vks:(var * kind) list,c:con):int = 
	  (avks vks) + (acon c) 
      (* analyze a list of constructors *)
      and acons (cons:con list) : int = alist acon cons
      (* analyze an expression *)
      and aexp (exp:exp) : int = 
	1 + 
        (case exp of
           Var_e v =>
	       ((case find_fun v of
		     NONE => ()
		   | SOME ({num_rec_occurs,num_nonrec_occurs,definition,...}) => 
			 (case !definition of
			      NONE => inc num_nonrec_occurs
			    | SOME _ => inc num_rec_occurs)); 0)
	 | Const_e pv =>
	   let fun aarray(c,ea) = Array.foldr (fn (e,s) => (s + (aexp e)))
(acon c) ea
           in
	      case pv of
		 Prim.array cea => aarray(cea)
	       | Prim.vector cea => aarray(cea)
               | Prim.refcell(er) => aexp (!er)
               | Prim.tag(t,c) => acon c
               | _ => 0
           end
         | Let_e(ls,bnds,e) => (alist abnd bnds) + (aexp e)
         | Prim_e(ap,cs,es) => (acons cs) + (aexps es)
         | Switch_e sw => aswitch sw
         | App_e(ot,e,cs,es1,es2) => 
	       let val s = (acons cs) + (aexps ((e::es1) @ es2))
	       in
		   (case (ot,e) of
			(Open,Var_e v) =>
			    (case find_fun v of
				 NONE => ()
			       | SOME({num_nonrec_calls,num_rec_calls,definition,...}) => 
				     (case !definition of
					  NONE => inc num_rec_calls
					| SOME _ => inc num_nonrec_calls))
		      | _ => ()); s
	       end
         | ExternApp_e(e,es) => aexps (e::es)
         | Raise_e(e,c) => (aexp e) + (acon c)
         | Handle_e{body,bound,handler,result_type} => 
	       (aexps [body,handler]) + (acon result_type)
       )
      (* analyze a list of expressions *)
      and aexps (es:exp list) : int = alist aexp es
      and avcs vcs = acons (map #3 vcs)
      and abnd (bnd: bnd) : int = 
        1 + 
	(case bnd of
	   Con_b(p,cb) => aconbnd cb
         | Exp_b(v,nt,e) => aexp e
         | Fixopen_b vfs => 
	     let val vf_list = Sequence.toList vfs
	     in 
		 case vf_list of
		     [(v,f as (Function{tFormals=vks,eFormals=vcs,fFormals=vs,
					body=e,body_type=c,...}))] =>
			 let val info = insert_fun(v)
			     val s = (avks vks) + (avcs vcs) + (List.length
vs) + 
			             (aexp e) + (acon c)
			 in 
			     (#size info) := s; 
			     (#definition info) := (SOME f);
			     s
			 end
		   | _ => alist2 afunction vf_list
	     end
         | Fixcode_b vfs => asequence afunction vfs
         | Fixclosure_b (_,vfs) => 
	       (asequence (fn ({code,cenv,venv,tipe}) => 
			   (acon cenv) + (aexp venv) + (acon tipe)) vfs)
        )
     and aexpopt (expopt) = (case expopt of NONE => 1 | SOME e => 1 + (aexp
e))
     and aswitch switch = 
        (case switch of
           Intsw_e{arg,size,arms,default,result_type} =>
	     List.foldl (fn ((_,e),s) => s + (aexp e)) 
	     ((aexp arg) + (aexpopt default)) arms
         | Sumsw_e{arg,(*unroll=u,*)sumtype,bound,arms,default,result_type} =>
	     List.foldl (fn ((_,tr,e),s) => 
			 s + (aexp e)) ((aexp arg) + (aexpopt default) +
					(acon sumtype)) arms
         | Exncase_e{arg,bound,arms,default,result_type} =>
	     List.foldl (fn ((e1,_,e2),s) => 
			 s + (aexp e1) + (aexp e2)) 
	     ((aexp arg) + (aexpopt default)) arms
         | Typecase_e{arg,arms,default,result_type} => 
	     List.foldl (fn ((vks,e),s) => s + (aexp e) + (avks vks))
	     ((acon arg) + (aexpopt default)) arms
       )
      and afunction (Function {tFormals=vks,eFormals=vcs,fFormals=vs,body=e,body_type=c,...}) = 
	  (avks vks) + (avcs vcs) + (List.length vs) + (aexp e) + (acon c)
      and aexport_entry (ExportValue (_,v)) = (aexp (Var_e v); ())
	| aexport_entry (_) = ()
      and amodule (MODULE {bnds, exports, ...}) = 
	  (alist abnd bnds; List.app aexport_entry exports)
  in
      (* print "analyzing module\n"; *)
      amodule m;
      update_table();
      if (!debug) then
         (print "function information\n"; 
          print_table())
        else ();
      (!inline_count,find_fun)
  end

  fun optimize m = 
  let val clicks = ref 0
      fun click() = clicks := (!clicks) + 1
      val (inline_count,find_fun) = analyze m
      fun revappend([],x) = x
	| revappend(hd::tl,x) = revappend(tl,hd::x)
      fun ropt r opt = (case opt of NONE => NONE | SOME x => SOME(r x))
      fun rbnds (bnds,revbnds) = 
        (case bnds of
	   [] => List.rev revbnds
	 | (b::bnds) => 
	     let val bs = rbnd b
	     in rbnds(bnds,revappend(bs,revbnds))
	     end)
      and make_handlers unit =
	(fn (b,e) =>
	 let val newexp = rexp e
	 in NilUtil.CHANGE_NORECURSE newexp
	 end,
	 fn (b, bnd) => 
	 error "Shouldn't get here",
	 fn (b, c) => NilUtil.NOCHANGE,
	 fn (b, cb) => NilUtil.NOCHANGE,
	 fn (b, k) => NilUtil.NOCHANGE)
      and rcon con = NilUtil.con_rewrite (make_handlers ()) con
      and rcbnd conbnd = NilUtil.cbnd_rewrite (make_handlers ()) conbnd

      and rexp e = 
	  (case e of
	     Switch_e sw => Switch_e(rswitch sw)
	   | Let_e(ls,bnds,e) => Let_e(ls,rbnds(bnds,[]),rexp e)
	   | Handle_e {body,bound,handler,result_type} => 
	       Handle_e{body = rexp body, bound = bound, 
			handler = rexp handler, result_type = rcon result_type}
	   (* -- *)
	   | Prim_e (p, cons, exps) => 
	       Prim_e (p, map rcon cons, map rexp exps)
	   | App_e (Open,Var_e f,cs,es1,es2) =>
	       		 (case find_fun f of
		    SOME{definition=ref(SOME(func)),status,...} =>
		      let val shouldRename = (case !status of
						  InlineOnce => false
						| InlineMany => true
						| _ => error "cannot have NoInline here")
			  val Function{tFormals=vks,eFormals=vcs,fFormals=vs,body=e,...} = 
			      if shouldRename then rename_func func else func
			  val bnd1 = 
			      Listops.map2 (fn ((v,k),c) =>
					    Con_b(Runtime,Con_cb(v,c)))(vks,cs)
			  val bnd2 = 
			      Listops.map2 (fn ((v,tr,c),e) =>
					    Exp_b(v,tr,e))
			      (vcs,es1)
			  val bnd3 = 
			      Listops.map2 (fn (v,e) => 
					    Exp_b(v,TraceKnown TraceInfo.Notrace_Real, e))
			      (vs,es2)

			  val bnds = List.concat [bnd1,bnd2,bnd3]
		      in click(); rexp (Let_e (Sequential, bnds, e))
		      end
		  | _ =>  App_e (Open,Var_e f, 
				 map rcon cs,
				 map rexp es1,
				 map rexp es2))
	   | Raise_e (exp, con) => 
	       Raise_e (rexp exp, rcon con)
	   | _ => e)
      and rbnd b = 
       (case b of
	  Con_b (p, cbnd) => [(Con_b (p, rcbnd cbnd))]
	| Exp_b(v,nt,e) => 
	    (case e of
	       Switch_e _ => [Exp_b(v,nt,rexp e)]
             | Handle_e _ => [Exp_b(v,nt,rexp e)]
	     | App_e(Open,Var_e f,cs,es1,es2) =>
		 (case find_fun f of
		    SOME{definition=ref(SOME(func)),status,...} =>
		      let val shouldRename = (case !status of
						  InlineOnce => false
						| InlineMany => true
						| _ => error "cannot have NoInline here")
			  val Function{tFormals=vks,eFormals=vcs,fFormals=vs,body=e,...} = 
			  if shouldRename then rename_func func else func
			  val bnd1 = 
			      Listops.map2 (fn ((v,k),c) =>
					    Con_b(Runtime,Con_cb(v,c)))(vks,cs)
			  val bnd2 = 
			      Listops.map2 (fn ((v,tr,c),e) =>
					    Exp_b(v,tr,e))
			      (vcs,es1)
			  val bnd3 = 
			      Listops.map2 (fn (v,e) => 
					    Exp_b(v,TraceKnown TraceInfo.Notrace_Real, e))
			      (vs,es2)
			  val (bnd4,bnd5) = 
			    (case e of
			       Let_e(ls,bnds,e) => (bnds,[Exp_b(v,nt,e)])
			     | _ => ([],[Exp_b(v,nt,e)]))
			  val bnds = List.concat [bnd1,bnd2,bnd3,bnd4,bnd5]
			  val bnds = rbnds(bnds,[])
		      in click(); bnds
		      end
		  | _ => [Exp_b(v,nt, rexp e)])

	     | _ =>  [Exp_b(v,nt, rexp e)])
	| Fixopen_b vfs => 
	    (case Sequence.toList vfs of
	       [(v,f)] =>

		 (case find_fun v of
		    SOME {definition=ref(SOME _),status,...} =>
		         if (!status = InlineOnce) then
                            []  (* delete definition when fn inlined once *)
                         else
                            ([Fixopen_b(Sequence.fromList[(v,rfunction f)])])
		  | _ => [Fixopen_b(Sequence.fromList[(v,rfunction f)])])

	     | vfl => 
		   [Fixopen_b(Sequence.fromList
			      (List.map(fn (v,f) => (v,rfunction f)) vfl))])
	| Fixcode_b vfs => 
	      [Fixcode_b(Sequence.fromList
			 (List.map(fn (v,f) => (v,rfunction f))
			  (Sequence.toList vfs)))]
	| Fixclosure_b _ => [b])
      and rfunction(Function{effect,recursive,isDependent,
			     tFormals,eFormals,fFormals,
			     body,body_type}) = 
	  Function{effect=effect,recursive=recursive,isDependent=isDependent,
		   tFormals=tFormals,eFormals=eFormals,fFormals=fFormals,
		   body=rexp body, body_type=body_type}

      and rswitch sw = 
	(case sw of
	   Intsw_e{arg,size,arms,default,result_type} =>
	       Intsw_e{arg=rexp arg,size=size,
		       arms=List.map(fn(w,e) => (w,rexp e)) arms,
		       default=ropt rexp default,
		       result_type = rcon result_type}
	 | Sumsw_e{arg,(*unroll=u,*)sumtype,bound,arms,default,result_type} => 
	       Sumsw_e{arg=rexp arg,(*unroll=u,*)sumtype=rcon sumtype,bound=bound,
		       arms=List.map(fn(w,tr,e) => (w,tr,rexp e)) arms,
		       default=ropt rexp default,
		       result_type = rcon result_type}
	 | Exncase_e {arg,bound,arms,default,result_type} => 
	       Exncase_e{arg=rexp arg,bound=bound,
			 arms=List.map(fn(e1,tr,e2) => (e1,tr,rexp e2)) arms,
			 default=ropt rexp default,
			 result_type = rcon result_type}
	 | Typecase_e {arg,arms,default,result_type} => 
	       Typecase_e{arg=rcon arg,
			  arms=List.map(fn (vks,e) => (vks,rexp e)) arms,
			  default=ropt rexp default,
			  result_type = rcon result_type})
      and rmodule (MODULE{bnds,imports,exports}) = 
	  MODULE{bnds=rbnds(bnds,[]),imports=imports,exports=exports}
  in 
      if inline_count > 0 then
	  let val m' = rmodule m
	  in (!clicks,m')
	  end
      else (0,m)
  end
end (* struct *)

