(* Remember the definitions of all encoutered functions.
   Perform the reduction in place whenever the function is used. *)
functor BetaReduce(structure Nil : NIL
		   structure Ppnil : PPNIL
		   structure NilUtil : NILUTIL
		   structure IlUtil : ILUTIL
		   sharing NilUtil.Nil = Ppnil.Nil = Nil) 
    : BETAREDUCE = 
struct

    val error = fn s => Util.error "betareduce.sml" s
    structure Nil = Nil
    open Nil Name NilUtil
    val flatten = Listops.flatten
    val list2set = Util.list2set
    val set2list = Util.set2list
    val list2sequence = Util.list2sequence
    val sequence2list = Util.sequence2list

    val debug = ref false


    local
	fun carg_folder (((v,k),carg),(table,cbnds)) = 
	    (case carg of
		 Var_c v' => ((v,carg)::table, cbnds)
	       | _ => (table, (Con_cb(v,k,carg))::cbnds))
	fun earg_folder (((v,c),earg),(table,ebnds)) = 
	    (case earg of
		 Var_e v' => ((v,earg)::table, ebnds)
	       | _ => (table, (Exp_b(v,c,earg))::ebnds))
    in  

	fun ereduce(vklist,vclist,flist,body,c,cargs,eargs,fargs) = 
	    let val etab_rebnds = foldl earg_folder ([],[]) (Listops.zip vclist eargs) 
		val float_con = Prim_c(Float_c (Prim.F64), [])
		val (exp_table,rev_ebnds) = foldl earg_folder etab_rebnds (Listops.zip 
									  (map (fn v => (v,float_con)) flist) fargs)
		val (con_table,rev_cbnds) = foldl carg_folder ([],[]) (Listops.zip vklist cargs)
		fun con_subster v = Listops.assoc_eq(eq_var, v, con_table)
		fun exp_subster v = Listops.assoc_eq(eq_var, v, exp_table)
		val body = substConInExp con_subster body
		val body = substExpInExp exp_subster body
		val body = (case rev_ebnds of
				      [] => body
				    | _ => Let_e(Sequential, rev rev_ebnds, body))
		val body = (case rev_cbnds of
				      [] => body
				    | _ => Let_e(Sequential, rev (map cbnd2bnd rev_cbnds), body))
	    in body
	    end


	fun creduce(vklist,c,cargs) =
	    let val (table,rev_cbnds) = foldl carg_folder ([],[]) (Listops.zip vklist cargs)
		fun subster v = Listops.assoc_eq(eq_var, v, table)
		val body = substConInCon subster c
		val result = (case rev_cbnds of
				  [] => body
				    | _ => Let_c(Sequential, rev rev_cbnds, body))
	    in  result
	    end

	local
	    val confuns = ref ([] : (var * ((var * kind) list * con * kind)) list)
	    val funs = ref ([] : (var * ((var * kind) list * (var * con) list * var list * exp * con)) list)
	in  fun reset() = (confuns := []; funs := [])
	    fun add_confun (v,vklist,c,k) = confuns := (v,(vklist,c,k))::(!confuns)
	    fun find_confun v = Listops.assoc_eq(eq_var,v,!confuns)
	    fun add_fun (v,quint) = funs := (v,quint)::(!funs)
	    fun find_fun v = Listops.assoc_eq(eq_var,v,!funs)
	end

	fun bnd_handler' (bound,bnd) = 
	    (print "bnd_handler called on:\n";
	     Ppnil.pp_bnd bnd;
	     print "\n\n";
	     bnd_handler(bound,bnd))

	and bnd_handler (_,Con_b(v,_,Let_c(_,[Open_cb(v',vklist,c,k)],Var_c v''))) = 
	    (if (eq_var(v',v''))
		 then ((* print "adding confun "; Ppnil.pp_var v; print "\n"; *)
		       add_confun(v,vklist,c,k))
	     else (); NOCHANGE)
	  | bnd_handler (_,Fixopen_b vfset) = 
		 let val vflist = set2list vfset
		 in  (case vflist of
			  [(v,Function(_,Leaf,vklist,vclist,flist,body,con))] => 
			      ((* print "adding fun "; Ppnil.pp_var v; print "\n"; *)
			       add_fun(v,(vklist,vclist,flist,body,con)))
			| _ => ();
		      NOCHANGE)
		 end
	  | bnd_handler _ = NOCHANGE

	fun ehandler' (bound,exp) = 
	    (print "ehandler called on:\n";
	     Ppnil.pp_exp exp;
	     print "\n\n";
	     ehandler(bound,exp))
		 
	and ehandler 
(*	     (_,App_e(_,f,cargs,eargs,fargs)) = 
	     (case f of
		Let_e(Sequential,[((Fixopen_b vfset) | (Fixcode_b vfset))], Var_e v') => 
		    (case (set2list vfset) of
			 [(v,Function(_, Leaf, vklist, vclist, flist, e, c))] =>
			     if (eq_var(v,v'))
				 then CHANGE_RECURSE(ereduce(vklist,vclist,flist,e,c,cargs,eargs,fargs))
			     else NOCHANGE
		       | _ => NOCHANGE)
	      | Var_e v => (case (find_fun v) of
				NONE => NOCHANGE
			      | SOME (vklist,vclist,flist,e,c) => 
				    CHANGE_RECURSE(ereduce(vklist,vclist,flist,e,c,cargs,eargs,fargs)))
	      | _ => NOCHANGE)
	  | ehandler *) _ = NOCHANGE
	    
	fun chandler (_,App_c(f,cargs)) =
	    (case f of
		     (Let_c (_,[((Open_cb(v,vklist,c,k)) |
				 (Code_cb(v,vklist,c,k)))],Var_c v')) =>
		     if (eq_var(v,v')) (* constructor functions not recursive *)
			 then CHANGE_RECURSE(creduce(vklist,c,cargs))
		     else NOCHANGE
		   | Var_c v => (case (find_confun v) of
				     NONE => NOCHANGE
				   | SOME (vklist,c,k) => CHANGE_RECURSE(creduce(vklist,c,cargs)))
		   | _ => NOCHANGE)
	  | chandler _ = NOCHANGE
    end (* local *)


    (* ------------ putting it all together ------------------- *)
    fun nada _ = NOCHANGE
    val handlers = (ehandler,bnd_handler,chandler,nada,nada)
    fun reduceExp e = exp_rewrite handlers e
    fun reduceCon c = con_rewrite handlers c
    fun reduceKind k = kind_rewrite handlers k
    fun reduceBnd b = bnd_rewrite handlers b
    fun reduceExport (ExportValue(l,e,c)) = ExportValue(l,reduceExp e, reduceCon c)
      | reduceExport (ExportType (l,c,k)) = ExportType(l,reduceCon c, reduceKind k)

    fun reduceModule (MODULE{bnds, imports, exports}) = 
	let val _ = reset()
	    val temp = Let_e(Sequential,bnds,true_exp)
	    val Let_e(Sequential,bnds,_) = reduceExp temp
	    val imports = imports
	    val exports = map reduceExport exports
	in  MODULE{bnds = bnds,
		  imports = imports,
		  exports = exports}
	end

   end
