(* Perform the following local cleanups:
   (1) Drop all record components at term and type level if the label
       is a datatype label.  These correspond to structures whose
       components are always unused because all uses were inlined away.
       Also, drop import and exports with datatype labels since these
       are the top-level datatypes.
       Check this invariant by checking the labels in the select primitive.
   (3) Perform certain beta reductions.
       Transform:
                   let ...
		          bnds
			   ...
			   fixopen-LEAF foo formal = ... (* LEAF ensures it's non-recursive *)
		      in  foo arg
                   end
       To:
                   let ...
		          bnds
			   ...
			   val formal = arg
		      in  body
                   end
   (4) Perform this rewrite and then, if possible, apply the beta rule above.
       Transform:
                  App(let bnds
                      in  body
                      end, args)      where FV(args) and Bound(bnds) are disjoint
	To:
	           let bnds
                  in  App(body,args)
                  end
*)

functor Cleanup(structure Nil : NIL
		structure Ppnil : PPNIL
		structure NilUtil : NILUTIL
		structure IlUtil : ILUTIL
		structure Subst : NILSUBST
		sharing NilUtil.Nil = Ppnil.Nil = Nil
			 and type Subst.con = Nil.con
		         and type Subst.exp = Nil.exp
			 and type Subst.kind = Nil.kind)
    : CLEANUP = 
struct

    val error = fn s => Util.error "cleanup.sml" s
    structure Nil = Nil
    open Nil Name NilUtil
    val flatten = Listops.flatten
    val list2set = Util.list2set
    val set2list = Util.set2list
    val list2sequence = Util.list2sequence
    val sequence2list = Util.sequence2list

    val substConInExp = Subst.substConInExp
    val substConInCon = Subst.substConInCon
    val substExpInExp = Subst.substExpInExp

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
	
        (* Rule 3 helper: actually perform the beta reduction *)    
	fun ereduce(vklist,vclist,flist,body,c,cargs,eargs,fargs) = 
	    let val etab_rebnds = foldl earg_folder ([],[]) (Listops.zip vclist eargs) 
		val float_con = Prim_c(Float_c (Prim.F64), [])
		val (exp_table,rev_ebnds) = foldl earg_folder etab_rebnds (Listops.zip 
									  (map (fn v => (v,float_con)) flist) fargs)
		val (con_table,rev_cbnds) = foldl carg_folder ([],[]) (Listops.zip vklist cargs)
		val con_subster = Subst.fromList con_table
		val exp_subster = Subst.fromList exp_table
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
		    val subster = Subst.fromList table
		    val body = substConInCon subster c
		    val result = (case rev_cbnds of
				      [] => body
				    | _ => Let_c(Sequential, rev rev_cbnds, body))
		in  result
		end


(* Rule 4 *)
    fun ehandler' (bound, exp) = 
	(print "cleanup.sml: ehandler with ";
	     Ppnil.pp_exp exp;
	     print "\n\n";
	     ehandler (bound,exp))

    and ehandler (bound,e as App_e(openness,f,cargs,eargs,fargs)) = 
	 (    case f of
		 Let_e(Sequential,bnds, Var_e v) => 
		     let fun addone(v1,fv) = VarSet.addList(fv,v1)
			 fun addpair((v1,v2),fv) = VarSet.addList(VarSet.addList(fv,v1),v2)
			 val fv = foldl addpair VarSet.empty (map NilUtil.freeExpConVarInExp eargs)
			 val fv = foldl addpair fv (map NilUtil.freeExpConVarInExp fargs)
			 val fv = foldl addone  fv (map NilUtil.freeConVarInCon cargs)
			 fun vf_mem(v,_) = VarSet.member(fv,v)
			 fun bnd_check (Con_b(v,_,_)) = VarSet.member(fv,v)
			   | bnd_check (Exp_b(v,_,_)) = VarSet.member(fv,v)
			   | bnd_check ((Fixopen_b vfset) | (Fixcode_b vfset)) =
			       Listops.orfold vf_mem (set2list vfset)
			   | bnd_check (Fixclosure_b vfset) = 
			       Listops.orfold vf_mem (set2list vfset)
			 val intersect = Listops.orfold bnd_check bnds
		     in  if (intersect)
			     then (print "intersected with Var_e v = ";
				   Ppnil.pp_var v; print "\n";
				   NOCHANGE)
			 else ehandler(bound,Let_e(Sequential,bnds, 
						   App_e(openness,Var_e v,cargs,eargs,fargs)))
		     end
	       | _ => NOCHANGE)
(* Rule 1 *)
      | ehandler (_,Prim_e(NilPrimOp(record labels),clist,elist)) = 
	let val changed = ref false
	    val triples = Listops.zip3 labels clist elist
	    val triples = List.mapPartial (fn (t as (l,_,_)) => if (IlUtil.is_datatype_lab l)
								    then (changed := true; NONE)
								else SOME t) triples
	    val labels = map #1 triples
	    val clist = map #2 triples
	    val elist = map #3 triples
	in  if (!changed)
		then CHANGE_RECURSE(Prim_e(NilPrimOp(record labels), clist, elist))
	    else NOCHANGE
	end
(* Rule 1 *)
      | ehandler (_, e as Prim_e(NilPrimOp(select l),_,_)) = 
	if (IlUtil.is_datatype_lab l)
	    then error "use of datatype label: should all be inlined away"
	else NOCHANGE
(* Rule 3 --- what we really want is to recurse first *)
      | ehandler (_,e as Let_e(letsort,bnds,body)) = 
	    let val changed = ref false
		fun loop acc [] = if !changed 
				      then CHANGE_RECURSE(Let_e(letsort,rev acc,body))
				  else NOCHANGE
                  (* Rule 3: applies exactly if there is one bnd *)
		  | loop acc [bnd as ((Fixopen_b vfset) | (Fixcode_b vfset))] = 
		      (case (set2list vfset,body) of
			   ([(v,Function(_, Leaf, vklist, vclist, flist, e, c))],
			    App_e(_,Var_e v',cargs,eargs,fargs)) =>
			   if (eq_var(v,v'))
			       then CHANGE_RECURSE(Let_e(letsort, rev acc,
							 ereduce(vklist,vclist,flist,e,c,cargs,eargs,fargs)))
			   else (changed := true; loop (bnd::acc) [])
			  | _ => loop (bnd::acc) [])
		  | loop acc (bnd::rest) = loop (bnd::acc) rest
	    in  loop [] bnds
	    end
      | ehandler _ = NOCHANGE


     fun chandler (_,App_c(f,cargs)) =
	 (case f of
	      (Let_c (_,[((Open_cb(v,vklist,c,k)) |
			  (Code_cb(v,vklist,c,k)))],Var_c v')) =>
	         if (eq_var(v,v')) (* constructor functions not recursive *)
		     then CHANGE_RECURSE(creduce(vklist,c,cargs))
		 else NOCHANGE
	      | _ => NOCHANGE)

(* Rule 1 - sanity check *)
	  | chandler (_,Prim_c(Record_c labels, clist)) = 
	    let val changed = ref false
		val pairs = Listops.zip labels clist
		val pairs = List.mapPartial (fn (p as (l,_)) => if (IlUtil.is_datatype_lab l)
								    then (changed := true; NONE)
								else SOME p) pairs
		val labels = map #1 pairs
		val clist = map #2 pairs
	    in  if (!changed)
		    then CHANGE_RECURSE(Prim_c(Record_c labels, clist))
		else NOCHANGE
	    end
	  | chandler _ = NOCHANGE
    end (* local *)

    fun nada _ = NOCHANGE
    val handlers = (ehandler,nada,chandler,nada,nada)
    fun reduceExp e = exp_rewrite handlers e
    fun reduceCon c = con_rewrite handlers c
    fun reduceKind k = kind_rewrite handlers k
    fun reduceBnd b = bnd_rewrite handlers b
    fun reduceImport (imp as ((ImportValue(l,_,_)) | (ImportType(l,_,_)))) = 
	if (IlUtil.is_datatype_lab l)
	    then NONE else SOME imp
    fun reduceExport (ExportValue(l,e,c)) = if (IlUtil.is_datatype_lab l)
						then NONE
					    else SOME(ExportValue(l,reduceExp e, reduceCon c))
      | reduceExport (ExportType (l,c,k)) = if (IlUtil.is_datatype_lab l)
						then NONE
					    else SOME(ExportType(l,reduceCon c, reduceKind k))

    fun cleanModule (MODULE{bnds, imports, exports}) = 
	let val temp = Let_e(Sequential,bnds,true_exp)
	    val Let_e(Sequential,bnds,_) = reduceExp temp
	    val imports = List.mapPartial reduceImport imports
	    val exports = List.mapPartial reduceExport exports
	in  MODULE{bnds = bnds,
		  imports = imports,
		  exports = exports}
	end

   end
