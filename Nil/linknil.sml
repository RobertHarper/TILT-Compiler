signature LINKNIL = 
sig
    structure Nil : NIL
    structure NilUtil : NILUTIL
    structure NilContext : NILCONTEXT
    structure NilStatic : NILSTATIC
    structure PpNil : PPNIL

    val compile_prelude : bool * string -> Nil.module
    val compile : string -> Nil.module
    val test : string -> Nil.module

end

structure Linknil : LINKNIL =
struct

    val error = fn s => Util.error "linknil.sml" s

    structure Nil = Nil(structure ArgAnnotation = Annotation
			structure ArgPrim = LinkIl.Prim)      
	
    structure PpNil = Ppnil(structure Nil = Nil
			    structure Ppprim = LinkIl.Ppprim)

    structure Alpha = Alpha(structure ArgNil = Nil)

    structure NilUtil = NilUtilFn(structure ArgNil = Nil
				  structure IlUtil = LinkIl.IlUtil
				  structure Alpha = Alpha)

    structure NilContext = NilContextFn(structure ArgNil = Nil
					structure PpNil = PpNil
					structure Cont = Cont)

    structure NilStatic = NilStaticFn(structure Annotation = Annotation
				      structure Prim = LinkIl.Prim
				      structure ArgNil = Nil
				      structure NilUtil = NilUtil
				      structure NilContext = NilContext
				      structure PpNil = PpNil
				      structure Alpha = Alpha)


    structure NilPrimUtilParam = NilPrimUtilParam(structure NilUtil = NilUtil);
	
    structure NilPrimUtil = PrimUtil(structure Prim = LinkIl.Prim
				     structure Ppprim = LinkIl.Ppprim
				     structure PrimUtilParam = NilPrimUtilParam);
    structure Tonil = Tonil(structure Il = LinkIl.Il
			    structure Nilstatic = NilStatic
			    structure Nilprimutil = NilPrimUtil
			    structure Ilutil = LinkIl.IlUtil
                            structure Ilcontext = LinkIl.IlContext
			    structure Nilcontext = NilContext
			    structure Nilutil = NilUtil
			    structure Ppnil = PpNil
			    structure Ppil = LinkIl.Ppil)

    structure Linearize = Linearize(structure Nil = Nil
				    structure NilUtil = NilUtil)

    structure ToClosure = ToClosure(structure Nil = Nil
				    structure Ppnil = PpNil
				    structure NilUtil = NilUtil)

	
    structure NilEval = NilEvaluate(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Ppnil = PpNil
				    structure PrimUtil = NilPrimUtil)

    fun phasesplit debug (ctxt,sbnds) : Nil.module = 
	let
	    open Nil LinkIl.Il LinkIl.IlContext Name
	    fun folder (v,(l,pc),maps as (vmap,mmap)) = 
		let fun addv () = (VarMap.insert(vmap,v,l),mmap)
		    fun addm () = (vmap,VarMap.insert(mmap,v,l))
		in  (case pc of
			 PHRASE_CLASS_EXP _ => addv()
		       | PHRASE_CLASS_CON _ => addv()
		       | PHRASE_CLASS_MOD _ => addm()
		       | PHRASE_CLASS_SIG _ => maps
		       | PHRASE_CLASS_OVEREXP _ => maps)
		end
	    val varmap = LinkIl.IlContext.Context_Varmap ctxt
	    val (import_valmap,import_modmap) = (VarMap.foldli folder 
						 (VarMap.empty,VarMap.empty) varmap)
	    fun folder ((SBND(l,bnd)),maps as (vmap,mmap)) = 
		if (is_label_internal l)
		    then maps
		else (case bnd of
			  BND_EXP (v,_) => (VarMap.insert(vmap,v,l),mmap)
			| BND_CON (v,_) => (VarMap.insert(vmap,v,l),mmap)
			| BND_MOD (v,_) => (vmap,VarMap.insert(mmap,v,l)))

	    val (export_valmap,export_modmap) = foldl folder (VarMap.empty,VarMap.empty) sbnds
	    fun folder (v,l,map) = let val vc = fresh_named_var "myvc"
				       val vr = fresh_named_var "myvr"
				   in  VarMap.insert(map,v,(vc,vr))
				   end
	    val import_varmap = VarMap.foldli folder VarMap.empty import_modmap
	    val {cu_bnds = bnds, vmap = total_varmap} = Tonil.xcompunit ctxt import_varmap sbnds

	    fun folder (v,l,map) = 
		let val lc = internal_label((label2string l) ^ "_c")
		    val lr = internal_label((label2string l) ^ "_r")
		    val (vc,vr) = (case VarMap.find(total_varmap,v) of
				       SOME vrc => vrc
				     | NONE => error "total_varmap missing bindings")
		in  VarMap.insert(VarMap.insert(map,vr,lr),vc,lc)
		end
	    val imports = Name.VarMap.foldli folder import_valmap import_modmap
	    val exports = Name.VarMap.foldli folder export_valmap export_modmap
	    val nilmod = MODULE{bnds = bnds, 
				imports = imports,
				exports = exports}
	    val _ = if debug
			then (print "\n\n=======================================\n\n";
			      print "phase-split results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "Phase-splitting complete\n"
	in  nilmod
	end
    val phasesplit = Stats.timer("Phase-splitting",phasesplit)


    fun compile' debug (ctxt,sbnds) = 
	let
	    open Nil LinkIl.Il LinkIl.IlContext Name
	    val nilmod = phasesplit debug (ctxt,sbnds)
	    val nilmod = (Stats.timer("Linearization",Linearize.linearize_mod)) nilmod
	    val _ = if debug 
			then (print "\n\n=======================================\n\n";
			      print "renaming results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "Renaming complete\n"
	    val nilmod = (Stats.timer("Closure-conversion",ToClosure.close_mod)) nilmod
	    val _ = if debug
			then (print "\n\n=======================================\n\n";
			      print "closure-conversion results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "Closure-conversion complete\n"
	in  nilmod
	end

    fun test filename = 
	let val SOME(ctxt,sbnds,_) = LinkIl.test filename
	in  compile' true (ctxt,sbnds)
	end
    fun compile filename = 
	let val SOME(ctxt,sbnds,_) = LinkIl.compile filename
	in  compile' false (ctxt,sbnds)
	end
    val cached_prelude = ref (NONE : Nil.module option)
    fun compile_prelude (use_cache,filename) = 
	case (use_cache, !cached_prelude) of
		(true, SOME m) => m
	      | _ => let val (ctxt,sbnds,_) = 
				LinkIl.compile_prelude(use_cache,filename)
			 val m = compile' false (ctxt,sbnds)
			 val _ = cached_prelude := SOME m
		     in  m
		     end
end

