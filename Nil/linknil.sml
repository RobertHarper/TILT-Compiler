structure Linknil =
struct

    val error = fn s => Util.error "linknil.sml" s

    structure Nil = Nil(structure ArgAnnotation = Annotation
			structure ArgPrim = LinkIl.Prim)      
	
    structure Ppnil = Ppnil(structure Nil = Nil
			    structure Ppprim = LinkIl.Ppprim)

    structure Alpha = Alpha(structure ArgNil = Nil)

    structure NilUtil = NilUtilFn(structure ArgNil = Nil
				  structure IlUtil = LinkIl.IlUtil
				  structure Alpha = Alpha)

    structure Nilcontext = NilContextFn(structure ArgNil = Nil
					structure PPNil = Ppnil
					structure Cont = Cont)

    structure NilStatic = NilStaticFn(structure Annotation = Annotation
				      structure Prim = LinkIl.Prim
				      structure ArgNil = Nil
				      structure NilUtil = NilUtil
				      structure NilContext = Nilcontext
				      structure Ppnil = Ppnil
				      structure Alpha = Alpha)


    structure Tonil = Tonil(structure Ilstatic = LinkIl.IlStatic
			    structure Ilutil = LinkIl.IlUtil
                            structure Ilcontext = LinkIl.IlContext
			    structure Nilcontext = Nilcontext
			    structure Nilutil = NilUtil
			    structure Ppnil = Ppnil
			    structure Ppil = LinkIl.Ppil)

    structure ToClosure = ToClosure(structure Nil = Nil
				    structure Ppnil = Ppnil
				    structure NilUtil = NilUtil)

    structure NilPrimUtilParam = NilPrimUtilParam(structure NilUtil = NilUtil);
	
    structure NilPrimUtil = PrimUtil(structure Prim = LinkIl.Prim
				     structure Ppprim = LinkIl.Ppprim
				     structure PrimUtilParam = NilPrimUtilParam);
	
    structure NilEval = NilEvaluate(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Ppnil = Ppnil
				    structure PrimUtil = NilPrimUtil)

    fun test s = 
	let
	    open Nil LinkIl.Il LinkIl.IlContext Name
	    val SOME(sbnds, ctxt) = LinkIl.elaborate s
	    val _ = print "\n\n\nELABORATION SUCESSFULLY COMPLETED\n\n\n"; 
	    val _ = LinkIl.Ppil.pp_sbnds sbnds
            val _ = print "\nSize of IL = ";
	    val _ = print (Int.toString (LinkIl.IlUtil.mod_size 
					 (LinkIl.MOD_STRUCTURE sbnds)));
            val _ = print "\n\n========================================\n"
            val _ = (print "\n\ninitial_context = ctxt = \n";
		     LinkIl.Ppil.pp_context ctxt;
		     print "\n")
	    val _ = Compiler.Profile.reset () 
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
            val _ = print "\nAbout to do Phase-split\n"
	    val {cu_bnds = bnds, vmap = total_varmap} = Tonil.xcompunit ctxt import_varmap sbnds
	    val _ = print "\nPhase-splitting done.\n"

	    fun folder (v,l,map) = let val lc = internal_label((label2string l) ^ "_c")
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
	    val _ = (print "\n\n=======================================\n\n";
		     print "phase-split results:\n";
		     Ppnil.pp_module nilmod;
		     print "\n")
		
	    val nilmod' = ToClosure.close_mod nilmod

	    val _ = (print "\n\n=======================================\n\n";
		     print "closure-conversion results:\n";
		     Ppnil.pp_module nilmod';
		     print "\n")
		
	in  nilmod'
	end
end

fun doit () = Linknil.test "test"

