structure Linknil =
struct
    structure Nil = Nil(structure Annotation = Annotation
			structure ArgPrim = LinkIl.Prim)      
	
    structure Ppnil = Ppnil(structure Nil = Nil
			    structure Ppprim = LinkIl.Ppprim)

    structure NilUtil = NilUtilFn(structure ArgNil = Nil
				  structure IlUtil = LinkIl.IlUtil)

    structure Nilcontext = NilContextFn(structure ArgNil = Nil
					structure PPNil = Ppnil
					structure Cont = Cont)
(*      
    structure NilStatic = NilStaticFn(structure Annotation = Annotation
				      structure Prim = LinkIl.Prim
				      structure Nil = Nil
				      structure NilUtil = NilUtil
				      structure Cont = Cont)
*)

    structure Tonil = Tonil(structure Ilstatic = LinkIl.IlStatic
			    structure Ilutil = LinkIl.IlUtil
                            structure Ilcontext = LinkIl.IlContext
			    structure Nilcontext = Nilcontext
			    structure Nilutil = NilUtil
			    structure Ppnil = Ppnil
			    structure Ppil = LinkIl.Ppil)

    structure ToClosure = ToClosure(structure Nil = Nil
				    structure NilUtil = NilUtil)

    structure NilPrimUtilParam = NilPrimUtilParam(structure NilUtil = NilUtil);
	
    structure NilPrimUtil = PrimUtil(structure Prim = LinkIl.Prim
				     structure Ppprim = LinkIl.Ppprim
				     structure PrimUtilParam = NilPrimUtilParam);
	
    structure NilEval = NilEvaluate(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Ppnil = Ppnil
				    structure PrimUtil = NilPrimUtil)
    type nilmodule = {bnds : Nil.bnd list,
		      name_c : Nil.var, knd_c : Nil.kind,
		      name_r : Nil.var, type_r : Nil.con}

    fun test s = 
	let
	    open Nil
	    val SOME(sbnds, decs) = LinkIl.elaborate s
	    val _ = print "\n\n\nELABORATION SUCESSFULLY COMPLETED\n\n\n"; 
	    val _ = LinkIl.Ppil.pp_sbnds sbnds
            val _ = print "\nSize of IL = ";
	    val _ = print (Int.toString (LinkIl.IlUtil.mod_size 
					 (LinkIl.MOD_STRUCTURE sbnds)));
            val _ = print "\n\n========================================\n"
	    val _ = Compiler.Profile.reset () 
	    val {cu_bnds, cu_c_var, cu_c_kind, cu_r_var, cu_r_type} =
		Tonil.xcompunit decs sbnds
	    val _ = print "\nPhase-splitting done.\n";
	    val nilmod : nilmodule = {bnds = cu_bnds, 
				      name_c = cu_c_var, name_r = cu_r_var,
				      knd_c = cu_c_kind, type_r = cu_r_type}
	    val {bnds,name_c,name_r,knd_c,type_r} = nilmod
	    val _ = (print "\n\n=======================================\n\n";
		     print "phase-split results:\n";
		     Ppnil.pp_var name_c; print "\n";
		     Ppnil.pp_var name_r; print "\n";
		     Ppnil.pp_bnds bnds)
	    val temp_var = Name.fresh_var()
	    val temp_exp = Let_e(Sequential,bnds,
				 Let_e(Sequential,[Con_b(temp_var,knd_c,Var_c name_c)],
							 Var_e name_r))
	    val temp_exp_closed = ToClosure.close_exp temp_exp
	    val Let_e(_,bnds',_) = temp_exp_closed
	    val knd_c' = ToClosure.close_kind knd_c
	    val type_r' = ToClosure.close_con type_r

	    val nilmod' : nilmodule = {bnds = bnds', name_c = name_c, name_r = name_r,
				      knd_c = knd_c', type_r = type_r'}

(*	    val _ = Compiler.Profile.report TextIO.stdOut  *)

	in

	    print "closure-conversion results:\n";
	    Ppnil.pp_var name_c; print "\n";
	    Ppnil.pp_var name_r; print "\n";
	    Ppnil.pp_bnds bnds';
	    print "\n\n";

	    nilmod'

	end
end

fun doit () = Linknil.test "test"

