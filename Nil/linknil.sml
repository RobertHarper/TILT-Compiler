structure Linknil =
struct
    structure Nil = Nil(structure Annotation = Annotation
			structure ArgPrim = LinkIl.Prim)      
	
    structure Ppnil = Ppnil(structure Nil = Nil
			    structure Ppprim = LinkIl.Ppprim)

    structure NilUtil = NilUtilFn(structure ArgNil = Nil
				  structure IlUtil = LinkIl.IlUtil)
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
			    structure Nilutil = NilUtil
			    structure Ppil = LinkIl.Ppil)

    fun test s = 
	let
	    val SOME(sbnds, decs) = LinkIl.elaborate s
	    val _ = print "\n\n\nELABORATION SUCESSFULLY COMPLETED\n\n\n"; 
            val _ = print "\nSize of IL = ";
	    val _ = print (Int.toString (LinkIl.IlUtil.mod_size 
					 (LinkIl.MOD_STRUCTURE sbnds)));
            val _ = print "\n\n"
	    val _ = Compiler.Profile.reset () 
	    val {cu_c, cu_c_kind, cu_r, cu_r_type} =
		Tonil.xcompunit decs sbnds
	    val _ = print "\nPhase-splitting done.\n";
	    val _ = Compiler.Profile.report TextIO.stdOut 
	in
	    Ppnil.pp_con cu_c;
	    print "\n";
	    Ppnil.pp_kind cu_c_kind;
	    print "\n";
	    Ppnil.pp_exp cu_r;
	    print "\n";
	    Ppnil.pp_con cu_r_type;
	    print "\n"
	end
end

fun doit () = Linknil.test "test"

