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
(*	    val _ = print "1\n" *)
	    val SOME(sbnds, decs) = LinkIl.elaborate s
(*	    val _ = LinkIl.Ppil.pp_sbnds sbnds  *)
	    val _ = print "\n\n\nELABORATION SUCESSFULLY COMPLETED\n\n\n"; 
            val _ = print "\n"
(* 	    val _ = print "2\n" *)
	    val _ = Compiler.Profile.reset () 
	    val {name_c, name_r, cbnd_cat, ebnd_cat, ...} =
		Tonil.xmod decs (LinkIl.MOD_STRUCTURE sbnds, Tonil.empty_vmap, NONE)
	    val _ = Compiler.Profile.report TextIO.stdOut
(*	    val _ = print "4\n" *)
	    val bnds = (map Nil.Con_b (Tonil.flattenCatlist cbnd_cat)) @ 
		       (Tonil.flattenCatlist ebnd_cat)
	in
	    print "\nPhase-splitting done.\n";
	    Ppnil.pp_bnds bnds; 
            print "\nSize of IL = \n";
	    print (Int.toString (LinkIl.IlUtil.mod_size 
				 (LinkIl.MOD_STRUCTURE sbnds)));
	    print "\n"
	end
end

fun doit () = Linknil.test "test"

