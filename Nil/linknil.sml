structure LinkNil =
struct
    structure Nil = Nil(structure Annotation = Annotation
			structure Prim = LinkIl.Prim)      
	
    structure Ppnil = Ppnil(structure Nil = Nil
			    structure Ppprim = LinkIl.Ppprim)

    structure NilUtil = NilUtilFn(structure Nil = Nil)
      
    structure NilStatic = NilStaticFn(structure Annotation = Annotation
				      structure Prim = LinkIl.Prim
				      structure Nil = Nil
				      structure NilUtil = NilUtil
				      structure Cont = Cont)
(*
    structure Tonil = Tonil(structure Ilstatic = LinkIl.IlStatic
			    structure Ilutil = LinkIl.IlUtil
                            structure Ilcontext = LinkIl.IlContext
			    structure Nilutil = Nilutil
			    structure Ppil = LinkIl.Ppil)

    fun test s = 
	let
(*	    val _ = print "1\n" *)
	    val (sbnds, decs) = LinkIl.elaborate s
	    val _ = LinkIl.Ppil.pp_sbnds sbnds
            val _ = print "\n"
(* 	    val _ = print "2\n" *)
(*	    val _ = Profile.reset () *)
	    val {name_c, name_r, rev_cbnds, rev_ebnds, ...} =
		Tonil.xmod decs (LinkIl.MOD_STRUCTURE sbnds)
(*	    val _ = Profile.report TextIO.stdOut*)
(*	    val _ = print "4\n" *)
	    val bnds = (List.rev (map Nil.Con_b rev_cbnds)) @ 
		       (List.rev rev_ebnds)
	in
	    print "\n\n\nELABORATION SUCESSFULLY COMPLETED\n\n\n";
	    Ppnil.pp_bnds bnds;
	    print "\n"
	end
           handle (LinkIl.IlContext.NOTFOUND s) => print ("NOTFOUND: " ^ s)
*)
end
(*
fun doit () = Linknil.test "test"
*)