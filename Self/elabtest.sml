(*$import TextIO LinkParse  Tyvar Prim Il Ppprim Ppil IlContext LinkIl Stats *)

(*
val xxx = Il.CON_SUM{noncarriers=2,carrier=Il.CON_TUPLE_INJECT[],special=NONE}
val yyy = Il.CON_SUM{noncarriers=2,carrier=Il.CON_TUPLE_INJECT[],special=NONE}
val _ = if (LinkIl.IlStatic.eq_con(LinkIl.IlContext.empty_context,xxx,yyy))
	    then print "same\n"
	else print "different"
*)

val _ = Stats.tt("littleEndian")
val _ = print "Test entered\n"
val (_,fp,str,dec) = LinkParse.parse_impl "Bench/hello.sml"
val _ = print "Test parsed itself!\n"
val initial_ctxt = LinkIl.initial_context()
val _ = (print "Test - made initial basis\n";
	 LinkIl.Ppil.pp_context initial_ctxt;
	 print "\n\n")
val ctxt_sbnd_entries_opt = LinkIl.elab_dec(initial_ctxt,fp,dec)
val _ = print "Test elaborated!\n"
val _ = (case ctxt_sbnd_entries_opt of
	 NONE => print "elaboration returned NONE\n"
	| SOME (ctxt,sbnd_entries) => 
	     let val _ = print "elaboration returned SOME\n"
		 fun loop [] = ()
		   | loop ((NONE,_)::rest) = 
		     (print "NOSBND\n"; loop rest)
		   | loop ((SOME sbnd,_)::rest) = 
		     (print "SBND: ";
		      LinkIl.Ppil.pp_sbnd sbnd;
		      print "\n"; loop rest)
	     in  loop sbnd_entries
	     end)

