(*$import TextIO LinkParse  Tyvar Prim Il Ppprim Ppil IlContext LinkIl Linknil *)

exception End

val _ = Stats.tt("littleEndian")
val _ = print "Test entered\n"
val (_,fp,str,dec) = LinkParse.parse_impl "Bench/hello.sml"
val _ = print "Test parsed itself!\n"
val initial_ctxt = LinkIl.initial_context()
val _ = print "Test - made initial basis\n"
val ctxt_sbnd_entries_opt = LinkIl.elab_dec(initial_ctxt,fp,dec)
val _ = print "Test elaborated!\n"
val hilmod = (case ctxt_sbnd_entries_opt of
		   NONE => (print "Elaboration failed.\n"; raise End)
		 | SOME (ctxt',sbnd_entries) => (print "Elaboration succeeded!\n"; 
						 (initial_ctxt,sbnd_entries)))
val _ = Stats.ff("UptoPhasesplit")
val nilmod = Linknil.il_to_nil("test",hilmod)
val _ = print "Test phase-split and closure-converted!\n"
val _ = Linknil.Ppnil.pp_module{module = nilmod,
				header = "test", name = "name", pass = "pass"}
val _ = print "\n\n"
