(*$import COMPILER LinkIl Linknil Linkrtl Linkalpha Linksparc Target FileCache Util Stats Delay *)

structure Compiler :> COMPILER =
struct

    val error = fn x => Util.error "compiler.sml" x

    val showWrittenContext = Stats.ff("ShowWrittenContext")
    val writeUnselfContext = Stats.ff("WriteUnselfContext")

    type il_module = LinkIl.module
    type nil_module = Nil.module
    type rtl_module = Rtl.module

    fun readPartialContextRaw file = 
	let 
(*	    val _ = print ("XXX reading context file " ^ file ^ "\n") *)
	    val is = BinIO.openIn file
	    val res = LinkIl.IlContextEq.blastInPartialContext is
	    val _ = BinIO.closeIn is
(*	    val _ = print ("XXX done reading context file " ^ file ^ "\n") *)
	in  res
	end
    val readPartialContextRaw = Stats.timer("ReadingContext",readPartialContextRaw)

    fun writePartialContextRaw' (file,pctxt) =
	let val os = BinIO.openOut file
	    val _ = LinkIl.IlContextEq.blastOutPartialContext os pctxt
	    val _ = BinIO.closeOut os
	in
	    ()
	end
    
    fun writePartialContextRaw (file,pctxt) = 
	let val _ = writePartialContextRaw' (file,pctxt)
	    val shortpctxt = Delay.delay (fn () => IlContext.UnselfifyPartialContext pctxt)
	    val _ = if (!writeUnselfContext)
			then (let val shortfile = Paths.ilToUnself file ^ ".unself"
			      in  writePartialContextRaw' (shortfile, Delay.force shortpctxt)
			      end)
		    else ()
	    val _ = if (!showWrittenContext)
			then (print "Selfified context:\n"; 
			      Ppil.pp_pcontext pctxt;
			      print "\n\n\nUnselfified context:\n";
			      Ppil.pp_pcontext (Delay.force shortpctxt))
		    else ()
	in  () 
	end
    
    val writePartialContextRaw = Stats.timer("WritingContext",writePartialContextRaw)
    structure IlCache = FileCache(type internal = Il.partial_context
				  val equaler = LinkIl.IlContextEq.eq_partial_context
				  val reader = readPartialContextRaw
				  val writer = writePartialContextRaw)
    fun getContext uifiles =
	let val _ = Name.reset_varmap()
	    val _ = IlCache.tick()
	    val start = Time.now()
	    val isCached_ctxts = map IlCache.read uifiles
	    val diff = Time.toReal(Time.-(Time.now(), start))
	    val diff = (Real.realFloor(diff * 100.0)) / 100.0
	    val partial_ctxts = map #2 isCached_ctxts
	    val (cached_temp,uncached_temp) = (List.partition (fn (imp,(isCached,_)) => isCached)
					       (Listops.zip uifiles isCached_ctxts))
	    val cached = map #1 cached_temp
	    val uncached = map #1 uncached_temp
	    val cached_size = foldl (fn (uifile,acc) => acc + (IlCache.size uifile)) 0 cached
	    val uncached_size = foldl (fn (uifile,acc) => acc + (IlCache.size uifile)) 0 uncached
	    val _ = (Help.chat "  ["; Help.chat (Int.toString (length cached)); 
		     Help.chat " imports of total size "; Help.chat (Int.toString cached_size);
		     Help.chat " were cached.\n";
		     Help.chat "   "; Help.chat (Int.toString (length uncached));
		     Help.chat " imports of total size "; Help.chat (Int.toString uncached_size);
		     Help.chat " were uncached and took ";
		     Help.chat (Real.toString diff); Help.chat " seconds.";
(*		     Help.chat "\n\nCACHED: "; Help.chat_strings 20 cached;
		     Help.chat "\n\nUNCACHED: "; Help.chat_strings 20 uncached;
*)		     Help.chat "]\n")
	    val initial_ctxt = LinkIl.initial_context()
	    val addContext = Stats.timer("AddingContext",LinkIl.plus_context)
	    val (partial_context_opts, context) = addContext (initial_ctxt, partial_ctxts)
	    val _ = Listops.map2 (fn (NONE,_) => false
	                           | (SOME new, file) => IlCache.updateCache(file,new))
		        (partial_context_opts, uifiles)
	    val _ = Help.chat ("  [Added contexts.]\n")
	in  context
	end


    fun elab_constrained(unit,ctxt,sourcefile,fp,dec,fp2,specs,uiFile,least_new_time) =
	(case LinkIl.elab_dec_constrained(ctxt, fp, dec, fp2,specs) of
	     SOME (il_module as (ctxt, partial_ctxt, binds)) =>
		 let val partial_ctxt_export = IlContext.removeNonExport partial_ctxt
		     val _ = Help.chat ("  [writing " ^ uiFile)
		     val written = IlCache.write (uiFile, partial_ctxt_export)
		     val _ = Help.chat "]\n"
		     val reduced_ctxt = Stats.timer("GCContext",LinkIl.IlContext.gc_context) il_module 
		     val il_module = (reduced_ctxt, partial_ctxt, binds)
		 in  (il_module, written)
		 end
	   | NONE => error("File " ^ sourcefile ^ " failed to elaborate."))
    
    fun elab_nonconstrained(unit,pre_ctxt,sourcefile,fp,dec,uiFile,least_new_time) =
	case LinkIl.elab_dec(pre_ctxt, fp, dec) of
	    SOME (il_module as (ctxt, partial_ctxt, binds)) =>
		let val partial_ctxt_export = IlContext.removeNonExport partial_ctxt
		    val _ = Help.chat ("  [writing " ^ uiFile)
		    val written = IlCache.write (uiFile, partial_ctxt_export)
		    val _ = if written then ()
			    else Help.chat " - unnecessary"
		    val _ = Help.chat "]\n"
		    val reduced_ctxt = Stats.timer("GCContext",LinkIl.IlContext.gc_context) il_module 
		    val il_module = (reduced_ctxt, partial_ctxt, binds)
		in  (il_module, written)
		end
	  | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")

    (* elaborate : {...} -> il_module * bool *)
    fun elaborate {unit, smlFile, intFile, targetIlFile, importIlFiles} =
	let val ctxt = getContext importIlFiles
	    val _ = Help.chat ("  [Parsing " ^ smlFile ^ "]\n")
	    val (lines,fp, _, dec) = LinkParse.parse_impl smlFile
	    val _ = if (lines > 3000) (* XXX: reconsider *)
			then (Help.chat "  [Large file: ";
			      Help.chat (Int.toString lines);
			      Help.chat " lines.   Flushing file cache.]\n";
			      IlCache.flushAll())
		    else ()
	    (* Elaborate the source file, generating a .ui file *)
	    val _ = IlCache.flushSome [targetIlFile]
	in  case intFile
	      of SOME intFile' =>
		  let val (_,fp2, _, specs) = LinkParse.parse_inter intFile'
		      val _ = Help.chat ("  [Warning: constraints currently coerce.  ")
		      val _ = Help.chat ("Not compatible with our notion of freshness.]\n")
		      val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " with constraint]\n"  )
		  in elab_constrained(unit,ctxt,smlFile,fp,dec,fp2,specs,targetIlFile,Time.zeroTime)
		  end
	       | NONE => 
		  let val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " non-constrained]\n")
		  in elab_nonconstrained(unit,ctxt,smlFile,fp,dec,targetIlFile,Time.zeroTime)
		  end
	end
    
    (* il_to_nil : string * il_module -> nil_module *)
    val il_to_nil = Linknil.il_to_nil

    (* nil_to_rtl : string * nil_module -> rtl_module *)
    val nil_to_rtl = Linkrtl.nil_to_rtl

    (* rtl_to_asm : string * rtl_module -> unit *)
    fun rtl_to_asm arg =
	let val rtl_to_asm = case Target.getTargetPlatform ()
			       of Target.TIL_ALPHA => Linkalpha.rtl_to_asm
				| Target.TIL_SPARC => Linksparc.rtl_to_asm
	    val mainLabel = rtl_to_asm arg
	in  ()
	end

    (* link : string * string list -> unit *)
    fun link (asmFile, units) =
	let val link = (case Target.getTargetPlatform()
			  of Target.TIL_ALPHA => Linkalpha.link
			   | Target.TIL_SPARC => Linksparc.link)
	    val local_labels = map (fn un => Rtl.ML_EXTERN_LABEL (un ^ "_unit")) units
	    val ignoredLabel = link (asmFile, local_labels)
	in  ()
	end
	
end
