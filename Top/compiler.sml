(*$import Prelude TopLevel BinIO List TopHelp Int Name Time ListPair Real64 Listops Paths COMPILER LinkParse Il LinkIl Nil Linknil Rtl Linkrtl Linkalpha Linksparc Target FileCache Util Stats Delay *)

structure Compiler :> COMPILER =
struct

    val error = fn x => Util.error "compiler.sml" x

    val showWrittenContext = Stats.ff("ShowWrittenContext")
    val writeUnselfContext = Stats.ff("WriteUnselfContext")
    val showImports = Stats.ff("ShowImports")

    type il_module = LinkIl.module
    type nil_module = Nil.module
    type rtl_module = Rtl.module

    datatype import = DIRECT | INDIRECT

    fun importName DIRECT = "direct"
      | importName INDIRECT = "indirect"

    fun importFromName "direct" = DIRECT
      | importFromName "indirect" = INDIRECT
      | importFromName name = error ("unknown import name " ^ name)
	
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
	    val shortpctxt = Delay.delay (fn () => LinkIl.IlContext.UnselfifyPartialContext pctxt)
	    val _ = if (!writeUnselfContext)
			then (let val shortfile = Paths.ilToUnself file ^ ".unself"
			      in  writePartialContextRaw' (shortfile, Delay.force shortpctxt)
			      end)
		    else ()
	    val _ = if (!showWrittenContext)
			then (print "Selfified context:\n"; 
			      LinkIl.Ppil.pp_pcontext pctxt;
			      print "\n\n\nUnselfified context:\n";
			      LinkIl.Ppil.pp_pcontext (Delay.force shortpctxt))
		    else ()
	in  () 
	end
    
    val writePartialContextRaw = Stats.timer("WritingContext",writePartialContextRaw)
    structure IlCache = FileCache(type internal = Il.partial_context
				  val equaler = LinkIl.IlContextEq.eq_partial_context
				  val reader = readPartialContextRaw
				  val writer = writePartialContextRaw)

    fun chatImports imports =
	let fun isdirect (_, DIRECT) = true
	      | isdirect (_, INDIRECT) = false
	    val direct = map #1 (List.filter isdirect imports)
	    val indirect = map #1 (List.filter (not o isdirect) imports)
	in
	    Help.chat ("  [" ^ Int.toString (length direct) ^ " direct imports: ");
	    Help.chat_strings 20 direct;
	    Help.chat ("\n   " ^ Int.toString (length indirect) ^ " indirect imports: ");
	    Help.chat_strings 20 indirect;
	    Help.chat "]\n"
	end
	
    fun getContext imports =
	let val _ = if (!showImports) then chatImports imports else ()
	    val _ = Name.reset_varmap()
	    val _ = IlCache.tick()
	    val start = Time.now()
	    val (iscached,partial_ctxts) = ListPair.unzip (map (IlCache.read o #1) imports)
	    val diff = Time.toReal(Time.-(Time.now(), start))
	    val diff = (Real.realFloor(diff * 100.0)) / 100.0
	    fun folder ((_, INDIRECT), pctxt, acc) = LinkIl.IlContext.get_labels (pctxt, acc)
	      | folder (_, _, acc) = acc
	    val indirect_labels = Listops.foldl2 folder LinkIl.IlContext.empty_label_info (imports, partial_ctxts)
	    fun folder ((file, _), true, (c,u)) = (file::c, u)
	      | folder ((file, _), false, (c,u)) = (c,file::u)
	    val (cached, uncached) = Listops.foldl2 folder (nil,nil) (imports, iscached)
	    val cached_size = foldl (fn (ilFile,acc) => acc + (IlCache.size ilFile)) 0 cached
	    val uncached_size = foldl (fn (ilFile,acc) => acc + (IlCache.size ilFile)) 0 uncached
	    val _ = (Help.chat "  ["; Help.chat (Int.toString (length cached)); 
		     Help.chat " imports of total size "; Help.chat (Int.toString cached_size);
		     Help.chat " were cached.\n";
		     Help.chat "   "; Help.chat (Int.toString (length uncached));
		     Help.chat " imports of total size "; Help.chat (Int.toString uncached_size);
		     Help.chat " were uncached and took ";
		     Help.chat (Real.toString diff); Help.chat " seconds.";
		     if (!showImports) then (Help.chat "\n\nCACHED: "; Help.chat_strings 20 cached;
					     Help.chat "\n\nUNCACHED: "; Help.chat_strings 20 uncached;
					     Help.chat "\n\n")
		     else ();
		     Help.chat "]\n")
	    val initial_ctxt = LinkIl.initial_context()
	    val addContext = Stats.timer("AddingContext",LinkIl.plus_context)
	    val (partial_context_opts, context) = addContext (initial_ctxt, partial_ctxts)
	    val _ = Listops.app2 (fn (NONE,_) => false
	                           | (SOME new, (file,_)) => IlCache.updateCache(file,new))
		                 (partial_context_opts, imports)
	    val context = LinkIl.IlContext.obscure_labels (context, indirect_labels)
	    val _ = Help.chat ("  [Added contexts.]\n")
	in  (context, indirect_labels)
	end


    (* elaborate : {...} -> il_module * bool *)
    fun elaborate {unit, smlFile, intFile, targetIlFile, imports} =
	let val (ctxt, label_info) = getContext imports
	    val _ = Help.chat ("  [Parsing " ^ smlFile ^ "]\n")
	    val (lines,fp, _, dec) = LinkParse.parse_impl smlFile
	    val _ = if (lines > 3000) (* XXX: reconsider *)
			then (Help.chat "  [Large file: ";
			      Help.chat (Int.toString lines);
			      Help.chat " lines.   Flushing file cache.]\n";
			      IlCache.flushAll())
		    else ()
	    (* Elaborate source file *)
	    val il_module_opt =
		case intFile
		  of SOME intFile' =>
		      let val (_,fp2, _, specs) = LinkParse.parse_inter intFile'
			  val _ = Help.chat ("  [Warning: constraints currently coerce.  ")
			  val _ = Help.chat ("Not compatible with our notion of freshness.]\n")
			  val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " with constraint]\n"  )
		      in  LinkIl.elab_dec_constrained(unit,ctxt,fp,dec,fp2,specs)
		      end
		   | NONE => 
		      let val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " non-constrained]\n")
		      in  LinkIl.elab_dec(unit,ctxt,fp,dec)
		      end
	    val il_module =
		case il_module_opt
		  of NONE => error("File " ^ smlFile ^ " failed to elaborate.")
		   | SOME ilmodule' => ilmodule'
	    val il_module =
		let val (_, partial_ctxt, binds) = il_module
		    val reduced_ctxt = Stats.timer("GCContext",LinkIl.IlContext.gc_context) il_module
		in  (reduced_ctxt, partial_ctxt, binds)
		end
	    val il_module = LinkIl.IlContext.unobscure_labels (il_module, label_info)
	    (* Update il file *)
	    val _ = IlCache.flushSome [targetIlFile]
	    val (_, partial_ctxt, _) = il_module
	    val partial_ctxt_export = LinkIl.IlContext.removeNonExport partial_ctxt
	    val _ = Help.chat ("  [writing " ^ targetIlFile)
	    val written = IlCache.write (targetIlFile, partial_ctxt_export)
	    val _ = if written then ()
		    else Help.chat " - unnecessary"
	    val _ = Help.chat "]\n"
	in  (il_module, written)
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
