(*$import Prelude TopLevel BinIO TopHelp Int Name Time ListPair Real Listops Paths COMPILER LinkParse Il LinkIl Nil Linknil Rtl Linkrtl Linkalpha Linksparc Target FileCache Util Stats Delay Paths *)

structure Compiler :> COMPILER =
struct

    val error = fn x => Util.error "compiler.sml" x

    val showWrittenContext = Stats.ff("ShowWrittenContext")
    val writeUnselfContext = Stats.ff("WriteUnselfContext")
    val showImports = Stats.ff("ShowImports")

    type unit_paths = Paths.unit_paths
    type il_module = LinkIl.module
    type nil_module = Nil.module
    type rtl_module = Rtl.module

    datatype kind =
	DIRECT				(* direct import; labels available *)
      | INDIRECT			(* indirect import; labels hidden *)

    datatype import =
	FILE of unit_paths * kind
      | PRIM of kind

    fun ilFile (FILE (p,_)) = SOME (Paths.ilFile p)
      | ilFile _ = NONE
	
    fun readPartialContextRaw ilFile = 
	let 
(*	    val _ = print ("XXX reading context file " ^ ilFile ^ "\n") *)
	    val is = BinIO.openIn ilFile
	    val res = LinkIl.IlContextEq.blastInPartialContext is
	    val _ = BinIO.closeIn is
(*	    val _ = print ("XXX done reading context file " ^ ilFile ^ "\n") *)
	in  res
	end
    val readPartialContextRaw = Stats.timer("ReadingContext",readPartialContextRaw)

    fun writePartialContextRaw' (ilFile,pctxt) =
	let val os = BinIO.openOut ilFile
	    val _ = LinkIl.IlContextEq.blastOutPartialContext os pctxt
	    val _ = BinIO.closeOut os
	in
	    ()
	end
    
    fun writePartialContextRaw (ilFile,pctxt) = 
	let val _ = writePartialContextRaw' (ilFile,pctxt)
	    val shortpctxt = Delay.delay (fn () => LinkIl.IlContext.UnselfifyPartialContext pctxt)
	    val _ = if (!writeUnselfContext)
			then (let val shortfile = Paths.ilToUnself ilFile ^ ".unself"
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
	let
	    fun kind (DIRECT, name) = name
	      | kind (INDIRECT, name) = "-" ^ name
		
	    fun toString (FILE (p,k)) = kind (k, Paths.unitName p)
	      | toString (PRIM k) = kind (k, "(PRIM)")
	in
	    Help.chat ("  [" ^ Int.toString (length imports) ^ " imports: ");
	    Help.chat_strings 20 (map toString imports);
	    Help.chat "]\n"
	end

	local
	    val saved = ref (NONE : Il.partial_context option)
	in
	    fun tiltprim context =
		(case !saved
		   of NONE =>
		       let val pctxt = LinkIl.tiltprim context
			   val _ = saved := SOME pctxt
		       in  pctxt
		       end
		    | SOME pctxt => pctxt)
	    fun updateTiltprim pctxt = saved := SOME pctxt
	end
    
    fun getContext imports =
	let val _ = if (!showImports) then chatImports imports else ()
	    val _ = Name.reset_varmap()
	    val _ = IlCache.tick()
	    val start = Time.now()
	    val plus_context = Stats.timer("AddingContext",LinkIl.plus_context)
	    fun folder (FILE (p,k), (cached, uncached, context, label_info)) =
		let val ilFile = Paths.ilFile p
		    val (iscached,pctxt) = IlCache.read ilFile
		    val (cached,uncached) = if iscached then (ilFile :: cached, uncached)
					    else (cached, ilFile :: uncached)
		    val (pctxtopt, context) = plus_context (context, pctxt)
		    val _ = (case pctxtopt
			       of SOME new => ignore (IlCache.updateCache (ilFile, new))
				| NONE => ())
		    val label_info = (case k
					of INDIRECT => LinkIl.IlContext.get_labels (pctxt, label_info)
					 | DIRECT => label_info)
		in  (cached, uncached, context, label_info)
		end
	      | folder (PRIM k, (cached, uncached, context, label_info)) =
		let val pctxt = tiltprim context
		    val (pctxtopt, context) = plus_context (context, pctxt)
		    val _ = (case pctxtopt
			       of SOME new => updateTiltprim new
				| NONE => ())
		    val label_info = (case k
					of INDIRECT => LinkIl.IlContext.get_labels (pctxt, label_info)
					 | DIRECT => label_info)
		in  (cached, uncached, context, label_info)
		end
	    val init = (nil, nil, LinkIl.empty_context, LinkIl.IlContext.empty_label_info)
	    val (cached, uncached, context, indirect_labels) = foldl folder init imports
	    val context = LinkIl.IlContext.obscure_labels (context, indirect_labels)
	    val totalSize = foldl (fn (ilFile,acc) => acc + (IlCache.size ilFile)) 0
	    val chatInt = Help.chat o Int.toString
	    val _ = (Help.chat "  ["; chatInt (length cached); 
		     Help.chat " imports of total size "; chatInt (totalSize cached);
		     Help.chat " were cached.\n";
		     Help.chat "   "; chatInt (length uncached);
		     Help.chat " imports of total size "; chatInt (totalSize uncached);
		     Help.chat " were uncached.";
		     if (!showImports) then (Help.chat "\n\nCACHED: "; Help.chat_strings 20 cached;
					     Help.chat "\n\nUNCACHED: "; Help.chat_strings 20 uncached;
					     Help.chat "\n\n")
		     else ();
		     Help.chat "]\n")
	    val diff = Time.toReal(Time.-(Time.now(), start))
	    val diff = (Real.realFloor(diff * 100.0)) / 100.0
	    val _ = (Help.chat "  [Elaboration context took ";
		     Help.chat (Real.toString diff);
		     Help.chat " seconds.]\n")
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
