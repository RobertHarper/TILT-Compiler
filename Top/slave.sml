(*$import SLAVE FileCache TopHelp Communication LinkParse LinkIl Compiler Linker OS List SplayMapFn SplaySetFn Platform Dirs Delay *)

(* This is a slave step which gives a result:

     Is there a message from the master?
     (A) Yes, fetch the job j from from the channel. Compile j.  Send acknowledgement. 
         This result in a WORK j.
     (B) No, Is there a message from me to the master?
         (1) Yes, do nothing.  Master has yet to consume this message. Result is WAIT.
         (2) No, send ready message.  Result is READY.

*)



structure Slave :> SLAVE =
struct
    open Help
    val error = fn s => Util.error "manager.sml" s
    val stat_each_file = Stats.ff("TimeEachFile")
    val showWrittenContext = Stats.ff("ShowWrittenContext")
    val writeUnselfContext = Stats.ff("WriteUnselfContext")

    structure Comm = Comm(val slaveTidOpt = NONE)

    datatype result = WORK of string | WAIT | READY
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
			then (let val shortfile = file ^ ".unself"
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
    structure Cache = FileCache(type internal = Il.partial_context
				(* val equaler = LinkIl.IlContextEq.eq_partial_context *)
				val equaler = (fn _ => false)
				val reader = readPartialContextRaw
				val writer = writePartialContextRaw)
    fun getContext uifiles =
	let 
	    val _ = Name.reset_varmap()
	    val _ = Cache.tick()
	    val start = Time.now()
	    val isCached_ctxts = map Cache.read uifiles
	    val diff = Time.toReal(Time.-(Time.now(), start))
	    val diff = (Real.realFloor(diff * 100.0)) / 100.0
	    val partial_ctxts = map #2 isCached_ctxts
	    val (cached_temp,uncached_temp) = (List.partition (fn (imp,(isCached,_)) => isCached)
					       (Listops.zip uifiles isCached_ctxts))
	    val cached = map #1 cached_temp
	    val uncached = map #1 uncached_temp
	    val cached_size = foldl (fn (uifile,acc) => acc + (Cache.size uifile)) 0 cached
	    val uncached_size = foldl (fn (uifile,acc) => acc + (Cache.size uifile)) 0 uncached
	    val _ = (chat "  ["; chat (Int.toString (length cached)); 
		     chat " imports of total size "; chat (Int.toString cached_size); chat " were cached.\n";
		     chat "   "; chat (Int.toString (length uncached)); chat " imports of total size ";
		     chat (Int.toString uncached_size);  chat " were uncached and took ";
		     chat (Real.toString diff); chat " seconds.";
(*		     chat "\n\nCACHED: "; chat_strings 20 cached;
		     chat "\n\nUNCACHED: "; chat_strings 20 uncached;
*)		     chat "]\n")
	    val initial_ctxt = LinkIl.initial_context()
	    val addContext = Stats.timer("AddingContext",LinkIl.plus_context)
	    val (partial_context_opts, context) = addContext (initial_ctxt, partial_ctxts)
	    val _ = Listops.map2 (fn (NONE,_) => false
	                           | (SOME new, file) => Cache.updateCache(file,new))
		        (partial_context_opts, uifiles)
	    val _ = chat ("  [Added contexts.]\n")
	in  context
	end


    fun elab_constrained(unit,ctxt,sourcefile,fp,dec,fp2,specs,uiFile,least_new_time) =
	(case LinkIl.elab_dec_constrained(ctxt, fp, dec, fp2,specs) of
	     SOME (il_module as (ctxt, partial_ctxt, binds)) =>
		 let val partial_ctxt_export = IlContext.removeNonExport partial_ctxt
		     val _ = (Help.chat ("  [writing " ^ uiFile);
			      Cache.write (uiFile, partial_ctxt_export);
			      Help.chat "]\n")
		     val reduced_ctxt = Stats.timer("GCContext",LinkIl.IlContext.gc_context) il_module 
		     val il_module = (reduced_ctxt, partial_ctxt, binds)
		 in il_module
		 end
	   | NONE => error("File " ^ sourcefile ^ " failed to elaborate."))
    
    fun elab_nonconstrained(unit,pre_ctxt,sourcefile,fp,dec,uiFile,least_new_time) =
	case LinkIl.elab_dec(pre_ctxt, fp, dec) of
	    SOME (il_module as (ctxt, partial_ctxt, binds)) =>
		let val partial_ctxt_export = IlContext.removeNonExport partial_ctxt
		    val _ = (Help.chat ("  [writing " ^ uiFile);
			     Cache.write (uiFile, partial_ctxt_export);
			     Help.chat "]\n")
		     val reduced_ctxt = Stats.timer("GCContext",LinkIl.IlContext.gc_context) il_module 
		     val il_module = (reduced_ctxt, partial_ctxt, binds)
		in il_module
		end
	  | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")

    (* true indicates .o file generated, false means only .s file generated *)
    fun compile (unit,absBase,importAbsBases) = 
	let val intFile = Help.base2int absBase
	    val smlFile = Help.base2sml absBase
	    val uiFile = Help.base2ui absBase

	    fun elaborate()=  
		let val ctxt = getContext (map Help.base2ui importAbsBases)
		    val _ = Help.chat ("  [Parsing " ^ smlFile ^ "]\n")
		    val (lines,fp, _, dec) = LinkParse.parse_impl smlFile
		    val _ = if (lines > 3000) 
				then (chat "  [Large file: ";
				      chat (Int.toString lines);
				      chat " lines.   Flushing file cache.]\n";
				      Cache.flushAll())
			    else ()
		    (* Elaborate the source file, generating a .ui file *)
		    val _ = Cache.flushSome [uiFile]
		    val il_module = 
			if Cache.exists intFile then
			    let val (_,fp2, _, specs) = LinkParse.parse_inter intFile
				val _ = Help.chat ("  [Warning: constraints currently coerce.  ")
				val _ = Help.chat ("Not compatiable with our notion of freshness.]\n")
				val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " with constraint]\n"  )
			    in elab_constrained(unit,ctxt,smlFile,fp,dec,fp2,specs,uiFile,Time.zeroTime)
			    end
			else let val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " non-constrained]\n")
			     in elab_nonconstrained(unit,ctxt,smlFile,fp,dec,uiFile,Time.zeroTime)
			     end
		    (* Generate a .uo file that matches up .ui files and theirs imports *)
		    val _ = Help.chat ("  [Creating .uo file ...")
		    val imports_uo = map (fn impAbsBase => 
					  (impAbsBase, Cache.crc (Help.base2ui impAbsBase))) 
			importAbsBases
		    val exports_uo = [(absBase, Cache.crc uiFile)]
		    val uoFile = Linker.mk_uo {imports = imports_uo,
					       exports = exports_uo,
					       base_result = absBase}
		    val _ = Cache.flushSome [uoFile]
		in  il_module
		end
	    fun generate il_module =
		let (* Continue compilation, generating a platform-dependent .o object file *)
		    val _ = Help.chat ("  [Compiling to assembly file]\n")
		    val sFile = Til.il_to_asm (unit, absBase, il_module)
		in  ()
		end
	in  (elaborate, generate)
	end

    fun assemble (unit,base,importBases) = 
	let val sFile = Help.base2ui base
	    val oFile = Help.base2o base
            val _ = if (Cache.exists sFile)
			then ()
		    else Platform.sleep 0.5
	    val _ = Help.chat ("  [Assembling to object file ...")
	    val _ = Til.assemble base;
	    val _ =  Help.chat "]\n"
	    val _ = Cache.flushSome [oFile]
(*
	    val _ = if (!stat_each_file)
			then (OS.Process.system ("size " ^ oFile ^ " &"); ())
		    else ()
*)
	    val _ = Help.chat "]\n"
	in  ()
	end

    fun setup () = (chat "Starting slave.\n";
		    Cache.flushAll())


    fun step () = 
	(case Comm.receive Comm.fromMaster of
	     NONE => if (Comm.exists Comm.toMaster)
			 then WAIT
		     else (Comm.send (Comm.toMaster, Comm.READY); READY)
	   | SOME Comm.READY => error "Slave got a ready message"
	   | SOME (Comm.ACK_INTERFACE _) => error "Slave got an ack_interface message"
	   | SOME (Comm.ACK_OBJECT _) => error "Slave got an ack_object message"
	   | SOME (Comm.ACK_ERROR _) => error "Slave got an ack_error message"
	   | SOME (Comm.FLUSH flags)=> (Cache.flushAll(); chat "Slave received FLUSH.\n"; 
					Comm.doFlags flags;
					READY)
	   | SOME (Comm.REQUEST (job as (platform::unit::absBase::absImportBases))) => 
		       (* It's okay for the first acknowledgement to be missed. 
			  In fact, we skip the acknowledgement if the expected compilation 
			  time was small to avoid communication traffic. *)
		       let 
			   val start = Time.now()
			   val _ = Til.setTargetPlatform
			                 (case platform of
					      "dunix" => Til.TIL_ALPHA
					    | "solaris" => Til.TIL_SPARC
					    | _ => error ("unknown target platform" ^ platform))
			   val (elaborate, generate) = compile (unit,absBase,absImportBases)
			   val il_module = (elaborate()
					    handle e => 
						(Comm.send(Comm.toMaster, Comm.ACK_ERROR job);
						 raise e))
			   val diff = Time.-(Time.now(), start)
			   val _ = if (Time.toReal diff > 0.5)
				       then (if (!chat_verbose)
						 then (chat "Sending ACK_INTERFACE: interface took ";
						       chat (Time.toString diff);
						       chat " seconds \n")
					     else ();
					     Comm.send (Comm.toMaster, Comm.ACK_INTERFACE job))
				   else ()
			   val _ = (generate il_module
				    handle e => 
					(Comm.send(Comm.toMaster, Comm.ACK_ERROR job);
					 raise e))
			   val _ = 
			       if (Til.native())
				   then
				       (assemble(unit,absBase,absImportBases);
					Comm.send (Comm.toMaster, Comm.ACK_OBJECT job))
			       else
				   Comm.send (Comm.toMaster, Comm.ACK_ASSEMBLY job)

			   val _ = if (!chat_ref andalso !stat_each_file)
				       then (Stats.print_timers();
					     Stats.clear_stats())
				   else ()

		       in  WORK unit
		       end
	   | SOME (Comm.REQUEST _) => error "Slave got a funny request")

    fun run() = 
	let val _ = setup()
	    val lastState = ref READY
	    val lastTime = ref (Time.now())
	    fun loop() = 
		let val prevState = !lastState
		    val curState = step()
		    val _ = lastState := curState
		in  (case curState of
			 WORK job => chat ("Slave compiled " ^ job ^ "\n")
		       | _ => (* READY or WAIT *)
			     let val curTime = Time.now()
				 val diff = Time.toReal(Time.-(curTime, !lastTime))
				 val _ = if (diff > 10.0)
					     then (chat ("Slave waiting for master to send work.\n");
						   lastTime := curTime)
					 else ()
			     in	 Platform.sleep 0.1
			     end);
		    loop()
		end
	in loop()
	end

end

