(*$import MANAGER LinkParse LinkIl Compiler Linker MakeDep OS List SplayMapFn SplaySetFn Platform *)


structure Communication :> COMMUNICATION = 
struct
    type job = string list
    val delimiter = #"|"
    val start = "!START!"
    val stop = "!STOP!"

    fun namepid2ID (name,pid) = name ^ "." ^ (Int.toString pid)
    val SOME selfName = OS.Process.getEnv "HOST"
    val selfPid = Word32.toInt(Platform.pid())
    val selfID = namepid2ID(selfName, selfPid)
    fun channel(from, to) = to ^ "<-" ^ from 

    (* --- Asynchronously send the message on the channel --- *)
    fun send (channel, strings) = 
	let fun loop [] = ""
	      | loop [str] = str
	      | loop (str::rest) = str ^ (String.str delimiter) ^ (loop rest)
	    val message = loop strings
	    val temp = "TEMP" ^ channel
	    val _ = if (OS.FileSys.access(temp, [OS.FileSys.A_READ]))
			then OS.FileSys.remove temp
		    else ()
	    val fd = TextIO.openAppend temp
	    val _ = TextIO.output(fd, message)
	    val _ = TextIO.closeOut fd
	    val _ = OS.FileSys.rename{old=temp, new=channel}
	in  ()
	end

    (* --- Asynchronously look for a message on the channel --- *)
    fun receive channel : job option =
	if (OS.FileSys.access(channel,[])) 
	    then let val fd = TextIO.openIn channel
		     fun loop acc = if (TextIO.endOfStream fd)
					then acc
				    else loop (acc ^ (TextIO.inputAll fd))
		     val message = loop ""
		     val _ = TextIO.closeIn fd
		     val _ = OS.FileSys.remove channel
		     fun loop words curWord [] = rev ((implode (rev curWord)) :: words)
		       | loop words curWord ((c : char)::crest) = 
			 if (c = delimiter)
			     then loop ((implode (rev curWord)) :: words) [] crest
			 else loop words (c :: curWord) crest
		     val words = loop [] [] (explode message)
		 in  SOME words
		 end
	else NONE

    fun channelReady channel = not (OS.FileSys.access(channel,[]))

    (* Non-blocking acknowledgement and blocking request for jobs *)
    fun acknowledge (completed : job option) = 
	let val masterID = "master"
	    val slaveID = selfID
	    val ch = channel(slaveID, masterID)
	    (* Acknowledge job completion and request more if acknowledgement processed *)
	in (case completed of
		SOME msg => send(ch, msg)
	      | NONE => send(ch, [start]))
	end

    fun request () =
	let val masterID = "master"
	    val slaveID = selfID
	    val ch = channel(masterID, slaveID)
	in  (* Look for available jobs *)
	    receive ch
	end


    (* Master looks for willing slaves - this call does not block 
       Slaves are represented by an optional string (indicating the file it just compiled)
          and an invoking function to cause the slave to continue compiling.
    *)
    fun findSlaves () =
	let val files = 
	      let val dirstream = OS.FileSys.openDir "."
		  fun loop acc = let val cur = OS.FileSys.readDir dirstream
				 in  if (cur = "")
					 then (OS.FileSys.closeDir dirstream; acc)
				     else loop (cur :: acc)
				 end
	      in  loop []
	      end
	    val masterID = "master"
	    val len = size masterID
	    val channels = List.filter (fn s => (size s > len) andalso
					        (String.substring(s,0,len) = masterID)) files
	    fun apper channel = 
		let val SOME message = receive channel
		    val message = (case message of
				       [temp] => if (temp = start) then NONE else SOME message
				     | _ => SOME message)
		    val tmp = size "master<-"
		    val slaveName = String.substring(channel,tmp,(size channel) - tmp)
		    val revChannel = slaveName ^ "<-master"
		    fun invoke NONE = send(revChannel, [stop])
		      | invoke (SOME words) = send(revChannel, words)
		in  (slaveName, message, invoke)
		end
	in  map apper channels
	end

    fun masterTest() = 
	let val max = 5
	    val current = ref 0
	    val completed = ref 0
	    fun nextJob invoke = (current := (!current) + 1;
				  if (!current > max) 
				      then invoke NONE
				  else (print "Issuing "; print (Int.toString (!current)); print "\n";
					invoke (SOME [(Int.toString(!current))]));
				  if (!current = max) then print "Done Issuing\n" else ())
	    fun loop () = 
		let val slaves = findSlaves()
		in  app (fn (slaveName, done, invoke) => 
			 ((case done of
			      NONE => ()
			    | SOME str => (completed := (!completed) + 1;
					   print ("  Slave " ^ slaveName ^ 
						  " completed " ^ (String.concat str) ^ "\n")));
			       nextJob invoke)) slaves;
		    if (!completed < max) then loop() else (print "Master done\n"; ())
		end
	in  loop()
	end

    fun slaveTest() = 
	let val _ = acknowledge NONE
	    fun loop () = 
		(case (request()) of
		     NONE => print "Slave done\n"
		   | SOME msg => (print "Received "; app print msg; print "\n";  
				  Platform.sleep 1.0; acknowledge (SOME msg);
				  print "Processed "; app print msg; print "\n";  
				  loop ()))
	in  loop ()
	end
end


structure Help :> HELP = 
struct
    val stat_each_file = Stats.ff("TimeEachFile")
    val cache_context = ref 5
    val error = fn s => Util.error "pmanager.sml" s
    val stat_each_file = Stats.ff("TimeEachFile")
    val cache_context = ref 5
    val stop_early_compiling_sml_to_ui = ref false
    val eager = ref true

    (* ---- Some diagnostic message helper functions ---- *)
    val chat_ref = Stats.tt("ManagerChat")
    fun chat s = if !chat_ref then (print s; TextIO.flushOut TextIO.stdOut)
		 else ()
    fun chat_strings skip imports =
	let fun f(str,acc) = let val acc = if acc > 80
						then (chat "\n        "; 8)
					    else acc + (size str) + 2
			     in  chat str; chat "  "; acc
			     end
	in  if (!chat_ref) then foldl f skip imports else 0
	end


    (* ---- we want to do lookup on strings ----- *)
    local
	structure StringKey = 
	    struct
		type ord_key = string
		val compare = String.compare
	    end
    in  structure StringMap = SplayMapFn(StringKey)
	structure StringSet = SplaySetFn(StringKey)
    end

    (* ---- memoize the result of getting file attributes ---- *)
    local
	datatype stat = ABSENT 
	              | PRESENT of Time.time
	    
	val stats = ref (StringMap.empty : stat StringMap.map)
	    
	fun fetch_stat file =
	    let val exists = 
		((OS.FileSys.access(file, []) andalso
		  OS.FileSys.access(file, [OS.FileSys.A_READ]))
		 handle _ => (print ("Warning: OS.FileSys.access " ^ 
				     file ^ "\n"); false))
	    in  if exists
		    then (PRESENT(OS.FileSys.modTime file)
			  handle _ => (print "Warning: OS.FileSys.modTime raised exception\n"; ABSENT))
		else ABSENT
	    end
	
	fun fetch file = 
	    (case StringMap.find(!stats,file) of
		 NONE => let val stat = fetch_stat file
		       in  (stats := (StringMap.insert(!stats,file,stat));
                            stat)
			 end
	       | SOME stat => stat)
    in
	fun reset_stats() = (stats := StringMap.empty)
	    
	fun forget_stat file = 
            (stats := #1 (StringMap.remove(!stats, file))
	     handle _ => ())
	    
	fun exists file = (case fetch file of
			       ABSENT => ((*print (file ^ " does not exist\n");*)
					  false)
			     | PRESENT _ => ((*print (file ^ " exists\n");*)
					     true))
	    
	fun modTime file = (case fetch file of
				ABSENT => error ("Getting modTime on non-existent file " ^ file)
			      | PRESENT t => t)
	    
    end

    type unitname = string
    type filebase = string
    fun base2sml (f : string) = f ^ ".sml"
    fun base2int (f : string) = f ^ ".int"
    val base2ui = Til.base2ui
    val base2s = Til.base2s
    val base2o = Til.base2o
    val base2uo = Til.base2uo

    fun split_line dropper line = 
	let val fields = String.fields Char.isSpace line
	    fun filter [] = []
	      | filter (x::y) = if (size x = 0)
				    then filter y
				else if dropper x 
					 then []
				     else
					 x :: (filter y)
	in 	filter fields
	end
    fun parse_depend depend_str failure file =
      let val ins = TextIO.openIn file
	  val line = TextIO.inputLine ins
	  val sz = size line
	  val _ = TextIO.closeIn ins
	  val depend_str_sz = size depend_str
      in  if (sz >= depend_str_sz andalso 
              String.substring(line,0,depend_str_sz) = depend_str) then
	     let 
                 fun dropper s = ("(*"; s = "*)")
	     in  
                split_line dropper 
                      (String.substring(line,depend_str_sz,sz-depend_str_sz))
             end
	  else 
            (print ("Warning: first line of " ^ file ^ 
                    " is not import/include.\n");
  	     print "Calling parser to process.\n";
             failure file)
      end
    fun parse_impl_import file = 
	parse_depend "(*$import" (#3 o LinkParse.parse_impl) file
    fun parse_inter_include file = 
	parse_depend "(*$include" (#3 o LinkParse.parse_inter) file
    val _ = "*)*)"

    fun readContextRaw file = 
	let val is = BinIO.openIn file
	    val res = LinkIl.IlContextEq.blastInContext is
	    val _ = BinIO.closeIn is
	in  res
	end
    val readContextRaw = Stats.timer("ReadingContext",readContextRaw)

    fun writeContextRaw (file,ctxt) = 
	let val os = BinIO.openOut file
	    val _ = LinkIl.IlContextEq.blastOutContext os ctxt
	    val _ = BinIO.closeOut os
	in  () 
	end
    val writeContextRaw = Stats.timer("WritingContext",writeContextRaw)
    val addContext = Stats.timer("AddingContext",LinkIl.plus_context)

    datatype status = WAITING | READY | PENDING | DONE
    local
	datatype unitinfo = 
	    UNIT of {position : int,
		     filebase : string,
		     imports_direct  : string list option ref,
		     imports_transitive : string list option ref,
		     dependents_direct : string list ref,
		     context : (int * LinkIl.context) option ref,
		     status : status ref,
		     ui_crc : Crc.crc option ref}
	    
        val units = ref ([] : unitname list)
	val mapping = ref (StringMap.empty : unitinfo StringMap.map)
	val mapfileInfo = ref NONE

	fun find_unit unitname = StringMap.find(!mapping,unitname)
	fun lookup unitname =
	    (case (find_unit unitname) of
		 NONE => error ("unit " ^ unitname ^ " missing")
	       | SOME entry => entry)
	fun get_context unit = 
            let val UNIT{context,...} = lookup unit
            in context 
	    end
 
    in 
	fun list_units() = !units
	fun get_ui_crc unit = 
            let val UNIT{filebase,ui_crc,...} = lookup unit
            in  (case !ui_crc of
		     SOME crc => crc
		   | NONE => let val uiFile = base2ui filebase
				 val crc = Crc.crc_of_file uiFile
				 val _ = ui_crc := SOME crc
			     in  crc
			     end)
	    end
	fun get_base unit = 
            let val UNIT{filebase,...} = lookup unit
            in filebase 
	    end
	fun get_position unit = 
            let val UNIT{position,...} = lookup unit
            in  position
	    end
	fun get_import_direct unit = 
            let val UNIT{imports_direct = ref (SOME res),...} = lookup unit
            in  res
	    end
	fun get_import_transitive unit = 
            let val UNIT{imports_transitive = ref (SOME res),...} = lookup unit
            in  res
	    end

	fun isDone unit = 
	    let val UNIT{status,...} = lookup unit
	    in  (case !status of
		     DONE => true
		   | _ => false)
	    end
	fun markReady unit = 
	    let val UNIT{status,...} = lookup unit
	    in  (case !status of
		     WAITING => status := READY
		   | READY => status := READY
		   | PENDING => error "unit was pending; making ready\n"
		   | DONE => error "unit was done; making ready\n")
	    end
	fun checkReady unit = 
	    let val imports = get_import_direct unit (* no need to check transitively *)
		val ready = Listops.andfold isDone imports
	    in  if ready then markReady unit else ()
	    end
	fun markPending unit = 
	    let val UNIT{status,filebase,dependents_direct,...} = lookup unit
		val _ = status := PENDING
	    in  ()
	    end
	fun markDone unit = 
	    let val UNIT{status,filebase,dependents_direct,...} = lookup unit
		val uifile = base2ui filebase
		fun loop() = if (exists uifile) then () 
			     else (print ("non-existent " ^ uifile ^ "\n"); loop())
		val _ = loop()
		val _ = status := DONE
	    in  app checkReady (!dependents_direct)
	    end
	fun partition units = 
	    let fun folder (unit,(w,r,p,d)) = 
		let val UNIT {status,...} = lookup unit
		in  (case !status of
			 WAITING => (unit::w, r, p, d)
		       | READY => (w, unit::r, p, d)
		       | PENDING => (w, r, unit::p, d)
		       | DONE => (w, r, p, unit::d))
		end
	    in  foldl folder ([],[],[],[]) units
	    end
	fun computeTransitive targetunit seenunits = 
	    let val UNIT{imports_transitive, ...} = lookup targetunit
	    in  (case !imports_transitive of
		   NONE => 
		       let fun folder(import,acc) = 
			   let val depends = computeTransitive import (import::seenunits)
			       fun adder(u, ac) = if Listops.member(u,ac)
						      then ac else u::ac
			   in  adder(import,foldl adder acc depends)
			   end
			   fun check_loop imports = 
			       app (fn imp => if (Listops.member(imp,seenunits))
						  then error ("Loop detected in: " ^
							      foldr (fn (a,b) => (a ^ " " ^ b)) "" seenunits)
					      else ()) imports
			   val base_imports = get_import_direct targetunit
			   val _ = check_loop base_imports
			   val result = rev(foldl folder [] base_imports)
			   val _ = imports_transitive := SOME result
		       in  result
		       end
		 | SOME result => result)
	    end

	fun setMapping (isMaster, mapFile) =
	    let val _ = reset_stats()
		fun init_status() = 
		    let fun help unit = 
			let val UNIT{status,imports_direct=ref (SOME imp),...} = lookup unit
			in  if (null imp) then status := READY else status := WAITING
			end
		    in  app help (list_units())
		    end
		val mapTime = if (exists mapFile)
			       then modTime mapFile
			   else error "Cannot read map file"
		val same = (case !mapfileInfo of
				SOME (name,time) => (name = mapFile andalso (time = mapTime))
			      | _ => false)
	    in  if same then (chat ("Mapfile " ^ mapFile ^ " cached.");
			      if isMaster then (init_status(); chat "  Initializing status.") else ();
			      chat "\n")
		else 
		  let
		      val _ = mapfileInfo := SOME(mapFile,mapTime)
		      val is = TextIO.openIn mapFile
		      fun loop (map,us) n = 
			  if (TextIO.endOfStream is)
			      then (map,us)
			  else 
			      let fun dropper s = String.sub(s,0) = #"#"
				  val line = TextIO.inputLine is
			      in  (case (split_line dropper line) of
				       [unitname, filebase] => 
					   let val entry = 
					       UNIT {position = n,
						     filebase = filebase,
						     imports_direct = ref NONE,
						     imports_transitive = ref NONE,
						     dependents_direct = ref [],
						     context = ref NONE,
						     status = ref WAITING,
						     ui_crc = ref NONE} 
					   in  loop (StringMap.insert(map, unitname, entry), 
						     unitname::us) (n+1)
					      end
				     | [] => loop (map,us) n
				     | _ => error ("ill-formed map line: " ^ line))
			      end
		      val (map,rev_us) = loop (StringMap.empty,[]) 0
		      val _ = TextIO.closeIn is
		      val _ = mapping := map
		      val _ = units := rev rev_us
		      val _ = chat ("Mapfile " ^ mapFile ^ " read.\n")
		      fun compute_graph unit = 
			  let val UNIT {status, filebase, imports_direct, imports_transitive, ...} = lookup unit
			      val imports = parse_impl_import(base2sml filebase)
			      val _ = imports_direct := SOME imports

			      val _ = computeTransitive unit []
			      val _ = app (fn imp => let val UNIT{dependents_direct, ...} = lookup imp
						     in  dependents_direct := unit :: (!dependents_direct)
						     end) imports
			  in  ()
			  end
		      val _ = app compute_graph (!units)
		      val _ = init_status()
		      val _ = chat ("Dependency graph computed.\n")
		  in  ()
		  end
	    end

	fun tick_cache() = let fun apper (UNIT{context=r as (ref(SOME(i,ctxt))),...}) = 
	    if (i<=1) then r := NONE else r := SOME(i-1,ctxt)
				 | apper _ = ()
			   in  StringMap.app apper (!mapping)
			   end
		       
	fun readContext unit = 
	    let val r = get_context unit
	    in  (case !r of
		     SOME (i,ctxt) => (r := SOME(Int.min(i+2,!cache_context),ctxt); 
				       (true,ctxt))
		   | NONE => let val uifile = base2ui (get_base unit)
				 val ctxt = readContextRaw uifile
				 val _ = if (!cache_context>0) 
					     then r:=SOME(2,ctxt) else ()
			     in  (false,ctxt)
			     end)
	    end
	
	fun writeContext (unit,ctxt) = 
	    let val r = get_context unit
		(*	   val _ = if (!cache_context>0) then r := SOME(2,ctxt) else () *)
		val uifile = base2ui (get_base unit)
		val same = 
		    (exists uifile) andalso
		    let val old_ctxt = readContextRaw uifile
		    in  LinkIl.IlContextEq.eq_context(old_ctxt,ctxt)
		    end handle _ => (print "Warning: Ill-formed uifile?\n"; false)
	    in  if same
		    then chat ("  [" ^ uifile ^ " remains unchanged]\n")
		else (forget_stat uifile;
		      chat ("  [writing " ^ uifile);
		      writeContextRaw(uifile,ctxt);
		      chat "]\n")
	    end
	
	fun getContext imports = 
	    let 
		val _ = Name.reset_varmap()
		val _ = tick_cache()
		val cached_ctxts = map readContext imports
		val ctxts = map #2 cached_ctxts
		val uncached = (List.mapPartial 
				(fn (imp,(false,_)) => SOME imp
			      | _ => NONE) (Listops.zip imports cached_ctxts))
		val _ = (chat "  [These imports were not cached: ";
			 chat_strings 30 uncached;
			 chat "]\n")
		val initial_ctxt = LinkIl.initial_context()
		val context = addContext (initial_ctxt :: ctxts)
		val _ = chat ("  [Added contexts.]\n")
	    in  context
	    end
    end
end



structure Slave :> SLAVE =
struct
    val error = fn s => Util.error "pmanager.sml" s

    fun setup () = Communication.acknowledge NONE
    fun single perform =
	(case Communication.request() of
	     NONE => false
	   | SOME cur => (Help.chat ("Received " ^ (String.concat cur) ^ "\n");
			  perform cur; Communication.acknowledge (SOME cur);
			  Help.chat ("Processed " ^ (String.concat cur) ^ "\n");
			  true))
    fun all perform =
	let val _ = setup()
	    fun loop () =
		(single perform;
		 loop())
	in  loop()
	end

    fun slaveTest() = all (fn _ => Platform.sleep 1.0)
    fun elab_constrained(unit,ctxt,sourcefile,fp,dec,fp2,specs,uiFile,least_new_time) =
	(case LinkIl.elab_dec_constrained(ctxt, fp, dec, fp2,specs) of
	     SOME (new_ctxt, sbnd_entries) =>
		 let val _ = (Help.chat ("  [writing " ^ uiFile);
			      Help.writeContext (unit, new_ctxt);
			      Help.chat "]\n")
		 in (new_ctxt,sbnd_entries)
		 end
	   | NONE => error("File " ^ sourcefile ^ " failed to elaborate."))
    
    fun elab_nonconstrained(unit,pre_ctxt,sourcefile,fp,dec,uiFile,least_new_time) =
	case LinkIl.elab_dec(pre_ctxt, fp, dec) of
	    SOME(new_ctxt, sbnd_entries) => 
		let val _ = (Help.chat ("  [writing " ^ uiFile);
			     Help.writeContext (unit, new_ctxt);
			     Help.chat "]\n")
		in (new_ctxt,sbnd_entries)
		end
	  | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")


    fun perform [mapFile,unit] = 
	let val _ = Help.setMapping (false, mapFile)
	    val base = Help.get_base unit
	    val intFile = Help.base2int base
	    val smlFile = Help.base2sml base
	    val uiFile = Help.base2ui base
	    val imports = Help.get_import_transitive unit
	    val ctxt = Help.getContext imports
		
	    val _ = Help.chat ("\n  [Parsing " ^ smlFile ^ "]\n")
	    val (lines,fp, _, dec) = LinkParse.parse_impl smlFile
(*  		  val _ = if (lines > 1000) then flush_cache() else () *)

	    (* Elaborate the source file, generating a .ui file *)
	    val (ctxt',sbnds) = 
		if Help.exists intFile then
		    let val (_,fp2, _, specs) = LinkParse.parse_inter intFile
			val _ = Help.chat ("  [Warning: constraints currently coerce.  Not compatiable with our notion of freshness.]\n")
			val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " with constraint]\n"  )
		    in elab_constrained(unit,ctxt,smlFile,fp,dec,fp2,specs,uiFile,Time.zeroTime)
		    end
		else let val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " non-constrained]\n")
		     in elab_nonconstrained(unit,ctxt,smlFile,fp,dec,uiFile,Time.zeroTime)
		     end

	    (* Continue compilation, generating a platform-dependent .o object file *)
	    val _ = Help.chat ("  [Compiling to object file ...")
	    val oFile = Til.compile(ctxt, base, sbnds, ctxt')  
	    val _ =  Help.chat "]\n"
		
	    (* Generate a .uo file containing the checksum for the .o file *)
	    val _ = Help.chat ("  [Creating .uo file ...")
	    val imports_uo = map (fn imp => (imp, Help.get_ui_crc imp)) imports
	    val exports_uo = [(base, Help.get_ui_crc unit)]
	    val uoFile = Linker.mk_uo {imports = imports_uo,
				       exports = exports_uo,
				       base_result = base}
	    val _ = Help.chat "]\n"
	    val _ = (Help.forget_stat uoFile; Help.forget_stat uiFile)
		
	    (* Print some diagnostic information *)
	    val _ = if (!Help.stat_each_file)
			then (OS.Process.system ("size " ^ oFile); Stats.print_timers())
		    else ()
	      in  ()
	end
      | perform _ = error "Slave received ill-formed message"
	
    fun once() = single perform 
    fun run() = all perform 

end




structure Master =
struct
    val error = fn s => Util.error "pmanager.sml" s
    open Help

    local
	val waitingSlaves = ref ([] : (string * (Communication.job option -> unit)) list)
    in  (* Asynchronously ask for whether there are slaves ready *)
	fun pollForSlaves process_ack : int = 
	    let val (names,acks,slaves) = Listops.unzip3 (Communication.findSlaves())
		val _ = app process_ack (Listops.zip names acks)
		val _ = waitingSlaves := (Listops.zip names slaves) @ (!waitingSlaves)
	    in  length (!waitingSlaves)
	    end
	(* Works only if there are slaves available *)
	fun useSlave process_request = 
	    let val (name_invoke::rest) = !waitingSlaves
		val _ = waitingSlaves := rest
	    in  process_request name_invoke
	    end
    end

  (* ----- Compute the latest mod time of a list of files --------- *)
    fun get_latest [] = (NONE, Time.zeroTime)
      | get_latest (f::fs) = 
	let
	    val recur_result as (_,fstime) = get_latest fs 
	    val ftime = modTime f
	in  if (Time.>=(ftime, fstime)) then (SOME f, ftime) else recur_result
	end

    (* Compiles the unit either by determining that compilation is not necessary or by calling a slave. 
       This call does not block. *)
    fun needsCompile unitname = 
        let val sourcebase = get_base unitname
	    val intfile = base2int sourcebase
	    val smlfile = base2sml sourcebase
	    val uofile = base2uo sourcebase
	    val ofile = base2o sourcebase
	    val uifile = base2ui sourcebase

	    val smldate = modTime smlfile
	    val dest_ui_exists = exists uifile
	    val dest_uo_exists = exists uofile
	    val dest_o_exists = exists ofile

	    val direct_imports = get_import_direct unitname
	    val direct_imports_base = map get_base direct_imports
	    val direct_imports_ui = map base2ui direct_imports_base
	    val (latest_import_file, latest_import_time) = get_latest direct_imports_ui
 
	    val sml_changed = 
                (dest_ui_exists andalso
                 dest_uo_exists andalso 
                 dest_o_exists andalso
                 (Time.<(modTime uofile, smldate) orelse
                  Time.<(modTime ofile, smldate)))

	    val import_changed = 
                (dest_uo_exists andalso
                 dest_o_exists andalso
                 (Time.<(modTime ofile, latest_import_time) orelse
                  Time.<(modTime uofile, latest_import_time)))

	    val fresh =           
		 if (not dest_ui_exists) then
		     (chat ("    [" ^ sourcebase ^ " is out-of-date: " ^
			    uifile ^ " is missing.]\n");
		      false)
		 else if (not dest_uo_exists) then
		     (chat ("    [" ^ sourcebase ^ " is out-of-date: " ^
			    uofile ^ " is missing.]\n");
		      false)
		 else if (not dest_o_exists) then
		     (chat ("    [" ^ sourcebase ^ " is out-of-date: " ^
			    ofile ^ " is missing.]\n");
		      false)
		 else if sml_changed then
		     (chat ("    [" ^ sourcebase ^ " is out-of-date: " ^
			    smlfile ^ " newer than objects or interface.]\n");
		      false)
                 else if import_changed then
		     (chat ("    [" ^ sourcebase ^ " is out-of-date: " ^
			    (valOf latest_import_file) ^ " changed.]\n");
		      false)
		 else 
		     true


	    val _ = if fresh
			then (chat ("  [" ^ sourcebase ^ " is up-to-date]\n");
			      markDone unitname)
		    else ()

	in  not fresh
	end

    type state = string list * string list
    fun once (mapfile, srcs) = 
	let val _ = setMapping (true, mapfile)
	    val srcs = if (null srcs) then list_units() else srcs
	    val temp = map get_import_transitive srcs
	    (* XXX Should be in module Listops *)
	    fun list_union_eq eq ([], b) = b
	      | list_union_eq eq (a::rest, b) = if (Listops.member_eq(eq,a,b))
						    then list_union_eq eq (rest, b)
						else list_union_eq eq (rest, a::b)
	    val units = foldl (list_union_eq (fn (a:string,b) => a=b)) [] temp
	    val _ = chat "Computed all necessary units\n"
	    fun waitForSlaves() = 
		let fun do_ack (_,NONE) = ()
		      | do_ack (name,SOME [mapFile,u]) = 
		          (markDone u; chat ("  [Slave " ^ name ^ " compiled " ^ u ^ "]\n"))
		      | do_ack _ = error "Acknowledgement not two words"
		    val numSlaves = pollForSlaves do_ack
		in  if numSlaves = 0
			then (Platform.sleep 0.1; waitForSlaves())
		    else numSlaves
		end
	    fun getReady waiting = 
		let val (waiting,ready, [], []) = partition waiting
		    val reallyReady = List.filter needsCompile ready
		in  if (length ready = length reallyReady)
			then (waiting, reallyReady)
		    else getReady (waiting @ reallyReady)
		end
	    fun step (([], []) : state)  = (false, NONE)
	      | step (waiting, pending) =
		let val (w,r, pending, done) = partition pending
		    val _ = if (null w) then () else (print "waiting nonempty: ";
						      app print w; print "\n")
		    val _ = if (null r) then () else (print "ready nonempty: "; app print r; print "\n")
		    val (waiting,ready) = getReady waiting
		    val numSlaves = waitForSlaves()
		    fun useSlaves slavesLeft (pending, ready, waiting) = 
			(case (slavesLeft, ready) of
			     (0, _) => (true, pending, ready @ waiting)
			   | (_, []) =>
				 (chat "  [Idling: Available slaves but no jobs.";
				  chat "  Waiting for completion of ";
				  chat_strings 20 pending;
				  chat "]\n";
				  chat "waiting = "; 
				  chat_strings 10 waiting;
				  chat "\n\n";
				  (false,pending,waiting))
			   | (_, first::rest) =>
				 let fun do_request (name, invoke) = 
				     let val _ = chat ("  [Calling slave " ^ name ^ 
						       " to compile " ^ first ^ "]\n");
					 val _ = markPending first
					 val _ = invoke (SOME [mapfile,first])
					 val base = get_base first
					 val _ = forget_stat (base2ui base)
					 val _ = forget_stat (base2o base)
					 val _ = forget_stat (base2uo base)
				     in  () 
				     end
				 in  useSlave do_request; 
				     useSlaves (slavesLeft - 1) (first::pending, rest, waiting)
				 end)
		    val (allSlavesUsed, pending, waiting) = useSlaves numSlaves (pending, ready, waiting)
		in  (allSlavesUsed, SOME (waiting, pending))
		end
	in  (units, (units, []), step)
	end
    fun run args = 
	let val (units,state,step) = once args
	    fun loop state = case (step state) of
		               (_,NONE) => units
			     | (progress,SOME state) => (if progress then Platform.sleep 1.0 else ();
							 loop state)
	in  loop state
	end
end

structure Manager :> MANAGER = 
struct

  val error = fn s => Util.error "pmanager.sml" s
  open Help

  fun makeExe(exe, units) = 
      let val unit_set = 
               List.foldl 
                 (fn (next, set) => 
                     let val import_tr = Help.get_import_transitive next
			 val next_pos = Help.get_position next
			 fun check import = if (get_position import < next_pos) then ()
			                    else 
						error ("Mapfile file ordering is inconsistent because " ^
							next ^ " imports " ^ import ^ " but precedes it.")
			 val _ = app check import_tr
		     in StringSet.add(StringSet.addList (set, import_tr), next)
                     end)
                 (StringSet.empty)
                 units

	  val units = List.filter (fn unit => (StringSet.member(unit_set,unit))) units

	  fun mapper unit = 
	      let val base = get_base unit
	      in  {unit=unit, base=base,
		   uiFile=base2ui base, uoFile=base2uo base, oFile=base2o base}
	      end
	  val packages = map mapper units
      in  (print "Manager calling linker with: ";
	   app (fn s => (print s; print " ")) units;
	   print "\n";
	   Linker.mk_exe {units = packages, exe_result = exe})
      end

  val slave = Slave.run
  fun helper runner (mapfile : string, cs : bool, os : string option, srcs : string list) =
	let val _ = if !(Stats.tt "Reset stats between calls") then Stats.clear_stats() else ()
	in  (case (cs, os) of
		 (false, NONE)   => let val units = runner(mapfile, srcs)
					val default_exe = (List.last units) ^ ".exe"
				    in  makeExe(default_exe, units)
				    end
	       | (false, SOME given_exe) => let val units = runner(mapfile, srcs)
					    in  makeExe(given_exe, units)
					    end
	       | (true,  NONE)   => (runner(mapfile, srcs); ())
	       | (true,  SOME _) => error "Cannot specify -c and -o");
	    Stats.print_stats()
	end

  fun master mapfile = helper Master.run (mapfile, false, NONE, [])
  fun pmake (mapfile, slaves) = 
      let fun startSlave (num,machine) = 
	  let val geometry = "80x16+0+" ^ (Int.toString (num * 250))
	      val dir = OS.FileSys.getDir()
	      val SOME display = OS.Process.getEnv "DISPLAY"
	      val SOME user = OS.Process.getEnv "USER"
	      val out = TextIO.openOut "startSlave1"
	      val _ = TextIO.output(out, "(shell)\n")
	      val _ = TextIO.output(out, "(end-of-buffer)\n")
	      val _ = TextIO.output(out, "(insert-file \"" ^ dir ^ "/startSlave2\")\n")
	      val _ = TextIO.output(out, "(end-of-buffer)\n")
	      val _ = TextIO.output(out, "(comint-send-input)\n")
	      val _ = TextIO.output(out, "(insert-file \"" ^ dir ^ "/startSlave3\")\n")
	      val _ = TextIO.output(out, "(end-of-buffer)\n")
	      val _ = TextIO.output(out, "(comint-send-input)\n")
	      val _ = TextIO.closeOut out
	      val out = TextIO.openOut "startSlave2"
	      val _ = TextIO.output(out, "cd " ^ dir ^ "; Local/sml-cm\n")
	      val _ = TextIO.closeOut out
	      val out = TextIO.openOut "startSlave3"
	      val _ = TextIO.output(out, "CM.make(); Manager.slave();\n")
	      val _ = TextIO.closeOut out
	      val command = 
		  "setenv DISPLAY " ^ display ^ 
		  "; xterm -geometry -0 -e kinit " ^ user ^
		  "; emacs -bg black -fg yellow -geometry " ^ geometry ^ " -l " ^ dir ^ "/startSlave1"
	  in  (OS.Process.system ("rsh " ^ machine ^ " '" ^ command ^ "'&"); ()) 
	  end
      in  Listops.mapcount startSlave slaves; 
	  master mapfile
      end
  fun tilc arg =
      let fun runner args = 
	  let val (units,mstate,mstep) = Master.once args
	      val _ = Slave.setup()
	      fun loop state = 
		  (case (mstep state) of
		       (_,NONE) => units
		     | (_,SOME state) => (Slave.once(); 
					  loop state))
	  in  loop mstate
	  end
      in  helper runner arg
      end
  fun make mapfile = tilc (mapfile, false, NONE, [])

  fun purge(mapfile : string) =
      let val _ = (print "Purging "; print mapfile; print "\n")
	  val _ = Help.setMapping (false, mapfile)
	  val units = Help.list_units()
	  fun remove unit = 
	      let val base = get_base unit
		  val ui = base2ui base
		  val uo = base2uo base
		  val s = base2s base
		  val obj = base2o base
		  fun kill file = if (OS.FileSys.access(file, []) andalso
				      OS.FileSys.access(file, [OS.FileSys.A_READ]))
				      then OS.FileSys.remove file
				  else ()
	      in  kill ui; kill uo; kill s; kill obj
	      end
      in  app remove units
      end

  fun buildRuntime rebuild = 
      let val command = if rebuild then "cd Runtime; gmake purge; gmake runtime"
			else "cd Runtime; gmake runtime"
      in  if Util.system command then () else error "Error in building runtime"
      end

  (* getArgs:
     Takes a list of string arguments and returns a 4-tuple.
     The 1st is the mapfile.
     The 2nd indicates whether the -c flag is present.
     The 3rd carries the name of the -o filename (final executable), if present.
     The 4th component is a list of the source files to process. *)

  val flags = (ref false, ref false, ref false, ref false) (* c, r ,o, all *)

  fun resetFlags () = (#1(flags) := false;
		       #2(flags) := false;
		       #3(flags) := false;
		       #4(flags) := false)

  fun getArgs (args : string list) :  string option * bool * string option * bool * string list = 
      let
	  fun loop args (acc as (mapFile, hasC, oFile, hasAll, srcs)) = 
	      case args of
		  [] => acc
		| ("-c"::rest) => if hasC 
				      then  error ("Two -c switches not allowed.")
				  else loop rest (mapFile, true, oFile, hasAll, srcs)
		| ("-all"::rest) => if hasAll
				      then error ("Two -all switches not allowed.")
				    else if (null srcs)
					     then loop rest (mapFile, hasC, oFile, true, srcs)
					 else error "-all switch given but also units"
		| ["-o"] => error "No output file specified for -o switch." 
		| ("-o" :: file :: rest) => 
				      (case oFile of
					   NONE => loop rest (mapFile, hasC, SOME file, hasAll, srcs)
					 | SOME _ => error "Output file already specified")
		| ["-m"] => error "No output file specified for -m switch." 
		| ("-m" :: file :: rest) => 
				      (case mapFile of
					   NONE => loop rest (SOME file, hasC, oFile, hasAll, srcs)
					 | SOME _ => error "Output file already specified")
		| (src::rest) => if hasAll
				     then error "-all switch given and also unts"
				 else loop rest (mapFile, hasC, oFile, hasAll, src::srcs)
      in  (resetFlags(); loop args (NONE, false, NONE, false, []))
      end

  fun help() = print "This is TILT - no help available.\n"
  fun command(env : string, args : string list) : int =
    case args of 
      [] => (print ("No arguments specified.\n"); 1)
    | ["-h"] => (help(); 0)
    | ["-?"] => (help(); 0)
    | ("-h"::_) =>
	  (print  ("Invalid arguments: -h or -? must occur by itself.\n"); 1)
    | ("-?"::_) =>
	  (print  ("Invalid arguments: -h or -? must occur by itself.\n"); 1)
    | (["-mkdep", makefile]) =>	(Makedep.mkDep(makefile); 0)
    | ("-mkdep"::args) => (print ("Incorrect number of files to -mkdep.\n"); 1)
    | args => 
	let val (mapFile, hasC, oFile, hasAll, srcs) = getArgs args
	    val mapFile = (case mapFile of
			       NONE => "mapfile"
			     | SOME mfile => mfile)
	    val srcs = if hasAll then [] else srcs
	in (tilc(mapFile, hasC, oFile, srcs); 0)
	end

end