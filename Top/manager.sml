(*$import MANAGER LinkParse LinkIl Compiler Linker MakeDep OS List SplayMapFn SplaySetFn Platform *)

structure Help :> HELP = 
struct
    val error = fn s => Util.error "manager.sml" s
    val eager = ref true

    (* ---- Some diagnostic message helper functions ---- *)
    val chat_ref = Stats.tt("ManagerChat")
    fun chat s = if !chat_ref then (print s; TextIO.flushOut TextIO.stdOut)
		 else ()
    fun chat_strings skip imports =
	let fun f(str,acc) = 
	    let val cur = 2 + size str
	    in  if (acc + cur > 80)
		    then (chat "\n        "; chat str; 6 + cur)
		else (chat "  "; chat str; acc + cur)
	    end
	in  if (!chat_ref) then foldl f skip imports else 0
	end

    type unitname = string
    type filebase = string
    fun base2sml (f : string) = f ^ ".sml"
    fun base2int (f : string) = f ^ ".int"
    val base2ui = Til.base2ui
    val base2s = Til.base2s
    val base2o = Til.base2o
    val base2uo = Til.base2uo

    val msgs = ref ([] : string list)
    fun showTime str = 
	let val msg = (str ^ ": " ^ 
		       (Date.toString(Date.fromTimeLocal(Time.now()))) ^ ".\n")
	in  msgs := msg :: (!msgs); chat msg
	end
    fun reshowTimes() = (chat "\n\n"; app chat (rev (!msgs)); msgs := [])
end


structure Communication :> COMMUNICATION = 
struct
    type job = string list
    val delimiter = #"|"
    val ready = "!READY!" (* Slaves send this to signify readiness *)
    val done = "!DONE!"   (* Master sends this to terminate slaves.
			     Normally, the master sends an acknowledgement by sending the next request. *)

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

    (* Non-blocking acknowledgement *)
    fun acknowledge (completed : job option) = 
	let val masterID = "master"
	    val slaveID = selfID
	    val ch = channel(slaveID, masterID)
	    (* Acknowledge job completion and request more if acknowledgement processed *)
	in (case completed of
		SOME msg => send(ch, msg)
	      | NONE => send(ch, [ready]))
	end
    (* blocking request for jobs *)
    fun request () : job option =
	let val masterID = "master"
	    val slaveID = selfID
	    val ch = channel(masterID, slaveID)
	    (* Look for available jobs *)
	    fun loop() = 
		(case receive ch of
		     NONE => loop()
		   | SOME msg => 
			 (case msg of
			      [word] => if (word = done) then NONE else SOME msg
			    | _ => SOME msg))
	in  loop()
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
(*		    val _ = (print "received from slave: "; Help.chat_strings 20 message; print "\n") *)
		    val message = (case message of
				       [temp] => if (temp = ready) then NONE else SOME message
				     | _ => SOME message)
		    val tmp = size "master<-"
		    val slaveName = String.substring(channel,tmp,(size channel) - tmp)
		    val revChannel = slaveName ^ "<-master"
		    fun invoke NONE = send(revChannel, [done])
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


structure StringKey = 
    struct
	type ord_key = string
	val compare = String.compare
    end
structure StringMap = SplayMapFn(StringKey)
structure StringSet = SplaySetFn(StringKey)
(* A set with an ordering maintained by a list *)
structure StringOrderedSet = 
struct
    type set = StringSet.set * string list
    val empty = (StringSet.empty, [])
    fun member (str,(set,_) : set) = StringSet.member(set,str)
    fun cons (str,(set,list) : set) : set = if (StringSet.member(set,str))
				    then (set,list)
				else (StringSet.add(set,str), str::list)
    fun toList ((set,list) : set) = list
end


functor FileCache(type internal
		  val equaler : internal * internal -> bool
		  val reader : string -> internal
		  val writer : string * internal -> unit) :>
    FILECACHE where type internal = internal =
struct

  type internal = internal
  val cache = ref 5  (* Number of ticks before an unused entry is discarded *)
  val error = fn s => Util.error "manager.sml" s
  datatype stat = ABSENT 
                | PRESENT of int * Time.time                            (* file size + mod time *)
                | CRC     of int * Time.time * Crc.crc                  (* + CRC *)
                | CACHED  of int * Time.time * Crc.crc * int * internal (* + ticks + cached result *)

  val stats = ref (StringMap.empty : stat StringMap.map)
	    
  fun set (file,stat) = stats := (StringMap.insert(!stats,file,stat))
  fun get file = 
      (case StringMap.find(!stats,file) of
	   NONE => let val stat = ABSENT
		       val _ = set (file, stat)
		   in  stat
		   end
	 | SOME stat => stat)

  (* This is the underlying uncached function *)
  fun modTimeSize_raw file =
      let val exists = 
	  ((OS.FileSys.access(file, []) andalso
	    OS.FileSys.access(file, [OS.FileSys.A_READ]))
	   handle _ => (print ("Warning: OS.FileSys.access " ^ 
			       file ^ "\n"); false))
      in  if exists
	      then SOME(OS.FileSys.fileSize file, OS.FileSys.modTime file)
	  else NONE
      end
	

  (* This two functions totally or partially flushes the cache *)
  fun flushAll() = (stats := StringMap.empty)
  fun flushSome files = 
      let fun remove file = (stats := #1 (StringMap.remove(!stats, file))
			     handle _ => ())
      in  app remove files
      end
	    
  fun modTimeSize_cached file =
      (case get file of
	   ABSENT => (case modTimeSize_raw file of
			  NONE => (set(file,ABSENT); NONE)
			| SOME st => (set(file, PRESENT st); SOME st))
	 | PRESENT (s,t) => SOME (s,t)
	 | CACHED (s, t, _, _, _) => SOME (s,t))
	   
  fun exists file = (case modTimeSize_cached file of
			 NONE => false
		       | SOME _ => true)
  fun modTime file = (case modTimeSize_cached file of
			 NONE => error ("modTime on non-existent file " ^ file)
		       | SOME (s,t) => t)
  fun size file = (case modTimeSize_cached file of
		       NONE => error ("size on non-existent file " ^ file)
		     | SOME (s,t) => s)

  fun tick() =
      let fun mapper (CACHED (s, t, crc, tick, r)) = if (tick <= 1) then PRESENT (s,t)
						     else CACHED(s, t, crc, tick-1, r)
	    | mapper entry = entry
      in  stats := (StringMap.map mapper (!stats))
      end

  fun read file = 
      (case (get file) of
	   CACHED (s, t, crc, tick, result) => 
	       let val stat = CACHED(s, t, crc, Int.min(tick+2,!cache), result)
		   val _ = set (file, stat)
	       in  (true, result)
	       end
	 | _ => let val (s,t) = (case modTimeSize_raw file of
				     SOME st => st
				   | NONE => error ("Reading non-existent file " ^ file))
		    val crc = Crc.crc_of_file file
		    val result = reader file
		    val stat = if (!cache>0) 
				   then CACHED(s, t, crc, 2, result)
			       else CRC (s, t, crc)
		    val _ = set(file, stat)
		in  (false, result)
		end)

  fun crc file = 
      (case (get file) of
	   ABSENT => error "reading absent file"
	 | CACHED (_, _, crc, _, _) => crc
	 | CRC (_, _, crc) => crc
	 | PRESENT (s, t)  => let val crc = Crc.crc_of_file file
				  val stat = CRC (s, t, crc)
				  val _ = set(file, stat)
			      in  crc
			      end)
	   
  fun write (file, result) = 
      let val same = (exists file) andalso
		      let val (_,oldResult) = read file
		      in  equaler(result, oldResult)
		      end
      in  if same
	      then false
	  else let val _ = writer(file,result)
		   val crc = Crc.crc_of_file file
		   val SOME (s,t) = modTimeSize_raw file
		   val stat = if (!cache>0) 
				  then CACHED(s, t, crc, 2, result)
			      else CRC (s, t, crc)
		   val _ = set(file, stat)
	       in  true
	       end
      end

end


structure Slave :> SLAVE =
struct
    open Help
    val error = fn s => Util.error "pmanager.sml" s
    val stat_each_file = Stats.tt("TimeEachFile")

    datatype result = WORK of string | WAIT | READY
    fun readPartialContextRaw file = 
	let val _ = print ("XXX reading context file " ^ file ^ "\n")
	    val is = BinIO.openIn file
	    val res = LinkIl.IlContextEq.blastInPartialContext is
	    val _ = BinIO.closeIn is
	in  res
	end
    val readPartialContextRaw = Stats.timer("ReadingContext",readPartialContextRaw)

    fun writePartialContextRaw (file,ctxt) = 
	let val os = BinIO.openOut file
	    val _ = LinkIl.IlContextEq.blastOutPartialContext os ctxt
	    val _ = BinIO.closeOut os
	in  () 
	end
    val writePartialContextRaw = Stats.timer("WritingContext",writePartialContextRaw)
    structure Cache = FileCache(type internal = Il.partial_context
				val equaler = LinkIl.IlContextEq.eq_partial_context
				val reader = readPartialContextRaw
				val writer = writePartialContextRaw)
    fun getContext uifiles =
	let 
	    val _ = Name.reset_varmap()
	    val _ = Cache.tick()
	    val isCached_ctxts = map Cache.read uifiles
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
		     chat (Int.toString uncached_size);  chat " were not cached: ";
		     chat_strings 40 uncached;
		     chat "]\n")
	    val initial_ctxt = LinkIl.initial_context()
	    val addContext = Stats.timer("AddingContext",LinkIl.plus_context)
	    val context = addContext (initial_ctxt, partial_ctxts)
	    val _ = chat ("  [Added contexts.]\n")
	in  context
	end

    fun elab_constrained(unit,ctxt,sourcefile,fp,dec,fp2,specs,uiFile,least_new_time) =
	(case LinkIl.elab_dec_constrained(ctxt, fp, dec, fp2,specs) of
	     SOME (il_module as (_, partial_ctxt, _)) =>
		 let val _ = (Help.chat ("  [writing " ^ uiFile);
			      Cache.write (uiFile, partial_ctxt);
			      Help.chat "]\n")
		 in il_module
		 end
	   | NONE => error("File " ^ sourcefile ^ " failed to elaborate."))
    
    fun elab_nonconstrained(unit,pre_ctxt,sourcefile,fp,dec,uiFile,least_new_time) =
	case LinkIl.elab_dec(pre_ctxt, fp, dec) of
	    SOME (il_module as (_, partial_ctxt, _)) => 
		let val _ = (Help.chat ("  [writing " ^ uiFile);
			     Cache.write (uiFile, partial_ctxt);
			     Help.chat "]\n")
		in il_module
		end
	  | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")


    fun setup () = (print "Starting slave.\n";
		    Cache.flushAll();
		    Communication.acknowledge NONE)
    (* Blocking single step of slave - bool indicates progress *)
    fun single perform =
	(case Communication.request() of
	     NONE => false
	   | SOME cur => ((* Help.chat ("Received "); Help.chat_strings 10 cur; Help.chat "\n"; *)
			  perform cur; Communication.acknowledge (SOME cur);
			  (* Help.chat ("Processed "); Help.chat_strings 10 cur; Help.chat "\n"; *)
			  true))
    fun all perform =
	let val _ = setup()
	    fun loop () =
		(if (single perform)
		    then loop()  (* did some work *)
		 else 
		     all perform)  (* restarting *)
	in  loop()
	end

    fun slaveTest() = all (fn _ => Platform.sleep 1.0)

    fun perform (unit::base::importBases) = 
	let val intFile = Help.base2int base
	    val smlFile = Help.base2sml base
	    val uiFile = Help.base2ui base
	    val ctxt = getContext (map Help.base2ui importBases)
		
	    val _ = Help.chat ("\n  [Parsing " ^ smlFile ^ "]\n")
	    val (lines,fp, _, dec) = LinkParse.parse_impl smlFile
(*  		  val _ = if (lines > 1000) then flush_cache() else () *)

	    (* Elaborate the source file, generating a .ui file *)
	    val _ = Cache.flushSome [uiFile]
	    val il_module =
		if Cache.exists intFile then
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
	    val oFile = Til.compile (unit,base, il_module)
	    val _ = Cache.flushSome [oFile]
	    val _ =  Help.chat "]\n"
		
	    (* Generate a .uo file containing the checksum for the .o file *)
	    val _ = Help.chat ("  [Creating .uo file ...")
	    val imports_uo = map (fn impBase => (impBase, Cache.crc (Help.base2ui impBase))) importBases
	    val exports_uo = [(base, Cache.crc uiFile)]
	    val uoFile = Linker.mk_uo {imports = imports_uo,
				       exports = exports_uo,
				       base_result = base}
	    val _ = Cache.flushSome [uoFile]
	    val _ = Help.chat "]\n"


		
	    (* Print some diagnostic information *)
	    val _ = if (!stat_each_file)
			then (OS.Process.system ("size " ^ oFile); Stats.print_timers())
		    else ()
	      in  ()
	end
      | perform _ = error "Slave received ill-formed message"
	
    fun step() = single perform 
    fun run() = all perform 

end




structure Master =
struct
    val error = fn s => Util.error "pmanager.sml" s
    open Help

    structure Cache = FileCache(type internal = unit
				val equaler = fn _ => true
				val reader = fn _ => error "Master structure called read"
				val writer = fn _ => error "Master structure called write")

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


    datatype status = WAITING | READY | PENDING | DONE
    local
	datatype unitinfo = 
	    UNIT of {position : int,
		     filebase : string,
		     imports_direct  : string list option ref,
		     imports_transitive : string list option ref,
		     dependents_direct : string list ref,
		     status : status ref}
	    
        val units = ref ([] : string list)
	val mapping = ref (StringMap.empty : unitinfo StringMap.map)

	fun find_unit unitname = StringMap.find(!mapping,unitname)
	fun lookup unitname =
	    (case (find_unit unitname) of
		 NONE => error ("unit " ^ unitname ^ " missing")
	       | SOME entry => entry)
 
    in 
	fun list_units() = !units

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
	fun get_dependent_direct unit = 
            let val UNIT{dependents_direct = ref res,...} = lookup unit
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
	(* Do we need to wait til the ui file is visible here? *)
	fun isDone unit = 
	    let val UNIT{status,filebase,dependents_direct,...} = lookup unit
	    in  (case (!status) of
		     DONE => true
		   | _ => false)
	    end
	fun markDone unit = 
	    let val UNIT{status,filebase,dependents_direct,...} = lookup unit
		val uifile = base2ui filebase
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
	fun computeTransitive targetunit (seen : StringOrderedSet.set) : string list =
	    let val UNIT{imports_transitive, ...} = lookup targetunit
	    in  (case !imports_transitive of
		   NONE => 
		       let fun folder(import,acc) = 
			   let val depends = computeTransitive import (StringOrderedSet.cons(import,seen))
			   in  StringOrderedSet.cons(import,foldl StringOrderedSet.cons acc depends)
			   end
			   fun check_loop imports = 
			       app (fn imp => if (StringOrderedSet.member(imp,seen))
						  then error ("Loop detected in: " ^
							      foldr (fn (a,b) => (a ^ " " ^ b)) "" 
							      (StringOrderedSet.toList seen))
					      else ()) imports
			   val base_imports = get_import_direct targetunit
			   val _ = check_loop base_imports
			   val result = rev(StringOrderedSet.toList 
					    (foldl folder StringOrderedSet.empty base_imports))
			   val _ = imports_transitive := SOME result
		       in  result
		       end
		 | SOME result => result)
	    end
	val computeTransitive = fn unit => computeTransitive unit StringOrderedSet.empty

	fun setMapping mapFile =
	    let val _ = Cache.flushAll()
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
					       status = ref WAITING}
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
	    in  ()
	    end

	fun compute_graph () = 
	    let fun read_import unit = 
		    let val UNIT {status, filebase, imports_direct, ...} = lookup unit
			val imports = parse_impl_import(base2sml filebase)
			val _ = imports_direct := SOME imports
			val _ = if (null imports) then status := READY else status := WAITING
		    in  ()
		    end
		fun do_node unit = 
		    let val UNIT {status, imports_direct, ...} = lookup unit
			val SOME imports = !imports_direct
			val _ = computeTransitive unit 
			val _ = app (fn imp => let val UNIT{dependents_direct, ...} = lookup imp
					       in  dependents_direct := unit :: (!dependents_direct)
					       end) imports
		    in  ()
		    end
		val _ = app read_import (!units)
		val _ = chat ("Imports read.\n")
		val _ = app do_node (!units)
		val _ = chat ("Dependency graph computed.\n")
	    in  ()
	    end

    end


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
	fun useSlave (showSlave, msg) = 
	    let val ((name,invoke)::rest) = !waitingSlaves
		val _ = waitingSlaves := rest
	    in  showSlave name; invoke (SOME msg)
	    end
	(* Send termination signal to slaves *)
	fun resetSlaves() =  waitingSlaves := []
	fun flushSlaves() = (app (fn (name,invoke) => invoke NONE) (!waitingSlaves);
			     waitingSlaves := [])
    end

  (* ----- Compute the latest mod time of a list of files --------- *)
    fun get_latest [] = (NONE, Time.zeroTime)
      | get_latest (f::fs) = 
	let
	    val recur_result as (_,fstime) = get_latest fs 
	    val ftime = Cache.modTime f
	in  if (Time.>=(ftime, fstime)) then (SOME f, ftime) else recur_result
	end

    (* Compiles the unit either by determining that compilation is not necessary or by calling a slave. 
       This call does not block. *)
    fun needsCompile unitname = (* This unit is always in a ready state *)
        if (isDone unitname)
	    then false
	else 
	let val sourcebase = get_base unitname
	    val intfile = base2int sourcebase
	    val smlfile = base2sml sourcebase
	    val uofile = base2uo sourcebase
	    val ofile = base2o sourcebase
	    val uifile = base2ui sourcebase

	    val smldate = Cache.modTime smlfile
	    val dest_ui_exists = Cache.exists uifile
	    val dest_uo_exists = Cache.exists uofile
	    val dest_o_exists = Cache.exists ofile

	    val direct_imports = get_import_direct unitname
	    val direct_imports_base = map get_base direct_imports
	    val direct_imports_ui = map Help.base2ui direct_imports_base
	    val (latest_import_file, latest_import_time) = get_latest direct_imports_ui
 
	    val sml_changed = 
                (dest_ui_exists andalso
                 dest_uo_exists andalso 
                 dest_o_exists andalso
                 (Time.<(Cache.modTime uofile, smldate) orelse
                  Time.<(Cache.modTime ofile, smldate)))

	    val import_changed = 
                (dest_uo_exists andalso
                 dest_o_exists andalso
                 (Time.<(Cache.modTime ofile, latest_import_time) orelse
                  Time.<(Cache.modTime uofile, latest_import_time)))

	    val fresh =           
		 if (not dest_ui_exists) then
		     (chat ("      [" ^ sourcebase ^ " is stale: " ^
			    uifile ^ " is missing.]\n");
		      false)
		 else if (not dest_uo_exists) then
		     (chat ("      [" ^ sourcebase ^ " is stale: " ^
			    uofile ^ " is missing.]\n");
		      false)
		 else if (not dest_o_exists) then
		     (chat ("      [" ^ sourcebase ^ " is stale: " ^
			    ofile ^ " is missing.]\n");
		      false)
		 else if sml_changed then
		     (chat ("      [" ^ sourcebase ^ " is stale: " ^
			    smlfile ^ " newer than objects or interface.]\n");
		      false)
                 else if import_changed then
		     (chat ("      [" ^ sourcebase ^ " is stale: " ^
			    (valOf latest_import_file) ^ " changed.]\n");
		      false)
		 else 
		     true


	    val _ = if fresh
			then (chat ("      [" ^ sourcebase ^ " is up-to-date.]\n");
			      markDone unitname)
		    else ()

	in  not fresh
	end

    type state = string list * string list * string list
    fun once (mapfile, srcs,exeOpt) = 
	let val _ = resetSlaves()
	    val _ = setMapping mapfile
	    val _ = compute_graph()
	    val srcs = if (null srcs) then list_units() else srcs
	    fun folder (unit,acc) = 
		let val imports = get_import_transitive unit
		in  foldl StringOrderedSet.cons acc imports
		end
	    val units = rev (StringOrderedSet.toList (foldl folder StringOrderedSet.empty srcs))
	    val _ = (chat "Computed all necessary units: \n";
		     chat_strings 20 units; print "\n")
	    val _ = showTime "Start compiling files"
	    fun waitForSlaves() = 
		let fun do_ack (_,NONE) = ()
		      | do_ack (name,SOME (u::_::_)) = 
		          (markDone u; chat ("  [Slave " ^ name ^ " compiled " ^ u ^ "]\n"))
		      | do_ack _ = error "Acknowledgement not at least two words"
		    val numSlaves = pollForSlaves do_ack
		in  numSlaves 
		end
	    fun getReady waiting = 
		let val (waiting,ready, [], []) = partition waiting
		    val (ready,done) = List.partition needsCompile ready
		in  if (null done) 
			then (waiting, ready)        (* no progress *)
		    else getReady (waiting @ ready)  (* some more may have become ready now *)
		end
	    val idle = ref 0
	    fun step (([], [], []) : state)  = (false, NONE)
	      | step (waiting, ready, pending) =
		let val ([],[], pending, done) = partition pending
		    val (waiting,newReady) = getReady waiting
		    val ready = ready @ newReady
		    val numSlaves = waitForSlaves()
		    fun useSlaves slavesLeft (pending, ready, waiting) = 
			(case (slavesLeft, ready) of
			     (0, _) => (true, pending, ready, waiting)
			   | (_, []) =>
				 (* In case a slave finished really fast *)
				 let val ([],[], pending, done) = partition pending
				     val (waiting, ready) = getReady waiting
				 in  (case ready of
					  [] => (idle := 1 + (!idle);
						 chat "  [Idling ";
						 chat (Int.toString (!idle));
						 chat ": ";
						 chat (Int.toString slavesLeft);
						 chat " available slaves.  No ready jobs.  ";
						 chat (Int.toString (length waiting));
						 chat " jobs left.";
						 chat "   Waiting for ";
						 chat_strings 20 pending;
						 chat "]\n";
						 (false,pending,[],waiting))
					| _ => useSlaves slavesLeft (pending, ready, waiting))
				 end
			   | (_, first::rest) =>
				 let fun showSlave name = chat ("  [Calling slave " ^ name ^ 
								" to compile " ^ first ^ "]\n")
				     val _ = markPending first
				     val firstBase = get_base first
				     val imports = get_import_transitive first
				     val importBases = map get_base imports
				     val base = get_base first
				     val _ = Cache.flushSome[base2ui base,
							     base2o base,
							     base2uo base]
				 in  useSlave (showSlave, first::firstBase::importBases);
				     useSlaves (slavesLeft - 1) (first::pending, rest, waiting)
				 end)
		    val (allSlavesUsed, pending, ready, waiting) = 
			           useSlaves numSlaves (pending, ready, waiting)
		in  (allSlavesUsed, (case (waiting,ready,pending) of
					 ([],[],[]) => NONE
				       | _ => SOME (waiting, ready, pending)))
		end
	    fun makeExe(exe, units) = 
		let val _ = showTime "Start linking" 
		    val exe = if exe = "" then (List.last units) ^ ".exe" else exe
		    val unit_set = 
		    List.foldl 
		    (fn (next, set) => 
                     let val import_tr = get_import_transitive next
			 val next_pos = get_position next
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
		in  (chat "Manager calling linker with: ";
		     app (fn s => (print s; print " ")) units;
		     chat "\n";
		     Linker.mk_exe {units = packages, exe_result = exe})
		end
	in  {setup = fn _ => (units, [], []), 
	     step = step, 
	     complete = fn _ => (flushSlaves();
				 case exeOpt of
				     NONE => ()
				   | SOME exe => makeExe (exe, units))}
	end
    fun run args = 
	let val {setup,step,complete} = once args
	    fun loop state = 
		case (step state) of
		    (_,NONE) => complete()
		  | (allSlavesUsed,SOME state) => (if allSlavesUsed then () else Platform.sleep 1.0;
						   loop state)
	in  loop (setup())
	end

    fun graph(mapfile : string) = 
	let val _ = setMapping mapfile
	    val _ = compute_graph()
	    val units = list_units()
	    val dot = mapfile ^ ".dot"
	    val ps = mapfile ^ ".ps"
	    val out = TextIO.openOut dot
	    val _ = TextIO.output (out, "digraph G {\n")
	    val _ = TextIO.output (out, "  size = \"8,8\"\n")
	    val _ = TextIO.output (out, "  rankdir = LR\n")
	    fun do_unit unit = 
		let val children = get_dependent_direct unit
		    fun apper child = TextIO.output (out, "  " ^ unit ^ " -> " ^ child ^";\n")
		in  app apper children
		end
	    val _ = app do_unit units
	    val _ = TextIO.output (out, "}\n")
	    val _ = TextIO.closeOut out
	    val _ = chat ("Generated " ^ dot ^ ".\n")
	    val _ = OS.Process.system ("/afs/cs/user/swasey/bin/dot -Tps " ^ dot ^ " -o " ^ ps)
	    val _ = chat ("Generated " ^ ps ^ ".\n")
	in  ()
	end
    fun purge(mapfile : string) =
	let val _ = setMapping mapfile
	    val units = list_units()
	    val _ = (print "Purging "; print mapfile; print "\n")
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
end

structure Manager :> MANAGER = 
struct

  val error = fn s => Util.error "pmanager.sml" s
  open Help

  val slave = Slave.run
  fun helper runner (mapfile : string, cs : bool, os : string option, srcs : string list) =
	let val _ = if !(Stats.tt "Reset stats between calls") then Stats.clear_stats() else ()
	in  (case (cs, os) of
		 (false, NONE)   => runner(mapfile, srcs, SOME "")
	       | (false, SOME given_exe) => runner(mapfile, srcs, SOME given_exe)
	       | (true,  NONE)   => runner(mapfile, srcs, NONE)
	       | (true,  SOME _) => error "Cannot specify -c and -o");
	    Stats.print_stats()
	end

  fun master mapfile = 
      let val _ = showTime "Starting compilation"
	  val _ = helper Master.run (mapfile, false, NONE, [])
	  val _ = showTime "Finished compilation" 
      in  reshowTimes()
      end
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
	  chat "Started slaves.\n";
	  master mapfile
      end
  fun tilc arg =
      let fun runner args = 
	  let val {setup,step,complete} = Master.once args
	      val _ = Slave.setup()
	      fun loop state = 
		  (case (step state) of
		       (_,NONE) => complete()
		     | (_,SOME state) => (Slave.step(); loop state))
	  in  loop (setup())
	  end
	  val _ = showTime "Starting compilation"
	  val _ = helper runner arg
	  val _ = showTime "Finished compilation"
      in  reshowTimes()
      end
  fun make mapfile = tilc (mapfile, false, NONE, [])

  val purge = Master.purge


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