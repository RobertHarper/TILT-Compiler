(*$import MANAGER LinkParse LinkIl Compiler Linker OS List SplayMapFn SplaySetFn Platform Dirs Delay *)

(* This is a slave step which gives a result:

     Is there a message from the master?
     (A) Yes, fetch the job j from from the channel. Compile j.  Send acknowledgement. 
         This result in a WORK j.
     (B) No, Is there a message from me to the master?
         (1) Yes, do nothing.  Master has yet to consume this message. Result is WAIT.
         (2) No, send ready message.  Result is READY.

   The master sets up by deleting all channels and then takes master steps which are:
   
     While there are available jobs
         (1) Look for available slave channels and process their acknowledgements
	     by marking compilation jobs as done.  Ready messages require no action.
	 (2) Issue all available jobs to availalbe slaves.
	     (A) If there are more jobs than slaves, then we are PROCESSING fully.
	     (B) Otherwise, we are IDLEing because the slaves are underutilized.
     When there are no more jobs, we are COMPLETEd.
*)

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

    val start = ref (NONE : Time.time option)
    val msgs = ref ([] : string list)
    fun showTime (printTime,str) = 
	let val cur = Time.now()
	    val curString = if (printTime)
				then Date.toString(Date.fromTimeLocal cur)
			    else ""
	    val diff = Time.-(cur, (case !start of
					NONE => error "no start time"
				      | SOME t => t))
	    val diff = Time.toReal diff
	    val diff = (Real.realFloor(diff * 100.0)) / 100.0
	    (* XXX should move this and the space-making function into util *)
	    fun loop i = if (i<0) then "" else " " ^ (loop (i-1))
	    val padding = loop (30 - size str)
	    val msg = (str ^ padding ^ ": " ^ curString ^ "   " ^ 
		       (Real.toString diff) ^ " seconds since start.\n")
	in  msgs := msg :: (!msgs); chat msg
	end
    fun startTime str = (msgs := []; start := SOME(Time.now()); showTime (true,str))
    fun reshowTimes() = (chat "\n\n"; app chat (rev (!msgs)); msgs := []; start := NONE)
end


structure Comm :> COMMUNICATION =
struct

    val error = fn s => Util.error "manager.sml" s

    type job = string list
    datatype message = READY                 (* Slave signals readiness *)
		     | ACK_INTERFACE of job  (* Slave signals that interface has compiled *)
		     | ACK_ASSEMBLY of job   (* Slave signals that asm file has compiled but cannot assemble *)
		     | ACK_OBJECT of job     (* Slave signals that object has compiled *)
		     | ACK_ERROR of job      (* Slave signals that an error occurred during given job *)
                     | FLUSH                 (* Master signals that slaves should flush file cache *)
	             | REQUEST of job        (* Master requests slave to compile file *)
    val delimiter = #"|"
    val ready = "READY"
    val ack_interface= "ACK_INTERFACE"
    val ack_assembly = "ACK_ASSEMBLY"
    val ack_object = "ACK_OBJECT"
    val ack_error = "ACK_ERROR"
    val flush = "FLUSH"
    val request = "REQUEST"

    fun messageToWords READY = [ready]
      | messageToWords (ACK_INTERFACE words) = ack_interface :: words
      | messageToWords (ACK_ASSEMBLY words) = ack_assembly :: words
      | messageToWords (ACK_OBJECT words) = ack_object :: words
      | messageToWords (ACK_ERROR words) = ack_error :: words
      | messageToWords FLUSH = [flush]
      | messageToWords (REQUEST words) = request :: words
    fun wordsToMessage [] = error "no words - bad msg"
      | wordsToMessage (first::rest) = 
	if (first = ready andalso null rest)
	    then READY
	else if (first = flush andalso null rest)
	    then FLUSH
	else if (first = ack_interface)
		 then ACK_INTERFACE rest
	else if (first = ack_assembly)
		 then ACK_ASSEMBLY rest
	else if (first = ack_object)
		 then ACK_OBJECT rest
	else if (first = ack_error)
		 then ACK_ERROR rest
        else if (first = request)
		 then REQUEST rest
	else error ("strange header word " ^ first ^ " - bad msg")

    type channel = (string * string) Delay.value
    fun eq (chan, chan') = (Delay.force chan) = (Delay.force chan')
    fun source chan = let val (from, to) = Delay.force chan
		      in  from
		      end
    fun destination chan = let val (from, to) = Delay.force chan
			   in  to
			   end
    local
	fun computeSelf () =
	    let val selfName = Platform.hostname()
		val selfName = (case Util.substring(".cs.cmu.edu",selfName)
				  of NONE => selfName
				   | SOME pos => String.substring(selfName,0,pos))
		val selfPid = Int.toString(Word32.toInt(Platform.pid()))
	    in
		selfName ^ "." ^ selfPid
	    end
	val self = Delay.delay computeSelf
    in
	val master = "master"
	val toMaster = Delay.delay (fn () => (Delay.force self, master))
	val fromMaster = Delay.delay (fn () => (master, Delay.force self))
	fun isToMaster chan = destination chan = master
	fun isFromMaster chan = source chan = master
    end
    fun reverse chan = let val (from,to) = Delay.force chan
		       in  Delay.delay (fn () => (to, from))
		       end
    val sep = "-to-"
    val sep_length = String.size sep
    val commDir = Dirs.getCommDir o Dirs.getDirs (* : unit -> string *)
    fun channelToName chan = let val (from, to) = Delay.force chan
			     in  Dirs.relative (commDir(), from ^ sep ^ to)
			     end
    (* directory has been stripped already *)
    fun nameToChannel name : channel option = 
	(case Util.substring ("-to-", name) of
	     NONE => NONE
	   | SOME pos => let val last = String.sub(name, size name - 1)
			 in  if (last = #"!" orelse last = #"@")
				 then NONE
			     else let val from = String.substring(name,0,pos)
				      val to = String.substring(name, 
								pos+sep_length, 
								(size name) - 
								(pos+sep_length))
				  in  SOME(Delay.delay (fn () => (from, to)))
				  end
			 end)

    fun remove (file : string) = 
	(if (OS.FileSys.access(file, [OS.FileSys.A_READ]))
	    then OS.FileSys.remove file
	 else ())
	    handle e => (print "WARNING: remove - file exists but then remove failed\n"; ())
			 
    fun erase channel = let val file = channelToName channel
			in  remove file
			end

    fun exists channel = 
	let val filename = channelToName channel
	in  (OS.FileSys.access(filename,[OS.FileSys.A_READ]) andalso
	     (OS.FileSys.fileSize filename > 0))
	    handle e => (print "WARNING: channel disappeared in the middle of exists\n"; 
			 exists channel)
	end

    fun send (channel, message) = 
	let val filename = channelToName channel
	    fun loop [] = ""
	      | loop [str] = str
	      | loop (str::rest) = str ^ (String.str delimiter) ^ (loop rest)
	    val message = loop (messageToWords message)
	    val temp = filename ^ "!"
	    val _ = remove temp
            (* CS: was openAppend, but NT can't do this if file doesn't exist*)
	    val fd = TextIO.openOut temp 
	    val _ = TextIO.output(fd, message)
	    val _ = TextIO.closeOut fd
	    val _ = OS.FileSys.rename{old=temp, new=(channelToName channel)}
	in  ()
	end

    fun receive channel : message option =
	if (exists channel)
	    then let val filename = channelToName channel
		     val temp = filename ^ "@"
		     val _ = remove temp
		     val _ = OS.FileSys.rename{old=filename, new=temp}
			 handle e => (print "rename failed\n"; raise e)
		     val fd = TextIO.openIn temp
			 handle e => (print "open failed\n"; raise e)
		     fun loop acc = 
			 let val isDone = 
			     ((TextIO.endOfStream fd)
			      handle e => (print "Comm.receieve.endOfStream raise exn.  Retrying...\n";
					   false))
			 in  if isDone 
				 then acc
			     else loop (acc ^ (TextIO.inputAll fd))
			 end
		     val string = loop ""
		     val _ = TextIO.closeIn fd
			 handle e => (print "close failed\n"; raise e)
		     val _ = remove temp
		     val words = String.fields (fn c => c = delimiter) string
		 in  SOME (wordsToMessage words)
		 end
	else NONE

    fun findChannels pred =
	let val files = 
	    let val dirstream = OS.FileSys.openDir (commDir())
		fun loop acc = let val cur = OS.FileSys.readDir dirstream
			       in  if (cur = "")
				       then (OS.FileSys.closeDir dirstream; acc)
				   else loop (cur :: acc)
			       end
	    in  loop []
	    end
	    val channels = List.mapPartial nameToChannel files
	in  List.filter (fn ch => (pred ch andalso exists ch)) channels
	end

    fun findToMasterChannels() = findChannels isToMaster
    fun findFromMasterChannels() = findChannels isFromMaster
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
	  ((OS.FileSys.access(file, [OS.FileSys.A_READ]))
	   handle _ => (print ("Warning: OS.FileSys.access on " ^ 
			       file ^ " failed \n"); false))
      in  (if exists
	      then SOME(OS.FileSys.fileSize file, OS.FileSys.modTime file)
	  else NONE)
	      handle e => (print ("File exists for read access but fileSize of morTime failed on " ^ file ^ "\n");
			   raise e)
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
	 | CRC (s,t, _) => SOME (s,t)
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

  (* ----- Compute the latest mod time of a list of exiting files --------- *)
    fun lastModTime [] = (NONE, Time.zeroTime)
      | lastModTime (f::fs) = 
	let
	    val recur_result as (_,fstime) = lastModTime fs 
	    val ftime = modTime f
	in  if (Time.>=(ftime, fstime)) then (SOME f, ftime) else recur_result
	end

  fun tick() =
      let fun mapper (CACHED (s, t, crc, tick, r)) = if (tick <= 1) then PRESENT (s,t)
						     else CACHED(s, t, crc, tick-1, r)
	    | mapper entry = entry
      in  stats := (StringMap.map mapper (!stats))
      end

  fun updateCache (file, newValue) : bool = 
      (case (get file) of
	   CACHED (s, t, crc, tick, result) => 
	       let val stat = CACHED(s, t, crc, tick, newValue)
		   val _ = set(file, stat)
	       in  true
	       end
	 | _ => false)

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
      (size file;
       case (get file) of
	   ABSENT => error ("reading absent file " ^ file)
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
    val error = fn s => Util.error "manager.sml" s
    val stat_each_file = Stats.tt("TimeEachFile")

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
		     chat (Real.toString diff); chat " seconds.  ";
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
		 let val _ = (Help.chat ("  [writing " ^ uiFile);
			      Cache.write (uiFile, partial_ctxt);
			      Help.chat "]\n")
		     val reduced_ctxt = Stats.timer("GCContext",LinkIl.IlContext.gc_context) il_module 
		     val il_module = (reduced_ctxt, partial_ctxt, binds)
		 in il_module
		 end
	   | NONE => error("File " ^ sourcefile ^ " failed to elaborate."))
    
    fun elab_nonconstrained(unit,pre_ctxt,sourcefile,fp,dec,uiFile,least_new_time) =
	case LinkIl.elab_dec(pre_ctxt, fp, dec) of
	    SOME (il_module as (ctxt, partial_ctxt, binds)) =>
		let val _ = (Help.chat ("  [writing " ^ uiFile);
			     Cache.write (uiFile, partial_ctxt);
			     Help.chat "]\n")
		     val reduced_ctxt = Stats.timer("GCContext",LinkIl.IlContext.gc_context) il_module 
		     val il_module = (reduced_ctxt, partial_ctxt, binds)
		in il_module
		end
	  | NONE => error("File " ^ sourcefile ^ " failed to elaborate.")

    (* true indicates .o file generated, false means only .s file generated *)
    fun compile (unit,base,importBases) = 
	let val intFile = Help.base2int base
	    val smlFile = Help.base2sml base
	    val uiFile = Help.base2ui base

	    fun elaborate()=  
		let val ctxt = getContext (map Help.base2ui importBases)
		    val _ = Help.chat ("\n  [Parsing " ^ smlFile ^ "]\n")
		    val (lines,fp, _, dec) = LinkParse.parse_impl smlFile
		    val _ = if (lines > 3000) 
				then (chat "  [Large file: ";
				      chat (Int.toString lines);
				      chat " lines.  Flushing file cache.]\n";
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
		    val imports_uo = map (fn impBase => 
					  (impBase, Cache.crc (Help.base2ui impBase))) 
			importBases
		    val exports_uo = [(base, Cache.crc uiFile)]
		    val uoFile = Linker.mk_uo {imports = imports_uo,
					       exports = exports_uo,
					       base_result = base}
		    val _ = Cache.flushSome [uoFile]
		in  il_module
		end
	    fun generate il_module =
		let (* Continue compilation, generating a platform-dependent .o object file *)
		    val _ = Help.chat ("  [Compiling to assembly file]\n")
		    val sFile = Til.il_to_asm (unit,base, il_module)
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

    fun setup () = (print "Starting slave.\n";
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
	   | SOME Comm.FLUSH => (Cache.flushAll(); chat "Slave received FLUSH.\n"; READY)
	   | SOME (Comm.REQUEST (job as (platform::unit::base::imports))) => 
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
			   val (elaborate, generate) = compile (unit,base,imports)
			   val il_module = (elaborate()
					    handle e => 
						(Comm.send(Comm.toMaster, Comm.ACK_ERROR job);
						 raise e))
			   val diff = Time.-(Time.now(), start)
			   val _ = if (Time.toReal diff > 0.5)
				       then (chat "Sending ACK_INTERFACE: interface took ";
					     chat (Time.toString diff);
					     chat " seconds \n";
					     Comm.send (Comm.toMaster, Comm.ACK_INTERFACE job))
				   else ()
			   val _ = (generate il_module
				    handle e => 
					(Comm.send(Comm.toMaster, Comm.ACK_ERROR job);
					 raise e))
			   val _ = 
			       if (Til.native())
				   then
				       (assemble(unit,base,imports);
					Comm.send (Comm.toMaster, Comm.ACK_OBJECT job))
			       else
				   Comm.send (Comm.toMaster, Comm.ACK_ASSEMBLY job)

			   val _ = if (!stat_each_file)
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


structure Master =
struct
    val error = fn s => Util.error "manager.sml" s
    open Help

    val show_stale = Stats.ff("ShowStale")
    val show_enable = Stats.ff("ShowEnable")
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


    type collapse = {maxWeight : int, maxParents : int, maxChildren : int}
    datatype status = 
	WAITING                 (* Not ready for compilation because some imports are not ready. *)
      | READY of Time.time      (* Ready for compilation. Ready time. *) 
      | PENDING of Time.time    (* Compiling interface. Pending time.*)
      | ASSEMBLING of Time.time (* Compiled interface and assembly.  
				   Locally compiling object file. Pending Time. *)
      | PROCEEDING of Time.time (* Compiled interface.  Compiling object file. Pending Time. *)
      | DONE of Time.time       (* Interface and object are both generated. *)
    local

	val graph = ref (Dag.empty() : {position : int,
					filebase : string,
					status : status ref} Dag.graph)
        val units = ref (true, ([] : string list)) (* kept in reverse order if bool is true *)

	fun lookup unitname =
	    ((Dag.nodeAttribute(!graph,unitname))
	     handle Dag.UnknownNode _ => error ("unit " ^ unitname ^ " missing"))
 
    in 


	fun reset_graph() = (graph := Dag.empty(); 
			     units := (true, []))
	fun add_node(node, size, attribute) = 
	    let val u = (case !units of
			     (false, rev_ls) => (false, node :: rev_ls)
			   | (true, ls) => (false, node :: (rev ls)))
		val _ = units := u
		val _ = Dag.insert_node(!graph,node,size,attribute)
	    in  ()
	    end
	fun add_edge(src,dest) = Dag.insert_edge(!graph,src,dest)


	fun list_units() = (case !units of
				(true, ls) => ls
			      | (false, rev_ls) => 
				    let val ls = rev rev_ls
					val _ = units := (true, ls)
				    in ls
				    end)

	fun get_base unit = #filebase(lookup unit)
	fun get_position unit = #position(lookup unit)
	fun get_status unit = !(#status(lookup unit))
	fun set_status (unit,s) = (#status(lookup unit)) := s

	fun get_import_direct unit = 
	    (Dag.parents(!graph,unit)
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
	fun get_import_transitive unit = 
	    (rev(Dag.ancestors(!graph,unit))
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
	fun get_dependent_direct unit = 
	    (Dag.children(!graph,unit)
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
	fun get_size unit = 
	    (Dag.nodeWeight(!graph,unit)
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
	fun refreshDag g =
	    Dag.refresh g
	    handle Dag.Cycle nodes =>
		   (chat "  Cycle detected in mapfile: ";
		    chat_strings 20 nodes;
		    chat "\n";
		    error "Cycle detected in mapfile")
		 | Dag.UnknownNode node =>
		   let val msg = "  " ^ node ^ " not defined in mapfile."
		   in
		       chat msg;
		       chat "  Used by ";
		       chat_strings 20 (Dag.children' (g, node));
		       chat "\n";
		       error msg
		   end

	fun markReady unit = 
	    (case (get_status unit) of
		 WAITING => set_status(unit,READY (Time.now()))
	       | READY _ => ()
	       | PENDING _ => error "unit was pending; making ready\n"
	       | ASSEMBLING _ => error "unit was assembling; making ready\n"
	       | PROCEEDING _ => error "unit was proceeding; making ready\n"
	       | DONE _ => error "unit was done; making ready\n")

	fun markPending unit = 
	    (case (get_status unit) of
		 WAITING => error "markPending: unit was waiting\n"
	       | READY _ => set_status(unit,PENDING (Time.now()))
	       | PENDING _ => ()
	       | ASSEMBLING _ => error "markPending: unit was assembling\n"
	       | PROCEEDING _ => error "markPending: unit was proceeding\n"
	       | DONE _ => error "markPending: unit was done\n")

	fun enableChildren parent = 
	    let 
		fun enableReady child = 
		    let val imports = get_import_direct child (* no need to check transitively *)
			fun atLeastProceeding unit = (case (get_status unit) of
							  PROCEEDING _ => true
							| DONE _ => true
							| _ => false)
			val ready = Listops.andfold atLeastProceeding imports
		    in  ready andalso 
			(case get_status child of
			     WAITING => (markReady child; true)
			   | _ => false)  (* The child status may be beyond READY since
					     the same parent might call enableChildren twice. *)
		    end
		val enabled = List.filter enableReady (get_dependent_direct parent)
	    in  if (!show_enable andalso (not (null enabled) ))
		    then (chat "  [Compilation of "; chat parent;
			  chat " has enabled these units: ";
			  chat_strings 40 enabled; 
			  chat "]\n")
		else ()
	    end

	fun markProceeding unit = 
	    ((case (get_status unit) of
		 WAITING => error "markProceeding: unit was waiting\n"
	       | READY _ => error "markProceeding: unit was ready\n"
	       | PENDING t => set_status(unit,PROCEEDING t)
	       | PROCEEDING _ => ()
	       | ASSEMBLING t => error "markProceeding: unit was assembling\n"
	       | DONE _ => error "markProceeding: unit was done\n");
	      enableChildren unit)

	fun markAssembling unit = 
	    let val startTime = 
		(case (get_status unit) of
		     WAITING => error "markAssembling: unit was waiting\n"
		   | READY _ => error "markAssembling: unit was ready\n"
		   | PENDING t => (set_status(unit, ASSEMBLING t); t)
		   | PROCEEDING t => (set_status(unit, ASSEMBLING t); t)
		   | ASSEMBLING t => t
		   | DONE _ => error "markAssembling: unit was done\n")
		val _ = enableChildren unit
	    in  startTime
	    end

	(* We must call enableChildren here because units may skip through the Proceeding stage. *)
	fun markDone unit : Time.time = 
	    let val now = Time.now()
		val startTime = 
		    (case (get_status unit) of
			 WAITING => error "markDone: unit was waiting\n"
		       (* May not need compile this unit. *)
		       | READY t => t
		       (* Might have missed the proceding step. *)
		       | PENDING t => t
		       | PROCEEDING t => t
		       | ASSEMBLING t => t
		       | DONE _ => error "markDone: unit was already done\n")
		val _ = set_status(unit,DONE(Time.-(now,startTime)))
		val _ = enableChildren unit
	    in  startTime
	    end


	fun partition units = 
	    let fun folder (unit,(w,r,pe,pr,a,d)) = 
		(case (get_status unit) of
		     WAITING => (unit::w, r, pe, pr, a, d)
		   | READY _ => (w, unit::r, pe, pr, a, d)
		   | PENDING _ => (w, r, unit::pe, pr, a, d)
		   | PROCEEDING _ => (w, r, pe, unit::pr, a, d)
		   | ASSEMBLING _ => (w, r, pe, pr, unit::a, d)
		   | DONE _ => (w, r, pe, pr, a, unit::d))
	    in  foldl folder ([],[],[],[],[],[]) units
	    end

	    
	(* getLibDir : unit -> string *)
	val getLibDir = Dirs.getLibDir o Dirs.getDirs
	    
	(* mapfilePath : string -> string list *)
	fun mapfilePath currentDir = [currentDir, getLibDir()]

	(* readAssociation : string -> (string * string) list *)
	fun readAssociation mapfile = 
	    let
		val dir = OS.Path.dir mapfile
		val findMapfile = Dirs.accessPath (mapfilePath dir, [OS.FileSys.A_READ])
		fun relative file = Dirs.relative (dir, file)
		val is = TextIO.openIn mapfile
		fun dropper s = let val len = size s
				in  String.sub(s,0) = #";" orelse
				    (len >= 2 andalso String.substring(s,0,2) = "//")
				end
		fun loop (n, acc) = 
		    if (TextIO.endOfStream is)
			then rev acc
		    else 
			let val line = TextIO.inputLine is
			in  (case (split_line dropper line) of
				 ["#include", innerMapfile] => 
				     (case findMapfile innerMapfile
					of NONE => error ("Line " ^ (Int.toString n) ^ " of " ^ mapfile ^
							  " includes an unreadable or non-existent file: " ^
							  line ^ "\n")
					 | SOME innerMapfile =>
					    let
						val innerAssociation = readAssociation innerMapfile
					    in
						loop (n+1, (rev innerAssociation) @ acc)
					    end)
			       | [unitname, filebase] => loop (n+1, (unitname, relative filebase) :: acc)
			       | [] => loop (n, acc)
			       | _ => error ("Line " ^ (Int.toString n) ^ 
					     " of " ^ mapfile ^ " is ill-formed: " ^ line ^ "\n"))
			end
		val result = loop (0, [])
		val _ = TextIO.closeIn is
	    in  result
	    end

	(* setMapping : string * bool -> unit *)
	(* Build graph from mapFile.  If getImports is true, add (*$import *) edges. *)
	fun setMapping (mapFile, getImports) =
	    let val _ = Cache.flushAll()
		val _ = reset_graph()
		val association = readAssociation mapFile
		fun mapper (n, (unitname, filebase)) = 
		    let val nodeWeight = Cache.size(base2sml filebase)
			val info = 
			    {position = n,
			     filebase = filebase,
			     status = ref WAITING}
		    in  add_node(unitname, nodeWeight, info)
		    end
		val _ = Listops.mapcount mapper association
		val _ = chat ("Mapfile " ^ mapFile ^ " with " ^ (Int.toString (length association)) 
			      ^ " units processed.\n")
		fun read_import unit = 
		    let val filebase = get_base unit
			val imports = parse_impl_import(base2sml filebase)
			val _ = app (fn import => add_edge(import,unit)) imports
			val _ = set_status(unit, if (null imports) then READY (Time.now()) else WAITING)
		    in  ()
		    end
	    in  if getImports
		    then 
			let val _ = app read_import (list_units());
			    val _ = chat ("Imports read.\n");
			    val _ = refreshDag (!graph);
			    val _= (chat "Dependency graph computed: ";
				    chat (Int.toString (Dag.numNodes (!graph))); print " nodes and ";
				    chat (Int.toString (Dag.numEdges (!graph))); print " edges.\n")
			    val reducedGraph = Dag.removeTransitive (!graph)
			    val _ = graph := reducedGraph
			    val _ = (chat "Reduced dependency graph computed: ";
				     chat (Int.toString (Dag.numNodes (!graph))); print " nodes and ";
				     chat (Int.toString (Dag.numEdges (!graph))); print " edges.\n")
			in  ()
			end
			 
		else ()
	    end


	(* makeGraph' : string * {maxWeight:int, maxParents:int, maxChildren:int} option -> string *)
	(* Generate dot(1) representation of current graph, name based on mapfile. *)
	fun makeGraph'(mapfile : string, collapseOpt) = 
	    let
		val start = Time.now() 
		val dot = mapfile ^ ".dot"
		val out = TextIO.openOut dot
		val g = !graph
		val g = (case collapseOpt of
			     NONE => g
			   | SOME collapse => let val g = Dag.collapse(g, collapse)
						  val _ = chat "Collapsed graph.\n"
					      in  g
					      end)
		val _ = Dag.makeDot{out = out, 
				    graph = g,
				    status = (fn n => (case (get_status n) of
							     DONE _ => Dag.Black
							   | ASSEMBLING _ => Dag.Gray
							   | PROCEEDING _ => Dag.Gray
							   | PENDING _ => Dag.Gray
							   | WAITING => Dag.White
							   | READY _ => Dag.White))}
		val _ = TextIO.closeOut out

		val diff = Time.toReal(Time.-(Time.now(), start))
		val diff = (Real.realFloor(diff * 100.0)) / 100.0
		val _ = chat ("Generated " ^ dot ^ " in " ^ (Real.toString diff) ^ " seconds.\n") 

	    in  dot
	    end

	(* makeGraph' : string * {maxWeight:int, maxParents:int, maxChildren:int} option -> string *)
	(* Generate dot(1) representation of graph in mapfile. *)
	fun makeGraph(mapfile : string, collapseOpt) = 
	    let val _ = setMapping(mapfile, true)
	    in  makeGraph'(mapfile, collapseOpt)
	    end
    end


    local
	val workingAsm = ref ([] : (string * Time.time) list)
	val readySlaves = ref ([] : Comm.channel list)
	val workingSlaves = ref ([] : Comm.channel list)
    in  (* Asynchronously ask for whether there are slaves ready *)
	fun pollForSlaves (do_ack_interface, do_ack_assembly, check_asm, do_ack_object): int = 
	    let val newWorkingAsm = List.filter check_asm (!workingAsm)
		val _ = workingAsm := newWorkingAsm
		val channels = Comm.findToMasterChannels()
		val _ = 
		    app (fn ch => 
			 (case Comm.receive ch of
			      NONE => error ("Ready channel became empty: " ^ 
					     (Comm.source ch) ^ " to " ^ (Comm.destination ch))
			    | SOME Comm.FLUSH => error ("slave " ^ (Comm.source ch) ^ " sent flush")
			    | SOME (Comm.REQUEST _) => error "slave sent request"
			    | SOME Comm.READY => ()
			    | SOME (Comm.ACK_ERROR (_::u::imps)) => 
				  (chat "\n\nSlave "; chat (Comm.source ch); 
				   chat " signalled error during job "; 
				   chat u; chat "\n";
				   error "Slave signalled error")
			    | SOME (Comm.ACK_INTERFACE job) => do_ack_interface (Comm.source ch, job)
			    | SOME (Comm.ACK_ASSEMBLY job) => 
				  let val _ = (workingSlaves := 
					       (Listops.list_diff_eq(Comm.eq, !workingSlaves, 
								     [Comm.reverse ch])))
				      val ut = do_ack_assembly (Comm.source ch, job)
				      val _ = workingAsm := ut :: (!workingAsm)
				  in  ()
				  end
			    | SOME (Comm.ACK_OBJECT job) => 
				  (workingSlaves := 
				   (Listops.list_diff_eq(Comm.eq, !workingSlaves, 
							 [Comm.reverse ch]));
				   do_ack_object (Comm.source ch, job)))) 
		     channels
		val potentialNewSlaves = map Comm.reverse channels 
		val _ = readySlaves := (foldl (fn (ch,acc) => 
						 if ((Listops.member_eq(Comm.eq, ch, acc)) orelse
						     (Listops.member_eq(Comm.eq, ch, !workingSlaves)))
						     then acc else ch::acc) 
					  (!readySlaves) potentialNewSlaves)
	    in  length (!readySlaves)
	    end
	(* Works only if there are slaves available *)
	fun useSlave (showSlave, msg) = 
	    let val (chan::rest) = !readySlaves
		val _ = readySlaves := rest
		val _ = workingSlaves := (chan :: (!workingSlaves))
		val platform = case Til.getTargetPlatform() of
		                  Til.TIL_ALPHA => "dunix"
				| Til.TIL_SPARC => "solaris"
				| _ => error "MLRISC not supported"
	    in  showSlave (Comm.destination chan); 
		Comm.send (chan, Comm.REQUEST (platform::msg))
	    end
	(* Kill active slave channels to restart and send flush slave's file caches *)
	fun resetSlaves() = let val _ = workingAsm := []
				val _ = readySlaves := []
				val _ = workingSlaves := []
				val toMaster = Comm.findToMasterChannels()
				val fromMaster = Comm.findFromMasterChannels()
				val _ = app Comm.erase toMaster
				val _ = app Comm.erase fromMaster
				fun flush toMaster = 
				    let val toSlave = Comm.reverse toMaster
				    in  Comm.send(toSlave,Comm.FLUSH)
				    end
			    in  app flush toMaster
			    end
    end



    (* Compiles the unit either by determining that compilation is not necessary or by calling a slave. 
       This call does not block. *)
    fun needsCompile unitname = (* This unit is in at least a ready state so its imports exist *)
        (case (get_status unitname) of
	      DONE _ => false
	    | _ => 
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
		      val (latest_import_file, latest_import_time) = Cache.lastModTime direct_imports_ui
			  
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
			      (if (!show_stale)
				   then chat ("      [" ^ sourcebase ^ " is stale: " ^
					      uifile ^ " is missing.]\n")
			       else ();
				   false)
			  else if (not dest_uo_exists) then
			      (if (!show_stale)
				   then chat ("      [" ^ sourcebase ^ " is stale: " ^
					      uofile ^ " is missing.]\n")
			       else ();
				   false)
			       else if (not dest_o_exists) then
				   (if (!show_stale)
					then chat ("      [" ^ sourcebase ^ " is stale: " ^
						   ofile ^ " is missing.]\n")
				    else ();
					false)
				    else if sml_changed then
					(if (!show_stale)
					     then chat ("      [" ^ sourcebase ^ " is stale: " ^
							smlfile ^ " newer than objects or interface.]\n")
					 else ();
		      false)
					 else if import_changed then
					     (if (!show_stale)
						  then chat ("      [" ^ sourcebase ^ " is stale: " ^
							     (valOf latest_import_file) ^ " changed.]\n")
					      else ();
						  false)
					      else 
						  true
						  
		      val _ = if fresh
				  then (
				      (* chat ("  [" ^ sourcebase ^ " is up-to-date.]\n"); *)
					markDone unitname; ())
			      else ()

		  in  not fresh
		  end)

    (* platformName : unit -> string *)
    fun platformName (base, ext) = (base ^ (case Til.getTargetPlatform()
					      of Til.TIL_ALPHA => ".alpha"
					       | Til.TIL_SPARC => ".sparc"
					       | _ => error "MLRISC unsupported") ^ ext)
    (* exeName : string * string list -> string *)
    fun exeName ("", units) = platformName (List.last units, ".exe")
      | exeName (exe, _) = exe
    (* exeDroppings : string * string list -> string list *)
    fun exeDroppings (exe, units) =
	let val exeName = exeName (exe, units)
	    fun linkName ext = platformName ("link_" ^ exeName, ext)
	in
	    [exeName, linkName ".o", linkName ".s"]
	end
    (* waiting, ready, pending, assembling, proceeding - done not included *)
    type state = string list * string list * string list * string list * string list
    datatype result = PROCESSING of Time.time * state  (* All slaves utilized *)
                    | IDLE of Time.time * state * int  (* Some slaves idle and there are ready jobs *)
	            | COMPLETE of Time.time 
    fun stateSize ((a,b,c,d,e) : state) = (length a) + (length b) + (length c) + (length d) + (length e)
    fun resultToTime (PROCESSING (t,_)) = t
      | resultToTime (IDLE (t,_,_)) = t
      | resultToTime (COMPLETE t) = t
    fun once (mapfile, srcs,exeOpt) = 
	let val _ = resetSlaves()
	    val _ = setMapping(mapfile, true)
	    val srcs = if (null srcs) then list_units() else srcs
	    fun folder (unit,acc) = 
		let val imports = get_import_transitive unit
		in  StringOrderedSet.cons(unit, foldl StringOrderedSet.cons acc imports)
		end
	    val units = rev (StringOrderedSet.toList (foldl folder StringOrderedSet.empty srcs))

	    val _ = (chat (Int.toString (length units));
		     chat " necessary units: \n")
		     (* chat_strings 20 units *)
	    val _ = showTime (true,"Start compiling files")
	    fun waitForSlaves() = 
		let fun ack_inter (name,(platform::u::_::_)) = 
		          (markProceeding u; 
			   chat ("  [" ^ name ^ " compiled interface of " ^ u ^ "]\n"))
		      | ack_inter _ = error "Acknowledgement message not at least three words"
		    fun ack_asm (name,(platform::u::base::importBases)) = 
		          let val pendingTime = markAssembling u
			      val now = Time.now()
			      val diff = Time.toReal(Time.-(now,pendingTime))
			      val diff = (Real.realFloor(diff * 100.0)) / 100.0
			      val _ = chat ("  [" ^ name ^ " compiled to assembly of " ^ u ^ " in " ^
					    (Real.toString diff) ^ " seconds]\n")
			      val _ = Til.assemble_start base
			  in  (u, now)
			  end
		      | ack_asm _ = error "Acknowledgement message not at least three words"
		    fun check_asm (name,asmTime) =
			let val isDone = Til.assemble_done(get_base name)
			    val _ = 
				if isDone 
				    then 
					let val _ = markDone name
					    val diff = Time.toReal(Time.-(Time.now(),asmTime))
					    val diff = (Real.realFloor(diff * 100.0)) / 100.0
					in  
					    chat ("  [Master locally assembled " ^ name ^ " to object in " ^
						  (Real.toString diff) ^ " seconds]\n")
					end
				else ()
			in  not isDone
			end
		    fun ack_obj (name,(platform::u::_::_)) = 
		          let val pendingTime = markDone u
			      val diff = Time.toReal(Time.-(Time.now(),pendingTime))
			      val diff = (Real.realFloor(diff * 100.0)) / 100.0
			  in  chat ("  [" ^ name ^ " compiled object of " ^ u ^ " in " ^
				 (Real.toString diff) ^ " seconds]\n")
			  end
		      | ack_obj _ = error "Acknowledgement message not at least three words"
		    val numSlaves = pollForSlaves (ack_inter, ack_asm, check_asm, ack_obj)
		in  numSlaves 
		end
	    fun getReady waiting = 
		let fun loop (waiting, ready, done) = 
		    let val (waiting,ready', [], [], [], []) = partition waiting
			val ready = ready @ ready'
			val (ready,done') = List.partition needsCompile ready
			val done = done @ done'
		    in  if (null done') 
			    then  (waiting, ready, done)  (* no progress *)
			else loop (waiting, ready, done)  (* some more may have become ready now *)
		    end
		    val (waiting, ready, done) = loop (waiting, [], [])
		    val _ = if (null done)
				then ()
			    else (chat "  [These files are up-to-date already:";
				  chat_strings 40 done;
				  chat "]\n")
		in  (waiting, ready)
		end

	    val idle = ref 0
	    fun newState(waiting, ready, pending, assembling, proceeding) : state =
		let val ([], [], [], [], assembling, _) = partition assembling
		    val ([], [], [], proceeding, newAssembling, _) = partition proceeding
		    val ([],[], pending, newProceeding, _, _) = partition pending
		    val assembling = assembling @ newAssembling
		    val proceeding = proceeding @ newProceeding
		    val (waiting,newReady) = getReady waiting
		    val ready = ready @ newReady
		in  (waiting, ready, pending, assembling, proceeding)
		end
	    fun stateDone(([],[],[],[],[]) : state) = true
	      | stateDone _ = false
	    fun useSlaves slavesLeft state = 
		let val (state as (_, ready, _, _, _)) = newState state
		in  if (stateDone state)
		      then 	
			  let fun mapper u = 
			      (case get_status u of
				   DONE         t => let val t = Time.toReal t
						     in if (t>=1.0) then SOME(u,t) else NONE
						     end
				 | READY        t => (print "Ready\n";     NONE)
				 | PENDING      t => (print "Pending\n";   NONE)
				 | ASSEMBLING   t => (print "Assembling\n"; NONE)
				 | PROCEEDING   t => (print "Proceeding\n"; NONE)
				 | WAITING => error "Unit still waiting for compilation!")
			      val unsorted = List.mapPartial mapper units
			      fun greater ((_,x),(_,y)) = x > (y : real)
			      val sorted = ListMergeSort.sort greater unsorted
			      val _ = chat "------- Times to compile files in ascending order -------\n"
			      val (underOne,overOne) = List.partition (fn (_,t) => t < 1.0) sorted
			      val (underTen, overTen) = List.partition (fn (_,t) => t < 10.0) overOne
			      val (underThirty, overThirty) = List.partition (fn (_,t) => t < 30.0) overOne
			      val _ = (print (Int.toString (length underOne));
				       print " files under 1.0 second.\n")
			      val _ = (print (Int.toString (length underTen));
				       print " files from 1.0 to 10.0 seconds.\n")
			      val _ = (print (Int.toString (length overTen));
				       print " files from 10.0 to 30.0 seconds:  ";
				       chat_strings 40 (map #1 overThirty); print "\n")
			      val _ = (print (Int.toString (length overThirty));
				       print " files over 30.0 seconds:\n")
			      val _ = app (fn (unit,t) => 
					   let val t = (Real.realFloor(t * 100.0)) / 100.0
					     in  chat ("  " ^ unit ^ 
						       (Util.spaces (20 - (size unit))) ^
						       " took " ^ 
						       (Real.toString t) ^ " seconds.\n")
					     end) overThirty
			    in  COMPLETE (Time.now())
			    end
		    else
			(case (slavesLeft, ready) of
			     (0, _) => PROCESSING (Time.now(),state)
			   | (_, []) => IDLE(Time.now(),state,slavesLeft)
			   | (_, first::rest) =>
				 let fun showSlave name = chat ("  [Calling " ^ name ^ 
								" to compile " ^ first ^ "]\n")
				     val _ = markPending first
				     val firstBase = get_base first
				     val imports = get_import_transitive first
				     val importBases = map get_base imports
				     val base = get_base first
				     val _ = Cache.flushSome[base2ui base,
							     base2s base,
							     base2o base,
							     base2uo base]
				     val (waiting, _, pending, assembling, proceeding) = state
				     val state = (waiting, rest, first::pending, assembling, proceeding)
				 in  useSlave (showSlave, first::firstBase::importBases);
				     useSlaves (slavesLeft - 1) state
				 end)
		end
	    val initialState : state = (units, [], [], [], [])
	    fun step (state : state) = 
		let val state = newState state
		in  if (stateDone state)
			then COMPLETE(Time.now())
		    else
			let val numSlaves = waitForSlaves()
			in  useSlaves numSlaves state
			end
		end
	    (* makeExe : string * string list -> unit *)
	    fun makeExe(exe, units) = 
		let val _ = showTime (true,"Start linking" )
		    val exe = exeName (exe, units)
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
		     chat_strings 30 units;
		     chat "\n";
		     Linker.mk_exe {units = packages, exe_result = exe})
		end
	in  {setup = fn _ => initialState,
	     step = step, 
	     complete = fn _ => (case exeOpt of
				     NONE => ()
				   | SOME exe => makeExe (exe, units))}
	end
    fun run (args as (mapfile,_,_)) = 
	let val {setup,step = step : state -> result,complete} = once args
	    val idle = ref 0
	    val initialState = setup()
	    val lastGraphTime = ref (Time.now())
	    val lastIdleTime = ref NONE
	    val last = ref (PROCESSING(Time.now(),initialState))
	    fun loop state =
	      let val prev = !last
		  val cur = step state
		  val _ = last := cur
		  val thisTime = resultToTime cur
		  val diff = Time.toReal(Time.-(thisTime,!lastGraphTime))
	      in
		(case cur of
		    COMPLETE _ => complete()
		  | PROCESSING (t,state) => 
			let val _ = (case !lastIdleTime of
					 NONE => ()
				       | SOME t' => let val diff = Time.toReal(Time.-(t,t'))
							val diff = (Real.realFloor(diff * 100.0)) / 100.0
							val _ = lastIdleTime := NONE
						    in  chat ("  [Idled for " ^ (Real.toString diff) ^ 
							      " seconds.]\n")
						    end)
			    val _ = (case prev of
					 PROCESSING _ => ()
				       | _ => chat "  [All processors working!]\n")
			    val _ = if (diff > 30.0)
					then 
					    let val left = stateSize state
					    in  (makeGraph'(mapfile,NONE); 
						 showTime (false, "CheckPoint (" ^ (Int.toString left) ^ " files left)");
						 lastGraphTime := (Time.now()))
					    end
				    else ()
			in  Platform.sleep 0.1;
			    loop state
			end
		  | IDLE(t, state as (waiting, ready, pending, assembling, proceeding), numIdle) =>
			((case !lastIdleTime of
			      NONE => lastIdleTime := SOME t
			    | _ => ());
			 (case prev of
			      IDLE _ => ()
			    | _ => (chat "  [Idling ";
				    chat (Int.toString (!idle));
				    chat ": ";
				    chat (Int.toString numIdle);
				    chat " ready slaves.  ";
				    chat (Int.toString (length waiting));
				    chat " waiting jobs.\n";
				    chat "     Waiting for interfaces of ";
				    chat_strings 20 pending; chat ".";
				    if (null proceeding)
					then ()
				    else (chat "\n     Waiting for objects of ";
					  chat_strings 20 proceeding; chat ".");
				    chat "]\n";
				    if (diff > 15.0)
					then 
					    let val left = stateSize state
					    in  (makeGraph'(mapfile,NONE); 
						 showTime (false,"CheckPoint (" ^ (Int.toString left) ^ " files left)");
						 lastGraphTime := (Time.now()))
					    end
				    else ();
				    Platform.sleep 0.1));
			   loop state))
	      end
	in loop initialState
	end


    fun makeGraphShow(mapfile : string, collapse) = 
	let val dot = makeGraph (mapfile, collapse)
	    val ps = mapfile ^ ".ps"
	    val _ = OS.Process.system ("dot -Tps " ^ dot ^ " -o " ^ ps)
	    val _ = chat ("Generated " ^ ps ^ ".\n")
	    val _ = OS.Process.system ("gv " ^ ps ^ "&")
	    val _ = chat ("Invoked gv on " ^ ps ^ ".\n")
	in  ps
	end

    fun purge(mapfile : string) =
	let val _ = setMapping(mapfile, false)
	    val units = list_units()
	    val _ = (print "Purging "; print mapfile; print "\n")
	    val dirs = Dirs.getDirs()
	    fun deletable file = not (Dirs.isSystemFile (dirs, file))
	    fun kill file = if (deletable file andalso
				OS.FileSys.access(file, []) andalso
				OS.FileSys.access(file, [OS.FileSys.A_READ]))
				then OS.FileSys.remove file
			    else ()
	    fun remove unit = 
		let val base = get_base unit
		    val ui = base2ui base
		    val uo = base2uo base
		    val s = base2s base
		    val gz = s ^ ".gz"
		    val obj = base2o base
		in  kill ui; kill uo; kill s; kill gz; kill obj
		end
	    fun purgePlatform platform =
		let val savedPlatform = Til.getTargetPlatform()
		in
		    Til.setTargetPlatform platform;
		    app remove units;
		    Til.setTargetPlatform savedPlatform
		end
	in
	    app purgePlatform [Til.TIL_ALPHA, Til.TIL_SPARC];
	    app kill (exeDroppings ("", units))
	end
end

structure Manager :> MANAGER = 
struct

  val error = fn s => Util.error "manager.sml" s

  (* checkNative : unit -> unit *)
  fun checkNative () = if Til.native() then ()
		       else error "No backend exists for this platform."
			   
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
      let val _ = checkNative()
	  val _ = startTime "Starting compilation"
	  val _ = helper Master.run (mapfile, false, NONE, [])
	  val _ = showTime (true,"Finished compilation")
      in  reshowTimes()
      end
  fun slaves (slaveList : (int * string) list) =
      let val dirs = Dirs.getDirs()
	  val commDir = Dirs.getCommDir dirs
	  val script = Dirs.bin (dirs, "til_slave")
	  fun cmdline (num, count, machine) =
	      String.concat [script, " ", Int.toString num, " ", Int.toString count, " ",
			     machine, " ", commDir, "&"]
	  fun startSlave ncm =
	      (OS.Process.system (cmdline ncm); ())
	  fun loop _ [] = ()
	    | loop cur ((count,machine)::rest) = (startSlave(cur,count,machine); loop (cur+count) rest)
      in  loop 0 slaveList;
	  chat "Started slaves.\n"
      end

  fun tilc arg =
      let val _ = checkNative()
	  fun runner args = 
	  let val {setup,step,complete} = Master.once args
	      val _ = Slave.setup()
	      fun loop state = 
		  (case (step state) of
		       Master.COMPLETE t => complete()
		     | Master.PROCESSING (t,state) => (Slave.step(); loop state)
		     | Master.IDLE (t,state, _) => (Slave.step(); loop state))
	  in  loop (setup())
	  end
	  val _ = startTime "Starting compilation"
	  val _ = helper runner arg
	  val _ = showTime (true,"Finished compilation")
      in  reshowTimes()
      end
  fun make mapfile = tilc (mapfile, false, NONE, [])

  fun purge mapfile = Master.purge mapfile

  fun buildRuntime rebuild = 
      let val _ = checkNative()
	  val command = if rebuild then "cd Runtime; gmake purge; gmake runtime"
			else "cd Runtime; gmake runtime"
      in  if Util.system command then () else error "Error in building runtime"
      end

  (*
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
    | args => 
	let val (mapFile, hasC, oFile, hasAll, srcs) = getArgs args
	    val mapFile = (case mapFile of
			       NONE => "mapfile"
			     | SOME mfile => mfile)
	    val srcs = if hasAll then [] else srcs
	in (tilc(mapFile, hasC, oFile, srcs); 0)
	end
*)
end
