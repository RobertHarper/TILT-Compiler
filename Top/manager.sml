(*$import MANAGER LinkParse LinkIl Compiler Linker OS List SplayMapFn SplaySetFn Platform *)

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

    val msgs = ref ([] : string list)
    fun showTime str = 
	let val msg = (str ^ ": " ^ 
		       (Date.toString(Date.fromTimeLocal(Time.now()))) ^ ".\n")
	in  msgs := msg :: (!msgs); chat msg
	end
    fun reshowTimes() = (chat "\n\n"; app chat (rev (!msgs)); msgs := [])
end


structure Comm :> COMMUNICATION =
struct

    val error = fn s => Util.error "manager.sml" s

    type job = string list
    datatype message = READY                 (* Slave signals readiness *)
		     | ACK_INTERFACE of job  (* Slave signals that interface has compiled *)
		     | ACK_OBJECT of job     (* Slave signals that object has compiled *)
		     | ACK_ERROR of job      (* Slave signals that an error occurred during given job *)
                     | FLUSH                 (* Master signals that slaves should flush file cache *)
	             | REQUEST of job        (* Master requests slave to compile file *)
    val delimiter = #"|"
    val ready = "READY"
    val ack_interface= "ACK_INTERFACE"
    val ack_object = "ACK_OBJECT"
    val ack_error = "ACK_ERROR"
    val flush = "FLUSH"
    val request = "REQUEST"

    fun messageToWords READY = [ready]
      | messageToWords (ACK_INTERFACE words) = ack_interface :: words
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
	else if (first = ack_object)
		 then ACK_OBJECT rest
	else if (first = ack_error)
		 then ACK_ERROR rest
        else if (first = request)
		 then REQUEST rest
	else error ("strange header word " ^ first ^ " - bad msg")

    type channel = string * string
    local
	val SOME selfName = OS.Process.getEnv "HOST"
	val selfName = (case Util.substring(".cs.cmu.edu",selfName) of
			    NONE => error "cannot strip of realm name .cs.cmu.edu"
			  | SOME pos => String.substring(selfName,0,pos))
	val selfPid = Word32.toInt(Platform.pid())
	val self = selfName ^ "." ^ (Int.toString selfPid)
    in  val toMaster = (self, "master")
	val fromMaster = ("master", self)
	fun isToMaster (from,to) = to = "master"
    end

    fun source(from, to) = from
    fun destination(from, to) = to
    fun reverse(from, to) = (to, from)
    fun channelToName(from, to) = from ^ "->" ^ to
    fun nameToChannel name : channel option = 
	(case Util.substring ("->", name) of
	     NONE => NONE
	   | SOME pos => if (String.sub(name,0) = #"!")
			     then NONE
			 else let val from = String.substring(name,0,pos)
				  val to = String.substring(name, pos+2, (size name) - (pos + 2))
			      in  SOME(from, to) 
			      end)

    fun remove (file : string) = if (OS.FileSys.access(file, [OS.FileSys.A_READ]))
				     then OS.FileSys.remove file
				 else ()
    fun erase channel = let val file = channelToName channel
			in  remove file
			end
    fun exists channel = OS.FileSys.access(channelToName channel,[])


    fun send (channel, message) = 
	let fun loop [] = ""
	      | loop [str] = str
	      | loop (str::rest) = str ^ (String.str delimiter) ^ (loop rest)
	    val message = loop (messageToWords message)
	    val temp = "!" ^ (channelToName channel)
	    val _ = remove temp
	    val fd = TextIO.openAppend temp
	    val _ = TextIO.output(fd, message)
	    val _ = TextIO.closeOut fd
	    val _ = OS.FileSys.rename{old=temp, new=(channelToName channel)}
	in  ()
	end

    fun receive channel : message option =
	if (exists channel)
	    then let val fd = TextIO.openIn (channelToName channel)
		     fun loop acc = if (TextIO.endOfStream fd)
					then acc
				    else loop (acc ^ (TextIO.inputAll fd))
		     val string = loop ""
		     val _ = TextIO.closeIn fd
		     val _ = erase channel
(*		     val _ = (print "XXX received from "; print (source channel);
			      print string; print "\n") *)
		     val words = String.fields (fn c => c = delimiter) string
		 in  SOME (wordsToMessage words)
		 end
	else NONE


    fun findToMasterChannels() =
	let val files = 
	    let val dirstream = OS.FileSys.openDir "."
		fun loop acc = let val cur = OS.FileSys.readDir dirstream
			       in  if (cur = "")
				       then (OS.FileSys.closeDir dirstream; acc)
				   else loop (cur :: acc)
			       end
	    in  loop []
	    end
	    val channels = List.mapPartial nameToChannel files
	in  List.filter isToMaster channels
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
	   handle _ => (print ("Warning: OS.FileSys.access on " ^ 
			       file ^ " failed \n"); false))
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
	    val _ = print ("XXX done reading context file " ^ file ^ "\n")
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


    fun compile (unit,base,importBases) = 
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
			val _ = Help.chat ("  [Warning: constraints currently coerce.  ")
			val _ = Help.chat ("Not compatiable with our notion of freshness.]\n")
			val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " with constraint]\n"  )
		    in elab_constrained(unit,ctxt,smlFile,fp,dec,fp2,specs,uiFile,Time.zeroTime)
		    end
		else let val _ = Help.chat ("  [Elaborating " ^ smlFile ^ " non-constrained]\n")
		     in elab_nonconstrained(unit,ctxt,smlFile,fp,dec,uiFile,Time.zeroTime)
		     end
	in  fn () =>
	    let (* Continue compilation, generating a platform-dependent .o object file *)
		val _ = Help.chat ("  [Compiling to object file ...")
		val oFile = Til.compile (unit,base, il_module)
		val _ = Cache.flushSome [oFile]
		val _ =  Help.chat "]\n"
		    
		(* Generate a .uo file containing the checksum for the .o file *)
		val _ = Help.chat ("  [Creating .uo file ...")
		val imports_uo = map (fn impBase => (impBase, Cache.crc (Help.base2ui impBase))) 
		                 importBases
		val exports_uo = [(base, Cache.crc uiFile)]
		val uoFile = Linker.mk_uo {imports = imports_uo,
					   exports = exports_uo,
					   base_result = base}
		val _ = Cache.flushSome [uoFile]
		val _ = Help.chat "]\n"
		
	    (* Print some diagnostic information *)
		val _ = if (!stat_each_file)
			    then (OS.Process.system ("size " ^ oFile); 
				  Stats.print_timers();
				  Stats.clear_stats())
			else ()
	    in  ()
	    end
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
	   | SOME Comm.FLUSH => (Cache.flushAll(); READY)
	   | SOME (Comm.REQUEST (job as (unit::base::imports))) => 
		       (* It's okay for the first acknowledgement to be missed. 
			  In fact, we skip the acknowledgement if the expected compilation 
			  time was small to avoid communication traffic. *)
		       let 
			   val start = Time.now()
			   val rest = compile (unit,base,imports)
				       handle e => 
					   (Comm.send(Comm.toMaster, Comm.ACK_ERROR job);
					    raise e)
			   val diff = Time.-(Time.now(), start)
			   val _ = if (Time.toReal diff > 0.5)
				       then (print "XXX sending ACK_INTERFACE: interface took ";
					     print (Time.toString diff);
					     print " seconds \n";
					     Comm.send (Comm.toMaster, Comm.ACK_INTERFACE job))
				   else print "XXX skipping ACK_INTERFACE\n"
			   val _ = rest()
			             handle e => 
					 (Comm.send(Comm.toMaster, Comm.ACK_ERROR job);
					  raise e)
			   val _ = Comm.send (Comm.toMaster, Comm.ACK_OBJECT job)
		       in  WORK unit
		       end
	   | SOME (Comm.REQUEST _) => error "Slave got a funny request")

    fun run() = 
	let val _ = setup()
	    val last = ref READY
	    fun loop() = 
		let val prev = !last
		    val cur = step()
		    val _ = last := prev
		in  (case cur of
			 WORK job => chat ("Slave compiled " ^ job ^ "\n")
		       | WAIT => let val pause = (case prev of
						      WORK _ => 0.1
						    | _ => 0.5)
				 in  chat ("Slave waiting for master to send work.\n");
				     Platform.sleep pause
				 end
		       | READY => let val pause = (case prev of
						       WORK _ => 0.1
						     | _ => 0.5)
				  in  chat ("Slave waiting for master to send work.\n");
				      Platform.sleep pause
				  end);
		    loop()
		end
	in loop()
	end

end


structure Master =
struct
    val error = fn s => Util.error "pmanager.sml" s
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
      | PROCEEDING of Time.time (* Compiled interface.  Compiling object file. Pending Time. *)
      | DONE                    (* Interface and object are both generated. *)
    local

	val graph = ref (Dag.empty() : {position : int,
					filebase : string,
					status : status ref} Dag.graph)
        val units = ref (true, ([] : string list)) (* kept in reverse order if bool is true *)

	fun lookup unitname =
	    ((Dag.nodeAttribute(!graph,unitname))
	     handle Dag.UnknownNode => error ("unit " ^ unitname ^ " missing"))
 
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
	     handle Dag.UnknownNode => error ("unit " ^ unit ^ " missing"))
	fun get_import_transitive unit = 
	    (rev(Dag.ancestors(!graph,unit))
	     handle Dag.UnknownNode => error ("unit " ^ unit ^ " missing"))
	fun get_dependent_direct unit = 
	    (Dag.children(!graph,unit)
	     handle Dag.UnknownNode => error ("unit " ^ unit ^ " missing"))
	fun get_size unit = 
	    (Dag.nodeWeight(!graph,unit)
	     handle Dag.UnknownNode => error ("unit " ^ unit ^ " missing"))
	fun get_depth unit = 
	    (Dag.pathWeight(!graph,unit)
	     handle Dag.UnknownNode => error ("unit " ^ unit ^ " missing"))

	fun markReady unit = 
	    (case (get_status unit) of
		 WAITING => set_status(unit,READY (Time.now()))
	       | READY _ => ()
	       | PENDING _ => error "unit was pending; making ready\n"
	       | PROCEEDING _ => error "unit was proceeding; making ready\n"
	       | DONE => error "unit was done; making ready\n")

	fun markPending unit = 
	    (case (get_status unit) of
		 WAITING => error "markPending: unit was waiting\n"
	       | READY _ => set_status(unit,PENDING (Time.now()))
	       | PENDING _ => ()
	       | PROCEEDING _ => error "markPending: unit was proceeding\n"
	       | DONE => error "markPending: unit was done\n")

	fun enableChildren parent = 
	    let 
		fun enableReady child = 
		    let val imports = get_import_direct child (* no need to check transitively *)
			fun atLeastProceeding unit = (case (get_status unit) of
							  PROCEEDING _ => true
							| DONE => true
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
	       | DONE => error "markProceeding: unit was done\n");
	      enableChildren unit)

	(* We must call enableChildren here because units may skip through the Proceeding stage. *)
	fun markDone unit : Time.time = 
	    let val startTime = 
		(case (get_status unit) of
		     WAITING => error "markDone: unit was waiting\n"
		   | READY t =>   (set_status(unit,DONE); t) (* May not need compile this unit. *)
		   | PENDING t => (set_status(unit,DONE); t) (* Might have missed the proceding step. *)
		   | PROCEEDING t => (set_status(unit,DONE); t)
		   | DONE => (Time.now()))
		val _ = enableChildren unit
	    in  startTime
	    end


	fun partition units = 
	    let fun folder (unit,(w,r,pe,pr,d)) = 
		(case (get_status unit) of
		     WAITING => (unit::w, r, pe, pr, d)
		   | READY _ => (w, unit::r, pe, pr, d)
		   | PENDING _ => (w, r, unit::pe, pr, d)
		   | PROCEEDING _ => (w, r, pe, unit::pr, d)
		   | DONE => (w, r, pe, pr, unit::d))
	    in  foldl folder ([],[],[],[],[]) units
	    end


	fun setMapping (mapFile, getImports) =
	    let val _ = Cache.flushAll()
		val is = TextIO.openIn mapFile
		val _ = reset_graph()
		fun loop n = 
		    if (TextIO.endOfStream is)
			then ()
		    else 
			let fun dropper s = String.sub(s,0) = #"#"
			    val line = TextIO.inputLine is
			in  (case (split_line dropper line) of
				 [unitname, filebase] => 
				     let val nodeWeight = Cache.size(base2sml filebase)
					 val info = 
					     {position = n,
					      filebase = filebase,
					      status = ref WAITING}
					 val _ = add_node(unitname, nodeWeight, info)
				     in  loop (n+1)
				     end
			       | [] => loop n
			       | _ => error ("ill-formed map line: " ^ line))
			end
		val _ = loop 0
		val _ = TextIO.closeIn is
		val _ = chat ("Mapfile " ^ mapFile ^ " read.\n")
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
			    val _ = Dag.refresh (!graph);
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


	fun makeGraph(mapfile : string, collapseOpt) = 
	    let val _ = setMapping(mapfile, true)
		val dot = mapfile ^ ".dot"
		val out = TextIO.openOut dot
		val g = !graph
		val g = (case collapseOpt of
			     NONE => g
			   | SOME collapse => let val g = Dag.collapse(g, collapse)
						  val _ = chat "Collapsed graph.\n"
					      in  g
					      end)
		val _ = Dag.makeDot(out, g)
		val _ = TextIO.closeOut out
		val _ = chat ("Generated " ^ dot ^ ".\n")
	    in  dot
	    end

    end


    local
	val readySlaves = ref ([] : Comm.channel list)
	val workingSlaves = ref ([] : Comm.channel list)
    in  (* Asynchronously ask for whether there are slaves ready *)
	fun pollForSlaves (do_ack_interface, do_ack_object): int = 
	    let val channels = Comm.findToMasterChannels()
		val _ = 
		    app (fn ch => 
			 (case Comm.receive ch of
			      NONE => error "channel cannot be empty now"
			    | SOME Comm.FLUSH => error "slave sent flush"
			    | SOME (Comm.REQUEST _) => error "slave sent request"
			    | SOME Comm.READY => ()
			    | SOME (Comm.ACK_ERROR jobs) => 
				  (chat "\n\nSlave "; chat (Comm.source ch); 
				   chat " signalled error during job "; 
				   chat_strings 30 jobs; chat "\n";
				   error "Slave signalled error")
			    | SOME (Comm.ACK_INTERFACE job) => do_ack_interface (Comm.source ch, job)
			    | SOME (Comm.ACK_OBJECT job) => 
				  (workingSlaves := 
				   (Listops.list_diff_eq(op =, !workingSlaves, 
							 [Comm.reverse ch]));
				   do_ack_object (Comm.source ch, job)))) channels
		val potentialNewSlaves = map Comm.reverse channels 
		val _ = readySlaves := (foldl (fn (ch,acc) => 
						 if ((Listops.member_eq(op =, ch, acc)) orelse
						     (Listops.member_eq(op =, ch, !workingSlaves)))
						     then acc else ch::acc) 
					  (!readySlaves) potentialNewSlaves)
(*
		val _ = (chat "XXX poll ready slaves = ";
			 chat_strings 10 (map Comm.destination (!readySlaves));
			 chatnt "\n")
*)
	    in  length (!readySlaves)
	    end
	(* Works only if there are slaves available *)
	fun useSlave (showSlave, msg) = 
	    let val (chan::rest) = !readySlaves
		val _ = readySlaves := rest
		val _ = workingSlaves := (chan :: (!workingSlaves))
	    in  showSlave (Comm.destination chan); 
		Comm.send (chan, Comm.REQUEST msg)
	    end
	(* Kill active slave channels to restart and send flush slave's file caches *)
	fun resetSlaves() = let val _ = readySlaves := []
				val _ = workingSlaves := []
				val channels = Comm.findToMasterChannels()
				fun apper ch = let val ch' = Comm.reverse ch
					       in  Comm.send(ch',Comm.FLUSH);
						   Comm.erase ch
					       end
			    in  app apper channels
			    end
    end



    (* Compiles the unit either by determining that compilation is not necessary or by calling a slave. 
       This call does not block. *)
    fun needsCompile unitname = (* This unit is in at least a ready state so its imports exist *)
        if (get_status unitname = DONE)
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
			then (chat ("  [" ^ sourcebase ^ " is up-to-date.]\n");
			      markDone unitname; ())
		    else ()

	in  not fresh
	end

    (* waiting, ready, pending, proceeding *)
    type state = string list * string list * string list * string list
    datatype result = PROCESSING of state        (* All slaves utilized *)
                    | IDLE of state * int        (* Some slaves not utilized and there are ready jobs *)
	            | COMPLETE

    fun once (mapfile, srcs,exeOpt) = 
	let val _ = resetSlaves()
	    val _ = setMapping(mapfile, true)
	    val srcs = if (null srcs) then list_units() else srcs
	    fun folder (unit,acc) = 
		let val imports = get_import_transitive unit
		in  StringOrderedSet.cons(unit, foldl StringOrderedSet.cons acc imports)
		end
	    val units = rev (StringOrderedSet.toList (foldl folder StringOrderedSet.empty srcs))
	    val _ = (chat "Computed all necessary units: \n";
		     chat_strings 20 units; print "\n")
	    val _ = showTime "Start compiling files"
	    fun waitForSlaves() = 
		let fun ack_inter (name,(u::_::_)) = 
		          (markProceeding u; 
			   chat ("  [" ^ name ^ " compiled interface of " ^ u ^ "]\n"))
		      | ack_inter _ = error "Acknowledgement message not at least two words"
		    fun ack_obj (name,(u::_::_)) = 
		          let val pendingTime = markDone u
			      val diff = Time.toReal(Time.-(Time.now(),pendingTime))
			      val diff = (Real.realFloor(diff * 100.0)) / 100.0
			  in  chat ("  [" ^ name ^ " compiled object of " ^ u ^ " in " ^
				 (Real.toString diff) ^ " seconds]\n")
			  end
		      | ack_obj _ = error "Acknowledgement message not at least two words"
		    val numSlaves = pollForSlaves (ack_inter, ack_obj)
		in  numSlaves 
		end
	    fun getReady waiting = 
		let val (waiting,ready, [], [], []) = partition waiting
		    val (ready,done) = List.partition needsCompile ready
		in  if (null done) 
			then (waiting, ready)        (* no progress *)
		    else getReady (waiting @ ready)  (* some more may have become ready now *)
		end
	    val idle = ref 0
	    fun newState(waiting, ready, pending, proceeding) =
		let val ([], [], [], proceeding, _) = partition proceeding
		    val ([],[], pending, newProceeding, _) = partition pending
		    val proceeding = proceeding @ newProceeding
		    val (waiting,newReady) = getReady waiting
		    val ready = ready @ newReady
		in  (waiting, ready, pending, proceeding)
		end
	    fun stateDone(([],[],[],[]) : state) = true
	      | stateDone _ = false
	    fun useSlaves slavesLeft state = 
		let val (state as (_, ready, _, _)) = newState state
		in  if (stateDone state)
			then COMPLETE
		    else
			(case (slavesLeft, ready) of
			     (0, _) => PROCESSING state
			   | (_, []) => IDLE(state,slavesLeft)
			   | (_, first::rest) =>
				 let fun showSlave name = chat ("  [Calling " ^ name ^ 
								" to compile " ^ first ^ "]\n")
				     val _ = markPending first
				     val firstBase = get_base first
				     val imports = get_import_transitive first
				     val importBases = map get_base imports
				     val base = get_base first
				     val _ = Cache.flushSome[base2ui base,
							     base2o base,
							     base2uo base]
				     val (waiting, _, pending, proceeding) = state
				     val state = (waiting, rest, first::pending, proceeding)
				 in  useSlave (showSlave, first::firstBase::importBases);
				     useSlaves (slavesLeft - 1) state
				 end)
		end
	    fun step state = 
		let val state = newState state
		in  if (stateDone state)
			then COMPLETE
		    else
			let val numSlaves = waitForSlaves()
			in  useSlaves numSlaves state
			end
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
		     chat_strings 30 units;
		     chat "\n";
		     Linker.mk_exe {units = packages, exe_result = exe})
		end
	in  {setup = fn _ => (units, [], [], []), 
	     step = step, 
	     complete = fn _ => (case exeOpt of
				     NONE => ()
				   | SOME exe => makeExe (exe, units))}
	end
    fun run args = 
	let val {setup,step,complete} = once args
	    val idle = ref 0
	    val last = ref (PROCESSING([],[],[],[]))
	    fun loop state = 
	      let val prev = !last
		  val cur = step state
		  val _ = last := cur
	      in		    
		(case cur of
		    COMPLETE => complete()
		  | PROCESSING state => 
			let val _ =
			    (case prev of
				 PROCESSING _ => ()
			       | _ => chat "  [All processors working!]\n")
			in  Platform.sleep 0.5;
			    loop state
			end
		  | IDLE(state as (waiting, ready, pending, proceeding), numIdle) =>
			(if (null ready) then () 
			 else (chat "IDLE but there are ready jobs: ";
			       chat_strings 20 ready;
			       error "IDLE but there are ready jobs");
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
				    Platform.sleep 0.5));
			   loop state))
	      end
	in loop (setup())
	end


    fun makeGraphShow(mapfile : string, collapse) = 
	let val dot = makeGraph (mapfile, collapse)
	    val ps = mapfile ^ ".ps"
	    val _ = OS.Process.system ("/afs/cs/user/swasey/bin/dot -Tps " ^ dot ^ " -o " ^ ps)
	    val _ = chat ("Generated " ^ ps ^ ".\n")
	    val _ = OS.Process.system ("gv " ^ ps ^ "&")
	    val _ = chat ("Invoked gv on " ^ ps ^ ".\n")
	in  ps
	end

    fun purge(mapfile : string) =
	let val _ = setMapping(mapfile, false)
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
	  let val row = num mod 3
	      val col = num div 3
	      val geometry = "80x16+" ^ (Int.toString (col * 300)) ^ "+" ^ (Int.toString (row * 250))
	      val dir = OS.FileSys.getDir()
	      val SOME display = OS.Process.getEnv "DISPLAY"
	      val SOME user = OS.Process.getEnv "USER"
	      val out = TextIO.openOut "startSlave1"
	      val _ = TextIO.output(out, "(set-default-font \"courier7\")\n")
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
		       Master.COMPLETE => complete()
		     | Master.PROCESSING state => (Slave.step(); loop state)
		     | Master.IDLE (state, _) => (Slave.step(); loop state))
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
    | args => 
	let val (mapFile, hasC, oFile, hasAll, srcs) = getArgs args
	    val mapFile = (case mapFile of
			       NONE => "mapfile"
			     | SOME mfile => mfile)
	    val srcs = if hasAll then [] else srcs
	in (tilc(mapFile, hasC, oFile, srcs); 0)
	end

end