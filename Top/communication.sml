(*$import Stats COMMUNICATION OS List SplayMapFn SplaySetFn Platform Dirs Delay *)

functor Comm(val slaveTidOpt : int option) :> COMMUNICATION =
struct

    val error = fn s => Util.error "manager.sml" s

    type job = string list
    datatype message = READY                 (* Slave signals readiness *)
		     | ACK_INTERFACE of job  (* Slave signals that interface has compiled *)
		     | ACK_ASSEMBLY of job   (* Slave signals that asm file has compiled but cannot assemble *)
		     | ACK_OBJECT of job     (* Slave signals that object has compiled *)
		     | ACK_ERROR of job      (* Slave signals that an error occurred during given job *)
                     | FLUSH of job          (* Master signals that slaves should flush file cache and set boolean flags -
					        each flag is a pair of the flag name and "true" or "false" *)
	             | REQUEST of job        (* Master requests slave to compile file *)
    val delimiter = #"|"
    val ready = "READY"
    val ack_interface= "ACK_INTERFACE"
    val ack_assembly = "ACK_ASSEMBLY"
    val ack_object = "ACK_OBJECT"
    val ack_error = "ACK_ERROR"
    val flush = "FLUSH"
    val request = "REQUEST"

    fun changeFiles f (platform::unit::absBase::absImportBases) =
	(platform::unit::(f absBase)::(map f absImportBases))
      | changeFiles f _ = error "wrong number of words - bad msg"
	
    fun jobToWords job = changeFiles (Dirs.encode (Dirs.getDirs())) job
    fun wordsToJob words = 
         let 
             val result = changeFiles (Dirs.decode (Dirs.getDirs())) words
         in
             result
         end
	
    fun messageToWords READY = [ready]
      | messageToWords (ACK_INTERFACE job) = ack_interface :: (jobToWords job)
      | messageToWords (ACK_ASSEMBLY job) = ack_assembly :: (jobToWords job)
      | messageToWords (ACK_OBJECT job) = ack_object :: (jobToWords job)
      | messageToWords (ACK_ERROR job) = ack_error :: (jobToWords job)
      | messageToWords (FLUSH job) = flush :: job
      | messageToWords (REQUEST job) = request :: (jobToWords job)
    fun wordsToMessage [] = error "no words - bad msg"
      | wordsToMessage (first::rest) = 
	if (first = ready andalso null rest)
	    then READY
	else if (first = flush)
	    then FLUSH rest
	else if (first = ack_interface)
		 then ACK_INTERFACE (wordsToJob rest)
	else if (first = ack_assembly)
		 then ACK_ASSEMBLY (wordsToJob rest)
	else if (first = ack_object)
		 then ACK_OBJECT (wordsToJob rest)
	else if (first = ack_error)
		 then ACK_ERROR (wordsToJob rest)
        else if (first = request)
		 then REQUEST (wordsToJob rest)
	else error ("strange header word " ^ first ^ " - bad msg")

    local
	val flags = ["PtrWriteBarrier", "FullWriteBarrier", "MirrorGlobal", "MirrorPtrArray"]
	fun getFlag (flag,rest) = let val flagRef = Stats.bool flag
				      val truthValue = Bool.toString (!flagRef)
				  in  rest @ [flag, truthValue]
				  end
    in  fun getFlags() = foldl getFlag [] flags 
    end

    fun doFlags [] = ()
      | doFlags [_] = error "doFlags got list of odd length"
      | doFlags (flagName::truthValue::rest) = 
	let val _ = (print "Setting "; print flagName; print " to "; print truthValue; print "\n")
	    val flagRef = Stats.bool flagName
	    val _ = flagRef := (case (Bool.fromString truthValue) of
				 SOME b => b
			       | _ => error ("doFlags got funny truth value string " ^ truthValue))
	in  doFlags rest
	end

    type channel = (string * string) Delay.value
    fun eq (chan, chan') = (Delay.force chan) = (Delay.force chan')
    fun source chan = let val (from, to) = Delay.force chan
		      in  from
		      end
    fun destination chan = let val (from, to) = Delay.force chan
			   in  to
			   end
    local
	val slaveSelf = 
	    Delay.delay 
	    (fn () =>
	     let val hostName = Platform.hostname()
		 val machineName = (case Util.substring(".cs.cmu.edu",hostName) of
					NONE => hostName
				      | SOME pos => String.substring(hostName,0,pos))
		 val pid = Int.toString(Word32.toInt(Platform.pid()))
		 val tid = (case slaveTidOpt of
				NONE => ""
			      | SOME tid => Int.toString tid)
		     
	     in  machineName ^ "." ^ pid ^ "." ^ tid
	     end)
    in
	val master = "master"
	val toMaster = Delay.delay (fn () => (Delay.force slaveSelf, master))
	val fromMaster = Delay.delay (fn () => (master, Delay.force slaveSelf))
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
	(if OS.FileSys.access(file,[]) andalso
            OS.FileSys.access(file, [OS.FileSys.A_READ])
	    then OS.FileSys.remove file
	 else ())
	    handle e => (print ("WARNING: remove - file " ^ file ^ " exists but then remove failed\n"); ())
			 
    fun erase channel = let val file = channelToName channel
			in  remove file
			end

    fun exists channel = 
	let val filename = channelToName channel
	in  ((OS.FileSys.access(filename,[]))
             andalso
             (OS.FileSys.access(filename,[OS.FileSys.A_READ])
               handle _ => false)
             andalso
	     ((OS.FileSys.fileSize filename > 0)
	    handle e => (print "WARNING: channel disappeared in the middle of exists\n"; 
			 exists channel)))
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


