(*$import Stats COMMUNICATION OS List Platform Dirs Delay Listops Target TopHelp UpdateHelp Compiler Tools *)

functor Comm(val slaveTidOpt : int option) :> COMMUNICATION =
struct

    val error = fn s => Util.error "communication.sml" s

    datatype message =
	READY					(* Slave signals readiness. *)
      | ACK_INTERFACE of string			(* Slave signals that interface has been compiled.  The job
						   is still in progress.  This message can be skipped. *)
      | ACK_DONE of string * Update.plan	(* Slave gives up on job, informing master what steps are left. *)
      | ACK_ERROR of string			(* Slave signals that an error occurred during job. *)
      | FLUSH of (Target.platform *		(* Master signals that slave should flush file cache and set boolean flags - *)
		  (string * bool) list)		(* each flag is a pair of the flag name and value.  Currently skipped when
						   master and slave are the same. *)
      | REQUEST of (Paths.unit_paths *		(* Master request slave to compile. *)
		    Paths.unit_paths list *
		    Update.plan)
    val ready = "READY"
    val ack_interface= "ACK_INTERFACE"
    val ack_done = "ACK_DONE"
    val ack_error = "ACK_ERROR"
    val flush = "FLUSH"
    val request = "REQUEST"

    val wordsToWord = Listops.toString (fn s => s)
    val wordToWords = fn word => case Listops.fromString SOME word
				   of NONE => error "not a list of words - bad msg"
				    | SOME words => words

    val planToWord = wordsToWord o (map Update.toString)
    val wordToPlan = (map Update.fromString) o wordToWords

    fun ackInterfaceToWords unit = [unit]
    fun wordsToAckInterface (unit :: nil) = unit
      | wordsToAckInterface _ = error "expected unit name - bad msg"

    fun ackDoneToWords (unit, plan) = [unit, planToWord plan]
    fun wordsToAckDone (unit :: plan :: nil) = (unit, wordToPlan plan)
      | wordsToAckDone _ = error ("expected unit name and plan - bad msg")

    val ackErrorToWords = ackInterfaceToWords
    val wordsToAckError = wordsToAckInterface
	
    fun flushToWords (platform, flags) =
	let fun convert (name, value) = [name, Bool.toString value]
	    val flag_words = List.concat (map convert flags)
	in  (Target.platformName platform) :: flag_words
	end
    fun wordsToFlush (platform::flagWords) =
	let fun fromString s = case Bool.fromString s
				 of NONE => error "funny boolean value string - bad msg"
				  | SOME b => b
	    fun convert (nil, acc) = rev acc
	      | convert (name::value::rest, acc) = convert (rest, (name, fromString value) :: acc)
	      | convert _ = error "wrong number of words in encoded flag - bad msg"
	in  (Target.platformFromName platform,
	     convert (flagWords, nil))
	end
      | wordsToFlush _ = error "expected platform - bad msg"

    fun requestToWords (target, imports, plan) =
	let
	    fun pathsToWords paths = [Paths.unitName paths, Paths.sourceFile paths]
	in
	    (planToWord plan) :: (List.concat (map pathsToWords (target :: imports)))
	end
    fun wordsToReqeust (plan :: rest) =
	let fun mkunit (unit, file) = Paths.sourceUnitPaths {unit=unit, file=file}
	    fun convert (nil, acc) = rev acc
	      | convert (unit::file::rest, acc) = convert (rest, (mkunit (unit, file)) :: acc)
	      | convert (_, _) = error "wrong number of words in encoded unit_paths - bad msg"
	in
	    case convert (rest, nil)
	      of (target::imports) => (target, imports, wordToPlan plan)
	       | _ => error "expected target - bad msg"
	end
      | wordsToReqeust _ = error "expected plan - bad msg"
	
    fun messageToWords READY = [ready]
      | messageToWords (ACK_INTERFACE arg) = ack_interface :: (ackInterfaceToWords arg)
      | messageToWords (ACK_DONE arg) = ack_done :: (ackDoneToWords arg)
      | messageToWords (ACK_ERROR arg) = ack_error :: (ackErrorToWords arg)
      | messageToWords (FLUSH arg) = flush :: (flushToWords arg)
      | messageToWords (REQUEST arg) = request :: (requestToWords arg)
    fun wordsToMessage [] = error "no words - bad msg"
      | wordsToMessage (first::rest) = 
	if (first = ready andalso null rest)
	    then READY
	else if (first = ack_interface)
		 then ACK_INTERFACE (wordsToAckInterface rest)
	else if (first = ack_done)
		 then ACK_DONE (wordsToAckDone rest)
	else if (first = ack_error)
		 then ACK_ERROR (wordsToAckError rest)
	else if (first = flush)
	    then FLUSH (wordsToFlush rest)
	else if (first = request)
		 then REQUEST (wordsToReqeust rest)
	else error ("strange header word " ^ first ^ " - bad msg")

    val flagsForSlave =
	(map (fn (name, ref_cell, _) => (name, ref_cell)) Target.importantFlags) @
	[("UptoElaborate", Help.uptoElaborate),
	 ("UptoPhasesplit", Help.uptoPhasesplit),
	 ("UptoClosureConvert", Help.uptoClosureConvert),
	 ("UptoRtl", Help.uptoRtl),
	 ("UptoAsm", Help.uptoAsm),
	 ("debug_asm", Tools.debugAsm),
	 ("keep_asm", Help.keepAsm),
	 ("compress_asm", UpdateHelp.compressAsm),
	 ("ManagerChat", Help.chat_ref),
	 ("ManagerVerbose", Help.chatVerbose),
	 ("TimeEachFile", Help.statEachFile),
	 ("makeBackups", Help.makeBackups),
	 ("ShowWrittenContext", Compiler.showWrittenContext),
	 ("WriteUnselfContext", Compiler.writeUnselfContext),
	 ("ShowTools", Tools.showTools)]

    fun getFlags () =
	let
	    (* Also verifies that names correspond to correct flags.  *)
	    fun getFlag (flag_name, flag_ref) =
		if Stats.bool flag_name = flag_ref then (flag_name, !flag_ref)
		else error ("the flag name " ^ flag_name ^ " doesn't map to the expected flag ref")
	in
	    map getFlag flagsForSlave
	end
	
    fun doFlags [] = ()
      | doFlags ((flagName, truthValue)::rest) = 
	let val _ = (print "Setting "; print flagName; print " to "; print (Bool.toString truthValue); print "\n")
	    val flagRef = Stats.bool flagName
	    val _ = flagRef := truthValue
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
	    val message = Listops.toString (fn s => s) (messageToWords message)
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
		     val words = case Listops.fromString SOME string
				   of NONE => error "not a list of strings - bad msg"
				    | SOME L => L
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


