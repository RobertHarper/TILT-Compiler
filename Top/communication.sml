(*$import Stats COMMUNICATION OS List Platform Dirs Delay Listops Target TopHelp UpdateHelp Compiler Tools Queue *)

structure Comm :> COMMUNICATION =
struct

    val error = fn s => Util.error "communication.sml" s

    structure SS = Substring
    structure Q = Queue
    structure Map = Help.StringMap
	
    datatype message =
	READY					(* Slave signals readiness. *)
      | ACK_INTERFACE of string			(* Slave signals that interface has been compiled.  The job
						   is still in progress.  This message can be skipped. *)
      | ACK_DONE of string * Update.plan	(* Slave gives up on job, informing master what steps are left. *)
      | ACK_ERROR of string			(* Slave signals that an error occurred during job. *)
      | FLUSH_ALL of (Target.platform *		(* Master signals that slave should flush file cache and set boolean flags - *)
		  (string * bool) list)		(* each flag is a pair of the flag name and value.  Currently skipped when
						   master and slave are the same. *)
      | FLUSH of (Paths.unit_paths *		(* Master signals that slave should flush files related to plan *)
		  Update.plan)			(* in anticipation of some other processor doing work. *)
      | REQUEST of (Paths.unit_paths *		(* Master request slave to compile. *)
		    Paths.unit_paths list *
		    Update.plan)
    val ready = "READY"
    val ack_interface= "ACK_INTERFACE"
    val ack_done = "ACK_DONE"
    val ack_error = "ACK_ERROR"
    val flushAll = "FLUSH_ALL"
    val flush = "FLUSH"
    val request = "REQUEST"

    (* We use terminators rather than separators so we can represent the empty list. *)
    val stringTerm = #" " and stringTerm' = "\\032"
    val listTerm = #"|" and listTerm' = "\\124"
	
    (* val messageTerm = #"\n" -- implicit *)
	
    (* quote, unquote : string -> string.
     * We want (unquote (quote s)) = s
     * and (quote s) contains no terminator characters.
     *)
    val quote = String.translate (fn c =>
				  if c = stringTerm then stringTerm'
				  else if c = listTerm then listTerm'
				       else Char.toString c) (* take care of messageTerm *)
    fun unquote s = case String.fromString s
		      of SOME s => s
		       | NONE => error ("malformed string - bad msg: " ^ s)

    (* terminatedFields : (char -> bool) -> substring -> substring list * substring *)
    fun terminatedFields terminator ss =
	let
	    val size = SS.size ss
	    fun findEndpoints (i, start, acc) =
		if i = size then rev acc
		else if terminator (SS.sub (ss, i)) then findEndpoints (i+1, i+1, (start, i)::acc)
		     else findEndpoints (i+1, start, acc)
	    fun toSubstring (a,b) = SS.slice (ss, a, SOME (b - a))
	    val endpoints = findEndpoints (0, 0, nil)
	    val tail = if null endpoints then ss
		       else let val (_, b) = List.last endpoints
			    in  toSubstring (b+1, size)
			    end
	    val fields = map toSubstring endpoints
	in  (fields,tail)
	end

    (* split : char * substring -> substring list *)
    fun split (terminator, ss) =
	if SS.isEmpty ss then nil
	else let val (fields, tail) = terminatedFields (fn c => c = terminator) ss
	     in  if SS.isEmpty tail then fields
		 else error ("terminated string followed by garbage")
	     end
	 
    (* parse : substring -> string list list *)
    fun parse ss =
	let val listFields = split (listTerm, ss)
	    fun toList field = map (unquote o SS.string) (split (stringTerm, field))
	in  map toList listFields
	end

    (* formatHelp : ('a -> string) * char -> 'a list -> string *)
    fun formatHelp (toString, terminator) =
	let
	    val terminator = String.str terminator
	    fun fmt (nil, acc) = String.concat (rev acc)
	      | fmt (a::rest, acc) = fmt (rest, terminator :: toString a :: acc)
	in
	    fn list => fmt (list, nil)
	end
    (* format : string list list -> string *)
    val format = formatHelp (formatHelp (quote, stringTerm), listTerm)

    (* Update.plan <-> string list *)
    val planToWords = map Update.toString
    val wordsToPlan = map Update.fromString

    (* Paths.unit_paths list <-> string list *)
    fun pathsListToWords pathsList =
	let fun toWords paths = [Paths.unitName paths, Paths.sourceFile paths]
	in  List.concat (map toWords pathsList)
	end
    fun wordsToPathsList words =
	let fun conv (nil, acc) = rev acc
	      | conv (_::nil, acc) = error "expected even number of words in encoded unit_paths list - bad msg"
	      | conv (unit::file::rest, acc) =
		let val paths = Paths.sourceUnitPaths {unit=unit, file=file}
		in  conv (rest, paths::acc)
		end
	in  conv (words, nil)
	end

    (* (string * bool) list <-> string list *)
    fun flagsToWords flags = List.concat (map (fn (name, value) => [name, Bool.toString value]) flags)
    fun wordsToFlags words =
	let fun fromString s = case Bool.fromString s
				 of NONE => error "funny boolean value string - bad msg"
				  | SOME b => b
	    fun convert (nil, acc) = rev acc
	      | convert (name::value::rest, acc) = convert (rest, (name, fromString value) :: acc)
	      | convert _ = error "expected even number of words in encoded flag list - bad msg"
	in  convert (words, nil)
	end

    fun encodeAckInterface unit = [[unit]]
    fun decodeAckInterface [[unit]] = unit
      | decodeAckInterface _ = error "bad ack_interface msg"

    fun encodeAckDone (unit, plan) = [[unit], planToWords plan]
    fun decodeAckDone [[unit], plan] = (unit, wordsToPlan plan)
      | decodeAckDone _ = error "bad ack_done msg"

    fun encodeAckError unit = [[unit]]
    fun decodeAckError [[unit]] = unit
      | decodeAckError _ = error "bad ack_error msg"
	
    fun encodeFlushAll (platform, flags) = [[Target.platformName platform], flagsToWords flags]
    fun decodeFlushAll [[platform], flags] = (Target.platformFromName platform, wordsToFlags flags)
      | decodeFlushAll _ = error "bad flush_all msg"

    fun encodeFlush (target, plan) = [pathsListToWords [target], planToWords plan]
    fun decodeFlush [target, plan] = (case wordsToPathsList target
					of [target] => (target, wordsToPlan plan)
					 | _ => error ("bad flush msg"))
      | decodeFlush _ = error ("bad flush msg")

    fun encodeRequest (target, imports, plan) = [pathsListToWords (target::imports), planToWords plan]
    fun decodeRequest [paths, plan] = (case wordsToPathsList paths
					 of (target::imports) => (target, imports, wordsToPlan plan)
					  | _ => error ("bad request msg"))
      | decodeRequest _ = error ("bad request msg")
					     
    fun messageToString message =
	let
	    fun encode (READY) = [[ready]]
	      | encode (ACK_INTERFACE arg) = [ack_interface] :: (encodeAckInterface arg)
	      | encode (ACK_DONE arg) = [ack_done] :: (encodeAckDone arg)
	      | encode (ACK_ERROR arg) = [ack_error] :: (encodeAckError arg)
	      | encode (FLUSH_ALL arg) = [flushAll] :: (encodeFlushAll arg)
	      | encode (FLUSH arg) = [flush] :: (encodeFlush arg)
	      | encode (REQUEST arg) = [request] :: (encodeRequest arg)
	in  format (encode message) ^ "\n"
	end

    fun messageFromString string =
	let
	    val uptoNewline = SS.substring (string, 0, size string - 1)
	    val error = fn s => error (s ^ ": " ^ SS.string uptoNewline)
		
	    fun decode [[first]] =
		if first = ready then READY
		else error ("strange header word")
	      | decode ([first] :: rest) =
		if (first = ack_interface) then ACK_INTERFACE (decodeAckInterface rest)
		else if (first = ack_done) then ACK_DONE (decodeAckDone rest)
		else if (first = ack_error) then ACK_ERROR (decodeAckError rest)
		else if (first = flushAll) then FLUSH_ALL (decodeFlushAll rest)
		else if (first = flush) then FLUSH (decodeFlush rest)
		else if (first = request) then REQUEST (decodeRequest rest)
		else error ("strange header word")
	      | decode _ = error ("bad message")
	    val parsed = parse uptoNewline
	in  decode parsed
	end

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
	let val _ = (Help.chat "Setting "; Help.chat flagName; Help.chat " to ";
		     Help.chat (Bool.toString truthValue); Help.chat "\n")
	    val flagRef = Stats.bool flagName
	    val _ = flagRef := truthValue
	in  doFlags rest
	end

    (* zeroFile : string -> unit *)
    fun zeroFile file = TextIO.closeOut (TextIO.openOut file)

    (* fileExists : string -> bool *)
    fun fileExists filename = ((OS.FileSys.access (filename, nil) andalso
				OS.FileSys.access (filename, [OS.FileSys.A_READ, OS.FileSys.A_WRITE]))
			       handle _ => false)
	
    (* fileNonempty : string -> bool option *)
    fun fileNonempty filename = (SOME (OS.FileSys.fileSize filename > 0)
				 handle _ => NONE)

    (* removeFile : string -> unit *)
    fun removeFile file = OS.FileSys.remove file handle _ => ()

    (* lockFile : string * string -> (unit -> unit) *)
    fun lockFile (old, new) =
	let
	    fun createLink () = ((Posix.FileSys.link {old=old, new=new}; true)
				 handle _ => false)
	    fun loop try =
		if createLink() then ()
		else
		    let val _ = if try = 1 then zeroFile old else () (* Avoid race where master deletes old. *)
			val _ = if try mod 3 = 0 then print ("Waiting for " ^ new ^ "\n")
				else ()
			val _ = Platform.sleep 1.0
		    in  loop (try + 1)
		    end
	    val _ = loop 1		(* acquire lock *)
	    fun unlock () = removeFile new
	in  unlock
	end

    (* readDir : string -> string list *)
    fun scanDir dir =
	let val dirstream = OS.FileSys.openDir dir
	    fun read acc = let val entry = OS.FileSys.readDir dirstream
			   in  if entry = "" then acc
			       else read (entry :: acc)
			   end
	    val contents = read nil
	    val _ = OS.FileSys.closeDir dirstream
	in  contents
	end

    type identity = string
    val master = "master"
    fun slave tidOpt =
	let val hostName = Platform.hostname()
	    val machineName = (case Util.substring(".cs.cmu.edu",hostName) of
				   NONE => hostName
				 | SOME pos => String.substring(hostName,0,pos))
	    val pid = Int.toString(Word32.toInt(Platform.pid()))
	    val tid = (case tidOpt of
			   NONE => ""
			 | SOME tid => "." ^ Int.toString tid)
	in  machineName ^ "." ^ pid ^ tid
	end
    fun compare (id : string, id') = String.compare (id, id')
    fun name (id : string) = id

    type channel = string * string
    fun eq ((src,dest), (src',dest')) = src = src andalso dest = dest
    fun toMaster slave = (slave,master)
    fun fromMaster slave = (master,slave)
    fun reverse (src,dest) = (dest,src)
    fun source (src, _) = src
    fun destination (_, dest) = dest
	
    datatype file_type = BUFFER | LOCK | SENDLOCK | RECVLOCK
    datatype name =
	JUNK_NAME of string
      | CHAN_NAME of string * file_type * channel
	
    local
	val sep = "-to-"
	val sep_length = String.size sep
    in
	(* does not include directory *)
	fun channelToName' (BUFFER,(from,to))   = from ^ sep ^ to ^ "+"
	  | channelToName' (LOCK,(from,to))     = from ^ sep ^ to ^ "#"
	  | channelToName' (SENDLOCK,(from,to)) = from ^ sep ^ to ^ "!"
	  | channelToName' (RECVLOCK,(from,to)) = from ^ sep ^ to ^ "@"

	(* directory has been stripped already *)
	fun nameToChannel' name : name =
	    (case Util.substring (sep, name) of
		 NONE => JUNK_NAME name
	       | SOME pos =>
		     if size name > pos + 2 then
			 let 
			     val from = String.substring(name,0,pos)
			     val to = String.substring(name, pos+sep_length, (size name) - (pos+sep_length) - 1)
			     val chan = (from, to)
			     val last = String.sub(name, size name - 1)
			 in
			     case last
			       of #"+" => CHAN_NAME (name, BUFFER, chan)
				| #"#" => CHAN_NAME (name, LOCK, chan)
				| #"!" => CHAN_NAME (name, SENDLOCK, chan)
				| #"@" => CHAN_NAME (name, RECVLOCK, chan)
				| _ => JUNK_NAME name
			 end
		     else JUNK_NAME name)
    end

    (* nameToChannel : string -> channel option *)
    fun nameToChannel name = (case nameToChannel' name
				of CHAN_NAME (_, BUFFER, chan) => SOME chan
				 | _ => NONE)
	
    val commDir = Dirs.getCommDir o Dirs.getDirs (* : unit -> string *)
    fun channelToName arg = Dirs.relative (commDir(), channelToName' arg)

    type in_channel = channel * message Q.queue	(* channel and buffered messages from peer *)
    type out_channel = channel * string Q.queue	(* channel and pending output *)

    local
	(* opener : 'a queue map * file_type -> channel -> channel * 'a queue *)
	fun opener (queues, lockType) chan =
	    let val lockfile = channelToName (lockType, chan)
		val _ = zeroFile lockfile
		val name = channelToName' (BUFFER, chan)
		val queue = case Map.find (!queues, name)
			      of NONE => let val q = Q.mkQueue ()
					     val _ = queues := Map.insert (!queues, name, q)
					 in  q
					 end
			       | SOME q => q
	    in  (chan, queue)
	    end
	val recvQueues : message Q.queue Map.map ref =
	    ref Map.empty
	val sendQueues : string Q.queue Map.map ref =
	    ref Map.empty
    in
	(* openIn : channel -> in_channel *)
	val openIn = opener (recvQueues, RECVLOCK)

	(* openOut : channel -> out_channel *)
	val openOut = opener (sendQueues, SENDLOCK)

	(* resetBuffers : unit -> unit *)
	fun resetBuffers () = (recvQueues := Map.empty;
			       sendQueues := Map.empty)
    end

    (* exists : channel -> bool *)
    fun exists channel =
	let val filename = channelToName (BUFFER, channel)
	in  fileExists filename andalso
	    (case fileNonempty filename
	       of SOME r => r
		| NONE => (print "WARNING: channel disappeared in the middle of exists\n";
			   exists channel))
	end

    (* canReceive : in_channel -> bool *)
    fun canReceive (channel, queue) = Q.length queue > 0 orelse exists channel

    (* lockChannel : channel * file_type -> (unit -> unit) *)
    fun lockChannel (channel, myLock) =
	let val file = channelToName (LOCK, channel)
	    val myFile = channelToName (myLock, channel)
	in  lockFile (myFile, file)
	end

    (* fillBuffer : in_channel -> unit *)
    fun fillBuffer (channel, queue) =
	let val file = channelToName (BUFFER, channel)
	    val unlock = lockChannel (channel, RECVLOCK)
	    fun wrap f arg = (f arg handle e => (unlock(); raise e))
	    val _ =
		if wrap exists channel then
		    let val stream = wrap TextIO.openIn file
			fun wrap' f arg = (f arg handle e => (TextIO.closeIn stream;
							      unlock();
							      raise e))
			(* We don't remove file unless all messages decode properly. *)
			fun readMessages acc =
			    case TextIO.inputLine stream
			      of "" => rev acc
			   | data => readMessages (messageFromString data :: acc)
			val messages = wrap' readMessages nil
			val _ = wrap TextIO.closeIn stream
			fun saveMessages () = (app (fn msg => Q.enqueue (queue, msg)) messages;
					       removeFile file)
			val _ = wrap saveMessages ()
		    in  ()
		    end
		else ()
	    val _ = unlock()
	in  ()
	end

    (* receive : in_channel -> message option *)
    fun receive (arg as (channel, queue)) =
	let val _ = if Q.isEmpty queue then fillBuffer arg else ()
	in  if Q.isEmpty queue then NONE
	    else SOME (Q.dequeue queue)
	end

    (* closeIn : in_channel -> unit *)
    fun closeIn (arg as (channel, queue)) =
	let val lockFile = channelToName (RECVLOCK, channel)
	    fun clear q = if Q.isEmpty q then ()
			  else (ignore (Q.dequeue q); clear q)
	    val _ = clear queue
	    val _ = removeFile lockFile
	in  ()
	end
    
    (* emptyBuffer : out_channel -> unit *)
    fun emptyBuffer (channel, queue) =
	let val file = channelToName (BUFFER, channel)
	    val unlock = lockChannel (channel, SENDLOCK)
	    fun wrap f arg = (f arg handle e => (unlock(); raise e))
	    val stream = wrap TextIO.openAppend file
	    fun wrap' f arg = (f arg handle e => (TextIO.closeOut stream;
						  unlock();
						  raise e))
	    fun writeData () =
		if Q.isEmpty queue then ()
		else let val data = Q.dequeue queue
		     in  TextIO.output (stream, data);
			 writeData()
		     end
	    val _ = wrap' writeData ()
	    val _ = wrap TextIO.closeOut stream
	    val _ = unlock()
	in  ()
	end

    (* send : message -> out_channel -> unit *)
    fun send message =
	let val data = messageToString message
	    val autoFlush = (case message
			       of FLUSH _ => false
				| _ => true)
	in
	    fn (arg as (channel, queue)) =>
	    (Q.enqueue (queue, data);
	     if autoFlush then emptyBuffer arg else ())
	end

    (* closeOut : out_channel -> unit *)
    fun closeOut (arg as (channel, queue)) =
	let val _ = emptyBuffer arg
	    val lockFile = channelToName (SENDLOCK, channel)
	    val _ = removeFile lockFile
	in  ()
	end

    (* destroyAllChannels : unit -> unit *)
    fun destroyAllChannels () =
	let val commDir = commDir()
	    fun removeName name = removeFile (OS.Path.joinDirFile{dir=commDir, file=name})
	    val names = map nameToChannel' (scanDir commDir)
	    (* Clean up junk *)
	    fun removeJunk (JUNK_NAME name) = (removeName name; false)
	      | removeJunk _ = true
	    val names = List.filter removeJunk names
	    (* Clean up files left over from inactive readers and writers *)
	    fun removeOld' (name, chan) = if exists chan then () else removeName name
	    fun removeOld (CHAN_NAME (name, RECVLOCK, chan)) = (removeOld' (name, reverse chan); false)
	      | removeOld (CHAN_NAME (name, SENDLOCK, chan)) = (removeOld' (name, chan); false)
	      | removeOld _ = true
	    val names = List.filter removeOld names
	    (* Clean up files which an active writer will replace *)
	    fun removeActive (CHAN_NAME (name, LOCK, _)) = (removeName name; false)
	      | removeActive (CHAN_NAME (name, BUFFER, _)) = (removeName name; false)
	      | removeActive _ = true
	    val names = List.filter removeActive names
	    val _ = if null names then ()
		    else error ("destroyAllChannels didn't consider all commdir files")
	    val _ = resetBuffers()
	in  ()
	end
    
    (* findChannels : (channel -> bool) -> channel list *)
    fun findChannels pred =
	let val files = scanDir(commDir())
	    val channels = List.mapPartial nameToChannel files
	in  List.filter (fn ch => pred ch) channels
	end

    (* findSlaves : unit -> identity list *)
    fun findSlaves () =
	let fun isSlave ch = (destination ch = master)
	    val channels = findChannels isSlave
	in  map source channels
	end
end
