(*
	Many of the access() calls exist to speed things up
	on AFS.
*)
structure Comm :> COMMUNICATION =
struct

    structure Q = Queue
    structure Map = Util.StringMap
    structure B = Blaster
    structure F = Formatter

    val CommDiag = Stats.ff("CommDiag")
    fun msg str = if (!CommDiag) then print str else ()

    val error = fn s => Util.error "communication.sml" s

    fun say (s : string) : unit =
	if !Blaster.BlastDebug then (print s; print "\n") else ()

    (* Q.delete is buggy. *)
    fun empty_queue (q : 'a Q.queue) : unit =
	if Q.isEmpty q then ()
	else (ignore (Q.dequeue q); empty_queue q)


    type job = int * string
    type plan = Update.plan
    type flags = (string * bool) list
    type platform = Target.platform

    datatype message =
	READY
      | ACK_INTERFACE of job
      | ACK_DONE of job * plan
      | ACK_ERROR of job * string
      | FLUSH_ALL of platform * flags
      | FLUSH of job * plan
      | REQUEST of job * plan

    fun blastOutJob (os : B.outstream) (job : job) : unit =
	(say "blastOutJob";
	 B.blastOutPair B.blastOutInt B.blastOutString os job)
    fun blastInJob (is : B.instream) : job =
	(say "blastInJob";
	 B.blastInPair B.blastInInt B.blastInString is)

    fun blastOutFlags (os : B.outstream) (flags : flags) : unit =
	(say "blastOutFlags";
	 B.blastOutList (B.blastOutPair B.blastOutString B.blastOutBool) os flags)
    fun blastInFlags (is : B.instream) : flags =
	(say "blastInFlags";
	 B.blastInList (B.blastInPair B.blastInString B.blastInBool) is)

    fun blastOutMessage (os : B.outstream) (m : message) : unit =
	(say "blastOutMessage";
	 (case m
	    of READY => B.blastOutInt os 0
	     | ACK_INTERFACE job =>
		(B.blastOutInt os 1; blastOutJob os job)
	     | ACK_DONE (job,plan) =>
		(B.blastOutInt os 2; blastOutJob os job;
		 Update.blastOutPlan os plan)
	     | ACK_ERROR (job,msg) =>
		(B.blastOutInt os 3; blastOutJob os job;
		 B.blastOutString os msg)
	     | FLUSH_ALL (platform,flags) =>
		(B.blastOutInt os 4; Target.blastOutPlatform os platform;
		 blastOutFlags os flags)
	     | FLUSH (job, plan) =>
		(B.blastOutInt os 5; blastOutJob os job;
		 Update.blastOutPlan os plan)
	     | REQUEST (job,plan) =>
		(B.blastOutInt os 6; blastOutJob os job;
		 Update.blastOutPlan os plan)))
    fun blastInMessage (is : B.instream) : message =
	(say "blastInMessage";
	 (case B.blastInInt is
	    of 0 => READY
	     | 1 => ACK_INTERFACE (blastInJob is)
	     | 2 => ACK_DONE (blastInJob is, Update.blastInPlan is)
	     | 3 => ACK_ERROR (blastInJob is, B.blastInString is)
	     | 4 => FLUSH_ALL (Target.blastInPlatform is, blastInFlags is)
	     | 5 => FLUSH (blastInJob is, Update.blastInPlan is)
	     | 6 => REQUEST (blastInJob is, Update.blastInPlan is)
	     | _ => error "bad message"))

    val (blastOutMessages,blastInMessages') =
	B.magic (B.blastOutList blastOutMessage,
		 B.blastInList blastInMessage,
		 "msg $Revision$")

    fun readMessages (is : B.instream, acc : message list) : message list =
	if B.endOfStream is then rev acc
	else
	    let val acc = blastInMessages' is @ acc
		val _ = Blaster.resetIn is
	    in	readMessages (is,acc)
	    end
    fun blastInMessages (is : B.instream) : message list =
	(readMessages (is,nil)
	 handle B.BadMagicNumber =>
	     raise Compiler.Reject "master and slave are incompatible")

    val Com = F.String ","

    fun pp_job ((n,s) : job) : F.format =
	F.Hbox [F.String "(", F.String (Int.toString n), Com,
		F.String (String.toString s), F.String ")"]

    val Eq = F.String "="
    fun pp_flag (name : string, value : bool) : F.format =
	F.Hbox [F.String name, Eq, F.String (Bool.toString value)]

    fun pp_message (msg : message) : F.format =
	(case msg
	   of READY => F.String "READY"
	    | ACK_INTERFACE job =>
		F.HOVbox [F.String "ACK_INTERFACE", F.Break,
			  F.String "job = ", pp_job job]
	    | ACK_DONE (job, plan) =>
		F.HOVbox [F.String "ACK_DONE", F.Break,
			  F.String "job = ", pp_job job, Com, F.Break,
			  F.String "plan = ", Update.pp_plan plan]
	    | ACK_ERROR (job, msg) =>
		F.HOVbox [F.String "ACK_DONE", F.Break,
			  F.String "job = ", pp_job job, Com, F.Break,
			  F.String "msg = ", F.String msg]
	    | FLUSH_ALL (platform, flags) =>
		F.HOVbox [F.String "FLUSH_ALL", F.Break,
			  F.String "platform = ",
			  F.String (Target.platformName platform), Com, F.Break,
			  F.String "flags = ", F.pp_list pp_flag flags]
	    | FLUSH (job, plan) =>
		F.HOVbox [F.String "FLUSH", F.Break,
			  F.String "job = ", pp_job job, Com, F.Break,
			  F.String "plan = ", Update.pp_plan plan]
	    | REQUEST (job, plan) =>
		F.HOVbox [F.String "REQUEST", F.Break,
			  F.String "job = ", pp_job job, Com, F.Break,
			  F.String "plan = ", Update.pp_plan plan])

    val pp_messages : message list -> F.format =
	F.pp_list pp_message

    val flagsForSlave : (string * bool ref) list =
	(map (fn (name, ref_cell, _) => (name, ref_cell))
	 Target.importantFlags) @
	[("DebugAsm", Tools.DebugAsm),
	 ("ShowTools", Tools.ShowTools),
	 ("Typecheck", Linknil.typecheck)]

    fun getFlags () : flags =
	let
	    (* Also verifies that names correspond to correct flags.  *)
	    fun getFlag (flag_name, flag_ref) =
		if Stats.bool flag_name = flag_ref then (flag_name, !flag_ref)
		else error ("the flag name " ^ flag_name ^ " doesn't map to the expected flag ref")
	in
	    map getFlag flagsForSlave
	end

    val doFlags : flags -> unit =
	app (fn (name,value) => Stats.bool name := value)

    fun zeroFile (file : string) : unit = BinIO.closeOut (BinIO.openOut file)

    fun fileExists (filename : string) : bool =
	((OS.FileSys.access (filename, nil) andalso
	  OS.FileSys.access (filename, [OS.FileSys.A_READ, OS.FileSys.A_WRITE]))
	 handle _ => false)

    fun fileNonempty (filename : string) : bool option =
	(SOME (OS.FileSys.fileSize filename > 0)
	 handle _ => NONE)

    fun removeFile (file : string) : unit =
	if (OS.FileSys.access (file, []) andalso
	    OS.FileSys.access (file, [OS.FileSys.A_WRITE]))
	then OS.FileSys.remove file handle _ => ()
	else ()

    (*
	A lock file L is a symbolic link from L.  This grabs a lock by
	creating L -> owner; the return value is a function to delete
	the lock.  Because on startup the master wipes the
	communictions directory, this periodically recreates "owner".
    *)
    fun lockFile (owner : string, L : string) : unit -> unit =
	let
	    fun createLink () = ((Posix.FileSys.link {old=owner, new=L}; true)
				 handle _ => false)
	    fun loop try =
		if createLink() then ()
		else
		    let val _ = if try mod 3 = 0 then
				    (zeroFile owner;
				     msg ("Waiting for " ^ L ^ "\n"))
				else ()
			val _ = Platform.sleep 1.0
		    in  loop (try + 1)
		    end
	    val _ = loop 1		(* acquire lock *)
	    fun unlock () = removeFile L
	in  unlock
	end

    fun scanDir (dir : string) : string list =
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
    val master : identity = "master"
    fun slave () : identity =
	let val hostName = Platform.hostname()
	    val machineName = (case Util.substring(".cs.cmu.edu",hostName) of
				   NONE => hostName
				 | SOME pos => String.substring(hostName,0,pos))
	    val pid = Int.toString(Word32.toInt(Platform.pid()))
	in  machineName ^ "." ^ pid
	end
    fun compare (id : identity, id' : identity) : order =
	String.compare (id, id')
    fun name (id : identity) : string = id

    type channel = identity * identity
    fun eq ((src,dest) : channel, (src',dest') : channel) =
	src = src andalso dest = dest
    fun toMaster (slave : identity) : channel = (slave,master)
    fun fromMaster (slave : identity) : channel = (master,slave)
    fun reverse ((src,dest) : channel) : channel = (dest,src)
    fun source ((src, _) : channel) : identity = src
    fun destination ((_, dest) : channel) : identity = dest

    datatype file_type = BUFFER | LOCK | SENDLOCK | RECVLOCK
    datatype name =
	JUNK_NAME of string
      | CHAN_NAME of string * file_type * channel

    local
	val sep = "-to-"
	val sep_length = String.size sep
	fun tyChar (ty : file_type) : char =
	    (case ty
	       of BUFFER => #"+"
		| LOCK => #"#"
		| SENDLOCK => #"!"
		| RECVLOCK => #"@")
	fun charTy (c : char) : file_type option =
	    (case c
	       of #"+" => SOME BUFFER
		| #"#" => SOME LOCK
		| #"!" => SOME SENDLOCK
		| #"@" => SOME RECVLOCK
		| _ => NONE)
    in
	(* does not include directory *)
	fun channelToName' (ty : file_type, c : channel) : string =
	    (#1 c) ^ sep ^ (#2 c) ^ (str (tyChar ty))

	(* directory has been stripped already *)
	fun nameToChannel' (name : string) : name =
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
			     case charTy last
			       of SOME ty => CHAN_NAME (name, ty, chan)
				| NONE => JUNK_NAME name
			 end
		     else JUNK_NAME name)
    end

    fun nameToChannel (name : string) : channel option =
	(case nameToChannel' name
	   of CHAN_NAME (_, BUFFER, chan) => SOME chan
	    | _ => NONE)

    fun channelToName (arg : file_type * channel) : string =
	OS.Path.joinDirFile {dir=Dirs.commDir(), file=channelToName' arg}

    type in_channel = channel * message Q.queue	(* channel and buffered messages from peer *)
    type out_channel = channel * message Q.queue	(* channel and pending output *)

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
	val sendQueues : message Q.queue Map.map ref =
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

    fun hasMsg (channel : channel) : bool =
	let val filename = channelToName (BUFFER, channel)
	in  fileExists filename andalso
	    (case fileNonempty filename
	       of SOME r => r
		| NONE => (msg "Warning: channel disappeared in the middle of hasMsg\n";
			   hasMsg channel))
	end

    (* canReceive : in_channel -> bool *)
    fun canReceive (channel, queue) = Q.length queue > 0 orelse hasMsg channel

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
		if wrap hasMsg channel then
		    let val stream = wrap B.openIn file
			fun wrap' f arg =
			    (f arg handle e => (wrap B.closeIn stream;
						unlock();
						raise e))
			val msgs = wrap' blastInMessages stream
			val _ = wrap B.closeIn stream
			val _ = wrap removeFile file
			val _ = app (fn msg => Q.enqueue (queue, msg)) msgs
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
	    val stream = wrap B.openAppend file
	    fun wrap' f arg = (f arg handle e => (B.closeOut stream;
						  unlock();
						  raise e))
	    fun writeData () =
		(blastOutMessages stream (Q.contents queue);
		 empty_queue queue)

	    val _ = wrap' writeData ()
	    val _ = wrap B.closeOut stream
	    val _ = unlock()
	in  ()
	end

    (* send : message -> out_channel -> unit *)
    fun send message =
	let val autoFlush = (case message
			       of FLUSH _ => false
				| _ => true)
	in
	    fn (arg as (channel, queue)) =>
	    (Q.enqueue (queue, message);
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
	let val commDir = Dirs.commDir()
	    fun removeName name = removeFile (OS.Path.joinDirFile{dir=commDir, file=name})
	    val names = map nameToChannel' (scanDir commDir)
	    (* Clean up junk *)
	    fun removeJunk (JUNK_NAME name) = (removeName name; false)
	      | removeJunk _ = true
	    val names = List.filter removeJunk names
	    (* Clean up files left over from inactive readers and writers *)
	    fun removeOld' (name, chan) = if hasMsg chan then () else removeName name
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
	let val files = scanDir(Dirs.commDir())
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
