(*
	Many of the access() calls exist to speed things up on AFS.
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
    val reject = Util.reject

    fun say (s : string) : unit =
	if !Blaster.BlastDebug then (print s; print "\n") else ()

    (* Q.delete is buggy. *)
    fun empty_queue (q : 'a Q.queue) : unit =
	if Q.isEmpty q then ()
	else (ignore (Q.dequeue q); empty_queue q)

    type label = Name.label

    datatype message =
	READY
      | ACK_INTERFACE of label
      | ACK_FINISHED of label * Stats.measurements
      | ACK_UNFINISHED of label * Stats.measurements
      | ACK_REJECT of label * string
      | BOMB of string
      | INIT of IntSyn.desc	(* Also transmits flags and target. *)
      | COMPILE_INT of label
      | COMPILE of label

    val blastOutJob = NameBlast.blastOutLabel
    val blastInJob = NameBlast.blastInLabel
    val blastOutMeas = Stats.blastOutMeasurements
    val blastInMeas = Stats.blastInMeasurements

    fun blastOutMessage (os : B.outstream) (m : message) : unit =
	(say "blastOutMessage";
	 (case m
	    of READY => B.blastOutInt os 0
	     | ACK_INTERFACE job =>
		(B.blastOutInt os 1; blastOutJob os job)
	     | ACK_FINISHED (job,meas) =>
		(B.blastOutInt os 2; blastOutJob os job;
		 blastOutMeas os meas)
	     | ACK_UNFINISHED (job,meas) =>
		(B.blastOutInt os 3; blastOutJob os job;
		 blastOutMeas os meas)
	     | ACK_REJECT (job,msg) =>
		(B.blastOutInt os 4; blastOutJob os job;
		 B.blastOutString os msg)
	     | BOMB msg =>
		(B.blastOutInt os 5; B.blastOutString os msg)
	     | INIT desc =>
		(B.blastOutInt os 6;
		 Platform.blastOutObjtype os (Target.getTarget());
		 Stats.blastOutFlags os (Stats.get_flags());
		 IntSyn.blastOutDesc os desc)
	     | COMPILE_INT job =>
		(B.blastOutInt os 7; blastOutJob os job)
	     | COMPILE job =>
		(B.blastOutInt os 8; blastOutJob os job)))

    fun blastInMessage (is : B.instream) : message =
	(say "blastInMessage";
	 (case B.blastInInt is
	    of 0 => READY
	     | 1 => ACK_INTERFACE (blastInJob is)
	     | 2 => ACK_FINISHED (blastInJob is, blastInMeas is)
	     | 3 => ACK_UNFINISHED (blastInJob is, blastInMeas is)
	     | 4 => ACK_REJECT (blastInJob is, B.blastInString is)
	     | 5 => BOMB (B.blastInString is)
	     | 6 =>
		let (* Target and flags are hidden affect blastInDesc *)
		    val _ = Target.setTarget (Platform.blastInObjtype is)
		    val _ = Stats.set_flags (Stats.blastInFlags is)
		in  INIT (IntSyn.blastInDesc is)
		end
	     | 7 => COMPILE_INT (blastInJob is)
	     | 8 => COMPILE (blastInJob is)
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
	 handle B.BadMagicNumber _ =>
	     reject "master and slave are incompatible")

    fun pp_job (l : label) : F.format =
	F.String (Name.label2string l)

    val Eq = F.String "="
    fun pp_flag (name : string, value : bool) : F.format =
	F.Hbox [F.String name, Eq, F.String (Bool.toString value)]

    fun pp_message (msg : message) : F.format =
	(case msg
	   of READY => F.String "READY"
	    | ACK_INTERFACE job =>
		F.HOVbox [F.String "ACK_INTERFACE", F.Break,
			  F.String "job = ", pp_job job]
	    | ACK_FINISHED (job,_) =>
		F.HOVbox [F.String "ACK_FINISHED", F.Break,
			  F.String "job = ", pp_job job]
	    | ACK_UNFINISHED (job,_) =>
		F.HOVbox [F.String "ACK_UNFINISHED", F.Break,
			  F.String "job = ", pp_job job]
	    | ACK_REJECT (job, msg) =>
		F.HOVbox [F.String "ACK_REJECT", F.Break,
			  F.String "job = ", pp_job job, F.Break,
			  F.String "msg = ", F.String msg]
	    | BOMB msg =>
		F.HOVbox [F.String "BOMB", F.Break,
			  F.String "msg = ", F.String msg]
	    | INIT desc =>
		F.HOVbox [F.String "INIT", F.Break,
			  F.String "desc = ", IntSyn.pp_desc desc]
	    | COMPILE_INT job =>
		F.HOVbox [F.String "COMPILE_INT", F.Break,
			  F.String "job = ", pp_job job]
	    | COMPILE job =>
		F.HOVbox [F.String "COMPILE", F.Break,
			  F.String "job = ", pp_job job])

    val pp_messages : message list -> F.format =
	F.pp_list pp_message

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
    val slave : unit -> identity = Fs.identity
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
	let val commdir = IntSyn.F.commdir()
	    val file = channelToName' arg
	in  OS.Path.joinDirFile {dir=commdir, file=file}
	end

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
    fun send message (channel,queue) : unit =
	(Q.enqueue (queue, message);
	 emptyBuffer (channel,queue))

    (* closeOut : out_channel -> unit *)
    fun closeOut (arg as (channel, queue)) =
	let val _ = emptyBuffer arg
	    val lockFile = channelToName (SENDLOCK, channel)
	    val _ = removeFile lockFile
	in  ()
	end

    (* destroyAllChannels : unit -> unit *)
    fun destroyAllChannels () =
	let val commDir = IntSyn.F.commdir()
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
	let val commDir = IntSyn.F.commdir()
	    val files = scanDir commDir
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
