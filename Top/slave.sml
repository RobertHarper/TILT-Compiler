structure Slave :> SLAVE =
struct
    val error = fn s => Util.error "slave.sml" s
    val Standalone = ref true
    val SlaveDiag = Stats.ff "SlaveDiag"
    fun msg str = if (!SlaveDiag) then print str else ()
    val PrintEachFile = Stats.ff "PrintEachFile"

    type comm =
	{from:Comm.channel,
	 to:Comm.channel,
	 in_channel:Comm.in_channel,
	 out_channel:Comm.out_channel}

    fun start_comm () : comm =
	let val identity = Comm.slave ()
	    val from = Comm.fromMaster identity
	    val to = Comm.toMaster identity
	    val in_channel = Comm.openIn from
	    val out_channel = Comm.openOut to
	in  {from=from, to=to, in_channel=in_channel,
	     out_channel=out_channel}
	end

    fun send ({out_channel, ...} : comm, msg : Comm.message) : unit =
	Comm.send msg out_channel

    fun receive ({in_channel, ...} : comm) : Comm.message option =
	Comm.receive in_channel

    fun wait (comm:comm) : unit -> unit =
	let val {from,to,...} = comm
	    val timer : Time.time option ref = ref NONE
	    val delay = Time.fromSeconds 10
	    fun startTimer () : unit = timer := SOME (Time.+(Time.now(),delay))
	    fun stopTimer () : unit = timer := NONE
	    fun timeout () : bool =
		(case !timer
		   of NONE => false
		    | SOME t' => Time.>(Time.now(),t'))
	    fun loop () =
		if Comm.hasMsg from then ()
		else
		    if Comm.hasMsg to then
			let val _ =
				if timeout() then
				    let val _ =
					    if !Standalone then
						msg "[waiting for work]\n"
					    else ()
				    in  stopTimer()
				    end
				else ()
			    val delay = if !Standalone then 1.0 else 0.1
			    val _ = Platform.sleep delay
			in  loop()
			end
		    else ()
	in  fn () => (startTimer(); loop())
	end

    (*
	It's okay for the first acknowledgement to be missed.  In
	fact, we skip the acknowledgement if the expected compilation
	time is small to avoid communication traffic.
    *)
    fun compile (comm:comm, desc:IntSyn.desc,
		 job:Name.label) : unit =
	let val _ = Stats.clear_measurements()
	    val start = Time.now()
	    fun ack_interface () : unit =
		let val diff = Time.-(Time.now(), start)
		    val slow = Time.toReal diff > 0.5
		in  if slow then send(comm, Comm.ACK_INTERFACE job)
		    else ()
		end
	    val (desc,pdec) = Compiler.get_inputs (desc, job)
	    val finished = Compiler.compile (desc,pdec,ack_interface)
	    val meas = Stats.get_measurements()
	    val msg =
		if finished then Comm.ACK_FINISHED (job,meas)
		else Comm.ACK_UNFINISHED (job,meas)
	    val _ = send (comm,msg)
	    val _ = if !PrintEachFile then Stats.print_measurements() else ()
	in  ()
	end handle (e as UtilError.Reject {msg}) =>
	    let val _ = if !Standalone then ExnHandler.print e else ()
		val _ = send (comm, Comm.ACK_REJECT (job,msg))
	    in  ()
	    end

    type state = IntSyn.desc option

    fun process (comm:comm, state:state, msg:Comm.message) : state =
	((case msg
	   of Comm.INIT (objtype, flags, desc) =>
		(Fs.flush();
		 Target.setTarget objtype;
		 Stats.set_flags flags;
		 SOME desc)
	    | Comm.COMPILE job =>
		(case state
		   of NONE => error "slave got COMPILE before INIT"
		    | SOME desc => (compile(comm,desc,job); state))
	    | _ => error "slave got unexpected message")
	 handle e =>
	    let val _ = if !Standalone then ExnHandler.print e else ()
		val _ = send (comm, Comm.BOMB (ExnHandler.errorMsg e))
		val _ = if !ExnHandler.Interactive then raise e else ()
	    in  state
	    end)

    fun step (comm:comm, state:state) : state =
	(case receive comm
	   of NONE =>
		let val _ =
			if Comm.hasMsg (#to comm) then ()
			else send (comm, Comm.READY)
		in  state
		end
	   | SOME msg => process(comm,state,msg))

    fun slave () : 'a =
	let val comm : comm = start_comm()
	    val wait : unit -> unit = wait comm
	    fun loop state =
		let val state = step (comm,state)
		    val _ = wait()
		in  loop state
		end
	in  loop NONE
	end

end
